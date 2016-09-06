#' @title Image Rescaler
#' @name rescale_img
#' @param filename filename of image to be read into R or nifti object 
#' @param pngname filename of png of histogram of values of image to be made. For no
#' png - set to NULL (default)
#' @param write.nii logical - should the image be written.  
#' @param outfile if \code{write.nii = TRUE}, filename of output file
#' @param min.val minimum value of image (default -1024 (for CT)).  If no thresholding
#' set to -Inf
#' @param max.val maximum value of image (default 3071 (for CT)).  If no thresholding
#' set to Inf
#' @param ROIformat if TRUE, any values $< 0$ will be set to 0
#' @param writer character value to add to description slot of NIfTI header
#' @param ... extra methods to be passed to \code{\link{writenii}}
#' @description Rescales an image to be in certain value range.  This was created
#' as sometimes DICOM scale and slope parameters may be inconsistent across sites
#' and the data need to be value restricted
#' @return Object of class nifti
#' @importFrom grDevices png dev.off
#' @export
rescale_img = function(filename, 
                       pngname = NULL, 
                       write.nii = FALSE,
                       outfile = NULL,
                       min.val = -1024,
                       max.val = 3071,
                       ROIformat=FALSE, 
                       writer = "dcm2nii", ...){
  
  if (write.nii){
    stopifnot(!is.null(outfile))
  }
  
  img = check_nifti(filename)
  # inter = as.numeric(img@scl_inter)
  # slope = as.numeric(img@scl_slope)
  # img = (img * slope + inter)
  r = range(c(img))
  if (r[1] >= min.val & r[2] <= max.val){
    return(img)
  }
  img[img < min.val] = min.val
  img[img > max.val] = max.val
  img = zero_trans(img)
  if (ROIformat) img[img < 0] = 0
  img = cal_img(img)
  descrip(img) = paste0("written by ", writer, " - ", descrip(img))
  
  img = drop_img_dim(img)
  #### create histograms
  if (!is.null(pngname)){
    options(bitmapType = 'cairo') 
    print(pngname)
    ### remove random percents
    pngname = gsub("%", "", pngname)
    grDevices::png(pngname)
      graphics::hist(img)
    grDevices::dev.off()
  }
  
  if (write.nii) {
    writenii(img, filename = outfile, ...)
  }
  return(img)
}




#' @title Change Data type for img
#' @return object of type nifti
#' @param img nifti object (or character of filename)
#' @param type_string (NULL) character of datatype and bitpix.  Supercedes
#' both datatype and bitpix.  If specified 
#' \code{convert.datatype[[type_string]]} and 
#' \code{convert.bitpix[[type_string]]} will be used.
#' @param datatype (NULL) character of datatype see 
#' \code{\link{convert.datatype}}
#' @param bitpix (NULL) character of bitpix see 
#' \code{\link{convert.bitpix}} 
#' @param trybyte (logical) Should you try to make a byte (UINT8) if image in
#' c(0, 1)?
#' @param warn Should a warning be issued if defaulting to FLOAT32?
#' @description Tries to figure out the correct datatype for image.  Useful 
#' for image masks - makes them binary if
#' @name datatype
#' @export
datatyper = function(img, type_string = NULL,
                     datatype = NULL, bitpix=NULL, trybyte=TRUE,
                     warn = TRUE){
  img = check_nifti(img)
  if (!is.null(type_string)) {
    accepted = names(convert.datatype())
    type_string = toupper(type_string)
    stopifnot(type_string %in% accepted)
    datatype = convert.datatype()[[type_string]]
    bitpix = convert.bitpix()[[type_string]]
  }  
  if (!is.null(datatype) & !is.null(bitpix)) {
    datatype(img) <- datatype
    bitpix(img) <- bitpix
    return(img)
  }
  if (!is.null(datatype) & is.null(bitpix)) {
    stop("Both bitipx and datatype need to be specified if oneis")
  }
  if (is.null(datatype) & !is.null(bitpix)) {
    stop("Both bitipx and datatype need to be specified if oneis")
  }
  #### logical - sign to unsigned int 8
  arr = as(img, "array")
  is.log = inherits(arr[1], "logical")
  if (is.log) {
    datatype(img) <- convert.datatype()$UINT8
    bitpix(img) <- convert.bitpix()$UINT8
    return(img)
  }
  #### testing for integers
  testInteger <- function(img){
    x = c(as(img, "array"))
    test <- all.equal(x, as.integer(x), check.attributes = FALSE)
    return(isTRUE(test))
  }  
  is.int = testInteger(img)
  if (is.int) {
    rr = range(img, na.rm = TRUE)
    ##### does this just for binary mask
    if (all(rr == c(0, 1)) & trybyte) {
      if (all(img %in% c(0, 1))) {
        datatype(img) <- convert.datatype()$UINT8
        bitpix(img) <- convert.bitpix()$UINT8
        return(img)
      }
    }
    signed = FALSE
    if (any(rr < 0)) {
      signed = TRUE
    }
    trange = diff(rr)
    # u = "U"
    mystr = NULL
    num = 16 # default is signed short
    if (is.null(mystr) & trange <= (2 ^ num) - 1 ) {
        mystr = ifelse(signed, "INT16", "FLOAT32")
    }

    num = 32 
    if (is.null(mystr) & trange <= (2 ^ num) - 1 ) {
      mystr = "INT32" # no UINT32 allowed
    }
    
    num = 64
    if (is.null(mystr) & trange <= (2 ^ num) - 1 ) {
      mystr = "DOUBLE64" # Only way to 64 bits is through double
    }
    if (is.null(mystr)) {
      stop(paste0("Cannot determine integer datatype, ", 
                  "may want to recheck data or not use datatyper!"))
    }
    datatype(img) <- convert.datatype()[[mystr]]
    bitpix(img) <- convert.bitpix()[[mystr]]
    return(img)
  } else {
    if (warn) {
      warning("Assuming FLOAT32")
    }
    mystr = "FLOAT32"
    datatype(img) <- convert.datatype()[[mystr]]
    bitpix(img) <- convert.bitpix()[[mystr]]
    return(img)
  }
}



#' @title Resets image parameters for a copied nifti object
#' @return object of type nifti
#' @param img nifti object (or character of filename)
#' @param ... arguments to be passed to \code{\link{datatype}}
#' @description Resets the slots of a nifti object, usually because an image
#' was loaded, then copied and filled in with new data instead of making a 
#' nifti object from scratch.  Just a wrapper for smaller functions 
#' @export
newnii = function(img, ...){
  img = check_nifti(img)
  img = zero_trans(img)
  img = cal_img(img)
  img = datatyper(img, ...)
  return(img)
}


#' @title Get Z-score over a margin of an img
#' @import oro.nifti
#' @description Standardizes an image either by the axial, sagittal, or
#' coronal slice
#' @importFrom matrixStats colSds
#' @return Array of object of class nifti
#' @seealso \link{aperm}
#' @param img character path of image or 
#' an object of class nifti
#' @param mask character path of mask or 
#' an object of class nifti 
#' @param margin Margin of image to z-score over (3-Axial, 2-Sagittal, 
#' 1-Coronal)
#' @param centrality (character) Measure to center the data, 
#' either mean or median
#' @param variability (character) Measure to scale the data
#' @param remove.na (logical) change NAs to remove.val
#' @param remove.nan (logical) change NaN to remove.val
#' @param remove.inf (logical) change Inf to remove.val
#' @param remove.val (logical) value to put the NA/NaN/Inf
#' @export
#' @importFrom matrixStats colMedians 
#' @importFrom matrixStats colSds 
#' @importFrom matrixStats colIQRDiffs colIQRs iqrDiff iqr
#' @importFrom matrixStats colMadDiffs colMads madDiff
#' @import stats
#' @examples
#' dim = c(100, 30, 5)
#' img = array(rnorm(prod(dim), mean=4, sd=4), 
#' dim=dim)
#' 
#' truth2 = img
#' for (i in 1:dim(img)[2]) {
#' truth2[,i,] = (truth2[,i,]- mean(truth2[,i,]))/sd(truth2[,i,])
#' }
#' 
#' truth1 = img
#' for (i in 1:dim(img)[1]) {
#' truth1[i,,] = (truth1[i,,]- mean(truth1[i,,]))/sd(truth1[i,,])
#' }
#' 
#' truth3 = img
#' for (i in 1:dim(img)[3]) {
#' truth3[,,i] = (truth3[,,i]- mean(truth3[,,i]))/sd(truth3[,,i])
#' }
#' try3 = zscore_img(img, margin=3)
#' stopifnot(all.equal(try3, truth3))
#' try2 = zscore_img(img, margin=2)
#' stopifnot(all.equal(try2, truth2))
#' try1 = zscore_img(img, margin=1)
#' stopifnot(all.equal(try1, truth1))
#'   
#' 
zscore_img <- function(img, mask = NULL, margin=3, 
                       centrality = c("mean", "median"),
                       variability = c("sd", "iqrdiff", "mad", 
                                       "maddiff", "iqr"),
                       remove.na = TRUE,
                       remove.nan = TRUE, remove.inf = TRUE,
                       remove.val = 0){
  centrality = match.arg(centrality, c("mean", "median"))
  variability = match.arg(variability, c("sd", "iqrdiff", "mad", 
                                         "maddiff", "iqr"))
  img = check_nifti(img, allow.array=TRUE)
  orig.img = img
  dimg = dim(orig.img)
  if (is.null(mask)){
    mask = array(1, dim = dimg)  
  }
  mask = check_nifti(mask, allow.array=TRUE)
  img[mask == 0] = NA
  
  stopifnot(length(dimg) == 3)  
  if (!is.null(margin)){
    if (margin == 3){
      perm = 1:3
    }
    if (margin == 2){
      perm = c(1, 3, 2)
    }  
    if (margin == 1){
      perm = c(2, 3, 1)
    }
    revperm = match(1:3, perm)
    img = aperm(img, perm)
    
    vec = matrix(img, ncol=dimg[margin])
    if (centrality == "mean") {
      m = colMeans(vec, na.rm = TRUE)
    }
    if (centrality == "median") {
      m = colMedians(vec, na.rm = TRUE)
    } 
    if (variability == "iqrdiff") {
      s = colIQRDiffs(vec, na.rm = TRUE)
    }
    if (variability == "maddiff") {
      s = colMadDiffs(vec, na.rm = TRUE)
    }   
    if (variability == "mad") {
      s = colMads(vec, na.rm = TRUE)
    }       
    if (variability == "iqr") {
      s = colIQRs(vec, na.rm = TRUE)
    }       
    if (variability == "sd") {
      s = colSds(vec, na.rm = TRUE)
    }     
    
    vecc = (t(vec) - m)/s
    vecc = t(vecc)
    imgc = array(vecc, 
                 dim = dim(img))
    imgc = aperm(imgc, revperm)
  } else {
    mn = do.call(centrality, list(x = c(img), na.rm = TRUE))
    if (variability == "iqrdiff") {
      s = iqrDiff(c(img), na.rm = TRUE)
    }
    if (variability == "sd") {
      s = sd(c(img), na.rm = TRUE)
    }
    if (variability == "maddiff") {
      s = madDiff(c(img), na.rm = TRUE)
    }   
    if (variability == "mad") {
      s = mad(c(img), na.rm = TRUE)
    }       
    if (variability == "iqr") {
      s = iqr(c(img), na.rm = TRUE)
    }       
    
    imgc = (img - mn) / s
  }
  stopifnot(all.equal(dim(imgc), dim(orig.img)))
  if (inherits(orig.img, "nifti")) {
    imgc = niftiarr(orig.img, imgc)
    imgc = datatyper(imgc, 
                     datatype = convert.datatype()$FLOAT32, 
                     bitpix = convert.bitpix()$FLOAT32) 
  }
  if (remove.na) {
    imgc[is.na(imgc)] = remove.val
  }
  if (remove.nan) {
    imgc[is.nan(imgc)] = remove.val
  } 
  if (remove.inf) {
    imgc[is.infinite(imgc)] = remove.val
  }   
  if (inherits(orig.img, "nifti")) {
    imgc = cal_img(imgc)
    imgc = zero_trans(imgc)
  }
  imgc
  
}
