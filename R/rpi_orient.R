
.orient_file = function(file, verbose = TRUE){
  file = checkimg(file)
  forms = getForms(file, verbose = verbose)
  if (forms$sform_code == 0 & forms$qform_code == 0) {
    stop("Cannot swap dimensions - sform_code and qform_code are 0!")
  }
  if (forms$sform_code != 0 & forms$qform_code != 0) {
    if (!all(forms$ssor == forms$sqor)) {
      warning(paste0("sform and qform are set, but the orientation ", 
                     "is not the same, using sform"))
    }
  }  
  if (forms$sform_code != 0) {
    sorient = forms$ssor
  } else {
    sorient = forms$sqor
  }
  L = list(file = file, orientation = sorient)
  return(L)
}

#' @title Reorient an Image to RPI orientation
#' @description This function uses \code{fslswapdim} to reorient an image
#' @param file Object of class \code{nifti} or character path
#' @param verbose print diagnostic messages
#' @return List of 3 elements
#' \itemize{
#' \item{\code{img}: }{Reoriented image of class \code{nifti}}
#' \item{\code{convention}: }{Convention (Neurological/Radiological) of original image}
#' \item{\code{orientation}: }{Original image orientations}
#' }
#' @export
#' @note `orient_rpi` and `orient_rpi_file` uses `RNifti` to ensure the 
#' reading orientation
#' @examples 
#' lr_fname = system.file( "nifti", "mniLR.nii.gz", package = "oro.nifti")
#' img = readnii(lr_fname)
#' 
#' rl_fname = system.file( "nifti", "mniRL.nii.gz", package = "oro.nifti")
#' rl_img = readnii(rl_fname)
#' stopifnot(all(rl_img[nrow(rl_img):1,,] == img))
#' 
#' \dontrun{
#' if (have_fsl()) {
#' 
#' 
#' reor = rpi_orient(rl_fname)
#' rev = reverse_rpi_orient(reor$img, convention = reor$convention,
#' orientation = reor$orientation)
#' stopifnot(all(rev == rl_img))
#' }
#' }
#' 
#' 
#' reor = orient_rpi(rl_fname)
#' stopifnot(all(img == reor$img))
#' 
#' rev = reverse_orient_rpi(reor$img, convention = reor$convention,
#' orientation = reor$orientation)
#' stopifnot(all(rev == rl_img))
rpi_orient = function(file, verbose = TRUE){
  L = rpi_orient_file(file = file, verbose = verbose)
  L$img = check_nifti(L$img)
  return(L)
}

#' @export
#' @rdname rpi_orient
rpi_orient_file = function(file, verbose = TRUE){
  .Deprecated(
    paste0("rpi_orient_file is going to be deprecated in the coming", 
           " releases of fslr, and things this relies on, ", 
           " including readrpi and rpi_orient.  Please use ",
           "neurobase functions orient_rpi_file, orient_rpi, ", 
           "and read_rpi in the future.")
  )
  file = checkimg(file)
  L = .orient_file(file = file, verbose = verbose)
  file = L$file
  sorient = L$orientation
  ori = fslgetorient(file, verbose = verbose)
  if (ori == "NEUROLOGICAL") {
    # need to copy because fslorient samefile stuff
    tdir = tempfile()
    dir.create(tdir, showWarnings = verbose)
    tfile = file.path(tdir,
                      basename(file))
    file.copy(file, tfile, overwrite = TRUE)
    
    # orient3 = substr(sorient, 1, 1)
    # if (any(orient3 == "L")) {
    #   ind = which(orient3 == "L")
    #   arglist = c(a = "x", b = "y", c = "z")
    #   arglist[ind] = paste0("-", arglist[ind])
    #   arglist = as.list(arglist)
    #   arglist$file = file
    #   arglist$retimg = FALSE
    #   arglist$outfile = tfile
    #   arglist$verbose = verbose
    #   do.call(fslswapdim, args = arglist)
    # }
    
    # changes from NEUROLOGICAL to RADIOLOGICAL
    fslorient(tfile,
              opts = "-swaporient",
              retimg = FALSE,
              outfile = tfile,
              verbose = verbose)
    ori2 = .orient_file(tfile, verbose = verbose)
    if (all(ori2$orientation == c("RL", "PA", "IS"))) {
      warning(
        paste0(
          "NEUROLOGICAL cases with RPI after swaporient",
          " may not reorder the rows or oro.nifti.  Please use ",
          "RNifti::orientation")
      )
    }
    
    file = tfile
  }
  outfile = tempfile(fileext = ".nii.gz")
  # Changes the data
  fslswapdim(file = file,
             retimg = FALSE,
             outfile = outfile,
             a = "RL", b = "PA", c = "IS",
             verbose = verbose)
  L = list(img = outfile,
           convention = ori,
           orientation = sorient)
  return(L)
}


#' @title Reverse Reorientation an Image to RPI orientation
#' @description This function uses \code{fslswapdim} to reorient an image
#' @param file Object of class \code{nifti} or character path
#' @param convention Convention of original image (usually from \code{\link{rpi_orient}})
#' @param orientation Vector of length 3 from original image 
#' (usually from \code{\link{rpi_orient}})
#' @param verbose print diagnostic messages
#' @return Object of class \code{nifti}
#' @export
reverse_rpi_orient = function(file, 
                              convention = c("NEUROLOGICAL", "RADIOLOGICAL"), 
                              orientation, verbose = TRUE){
  img = reverse_rpi_orient_file(file = file, convention = convention,
                                orientation = orientation, verbose = verbose)
  img = check_nifti(img)
  return(img)
}

#' @rdname reverse_rpi_orient
#' @export
reverse_rpi_orient_file = function(
  file, 
  convention = c("NEUROLOGICAL", "RADIOLOGICAL"), 
  orientation, verbose = TRUE){
  
  file = checkimg(file)
  stopifnot(length(orientation) == 3)
  convention = match.arg(convention)
  
  outfile = tempfile(fileext = ".nii.gz")
  if (convention == "NEUROLOGICAL") {   
    file.copy(file, outfile)
    fslorient(outfile, 
              opts = "-swaporient",
              retimg = FALSE, 
              verbose = verbose)   
    file = outfile
    outfile = tempfile(fileext = ".nii.gz")    
  }  
  fslswapdim(file = file, 
             a = orientation[1], 
             b = orientation[2], 
             c = orientation[3], 
             verbose = verbose,
             retimg = FALSE,
             outfile = outfile)
  
  return(outfile)
}


#' @export
#' @rdname rpi_orient
is_rpi = function(file, verbose = FALSE) {
  file = checkimg(file)
  L = .orient_file(file = file, verbose = verbose)
  file = L$file
  sorient = L$orientation
  orient = paste0(substr(sorient, 1, 1), collapse = "")
  ori = fslgetorient(file, verbose = verbose)
  res = ori == "RADIOLOGICAL" && orient == "RPI"
  return(res)
}

#' @export
#' @rdname rpi_orient
is.rpi = is_rpi