#' @title Drop Empty Image Dimensions
#' @name dropEmptyImageDimensions
#' @param img nifti object
#' @param value Value to check against.  If zero, then 
#' \code{dropEmptyImageDimensions} will drop any dimension that has 
#' fewer than \code{threshold} zeroes.  May be a vector of values, matched with 
#' \code{\link{match}}
#' @param threshold Drop dimension if fewer than \code{threshold} voxels
#' are in the slice
#' @param other.imgs List of other nifti objects or filenames 
#' to apply the same dropping as \code{img}.
#' @param keep_ind keep indices in output.  Will return list, even if 
#' \code{other.imgs} not specified
#' @param reorient Should image be reoriented if a filename?
#' @description Drops dimensions of an image that has all irrelevant
#' values
#' @return List of output image indices, and other images
#' if \code{other.imgs} not specified or \code{keep_ind = TRUE}. 
#' Otherwise object of class \code{nifti}
#' @note \code{drop_empty_dim} is a shorthand for \code{dropEmptyImageDimensions}
#' with all the same arguments.  Also, \code{NA} are set to zero.
#' @seealso \code{\link{getEmptyImageDimensions}}  
#' @export
dropEmptyImageDimensions <- function(img, 
                                     value = 0, 
                                     threshold = 0,
                                     other.imgs = NULL, 
                                     keep_ind = FALSE,
                                     reorient = FALSE) {
  
  img = check_nifti(img, reorient = reorient)
  if (dim_(img)[1] > 3){
    stop(paste0("Only images with 3 dimensions supported, ", 
                "as checked by dim_"))
  }
  inds = getEmptyImageDimensions(img = img,
                                 value = value, 
                                 threshold = threshold,
                                 reorient = reorient)
#   ############################
#   # Set NAs to 0
#   ############################
#   arr = as.array(img)
#   arr[is.na(arr)] = 0
#   img = niftiarr(img, arr)
#   
#   
#   ############################
#   # Get indices for slices with all zeros (or of certain value)
#   ############################
#   inds = vector(mode = "list", length = 3)
#   for (i in 1:3) {
#     zero_x = apply(img, i, function(x) sum(x != value))
#     dzero_x = !(
#       cumsum(zero_x) <= threshold | 
#         rev( cumsum(rev(zero_x)) <= threshold )
#     )
#     inds[[i]] = which(dzero_x)
#     #     print(i)
#   }
  
  ############################
  # Get matrix of indices
  ############################
  all.inds = as.matrix(expand.grid(inds))
  
  ############################
  # Subset the image
  ############################
  i2 = img[inds[[1]], inds[[2]], inds[[3]]]
  
  outimg = copyNIfTIHeader(img = img, arr = i2, drop = TRUE)
  
  if (!is.null(other.imgs)) {
    if (is.nifti(other.imgs)) {
      other.imgs = list(other.imgs)
    }
    stopifnot(is.list(other.imgs))
    other.imgs = lapply(other.imgs, 
                        check_nifti, reorient = reorient)
    other.imgs = lapply(other.imgs, function(oimg){
      i2 = oimg[inds[[1]], inds[[2]], inds[[3]]]
      newimg = copyNIfTIHeader(img = img, arr = i2, drop = TRUE)
      return(newimg)
    })
    if (length(other.imgs) == 1){
      other.imgs = other.imgs[[1]]
    }
    return(list(outimg = outimg, 
                other.imgs = other.imgs, 
                inds = inds, orig.dim = dim(img)))
  }
  if (keep_ind){
    outimg = list(outimg = outimg, inds = inds,
                  orig.dim = dim(img))
  }
  return(outimg)
}

#' @rdname dropEmptyImageDimensions
#' @export
drop_empty_dim <- function(img, 
                           value = 0, 
                           threshold = 0,
                           other.imgs = NULL, 
                           keep_ind = FALSE,
                           reorient = FALSE) {
  dropEmptyImageDimensions(img = img, 
                           value = value, 
                           threshold = threshold,
                           other.imgs = other.imgs, 
                           keep_ind = keep_ind,
                           reorient = reorient)
}
