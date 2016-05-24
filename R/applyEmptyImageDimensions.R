#' @title Apply Subsetting from Empty Image Dimensions
#' @name applyEmptyImageDimensions
#' @param img nifti object
#' @param inds indices of subset from \code{\link{getEmptyImageDimensions}} or
#' \code{\link{dropEmptyImageDimensions}}.
#' @param reorient Should image be reoriented if a filename?
#' @description Simple wrapper for subsetting an image with indices, 
#' dropping empty dimensions.
#' @return Object of class \code{nifti}
#' @note \code{apply_empty_dim} is a shorthand for 
#' \code{applyEmptyImageDimensions} with all the same arguments.
#' @seealso \code{\link{getEmptyImageDimensions}}, 
#' \code{\link{dropEmptyImageDimensions}} 
#' @export
applyEmptyImageDimensions <- function(img, 
                                      inds,
                                      reorient = FALSE) {
  
  
  img = check_nifti(img, reorient = reorient)
  if (dim_(img)[1] > 3) {
    stop(paste0("Only images with 3 dimensions supported, ", 
                "as checked by dim_"))
  }
  i2 = img[inds[[1]], inds[[2]], inds[[3]]]
  outimg = copyNIfTIHeader(img = img, arr = i2, drop = TRUE)  
  return(outimg)
}

#' @rdname applyEmptyImageDimensions
#' @export
apply_empty_dim <- function(img, 
                            inds,
                            reorient = FALSE) {
  applyEmptyImageDimensions(img = img, 
                           inds = inds,
                           reorient = reorient)
}
