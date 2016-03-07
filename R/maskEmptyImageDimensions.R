#' @title Make Mask from Empty Image Dimensions
#' @name maskEmptyImageDimensions
#' @param img nifti object
#' @param ... Arguments to be passed to \code{\link{getEmptyImageDimensions}}.
#' @param reorient Should image be reoriented if a filename?
#' @description Make a mask of an image that has all irrelevant
#' values
#' @return Object of class \code{nifti}
#' @note \code{mask_empty_dim} is a shorthand for \code{maskEmptyImageDimensions}
#' with all the same arguments.
#' @seealso \code{\link{getEmptyImageDimensions}}  
#' @export
maskEmptyImageDimensions <- function(img, 
                                     ...,
                                     reorient = FALSE) {
  
  img = check_nifti(img, reorient = reorient)
  if (dim_(img)[1] > 3) {
    stop(paste0("Only images with 3 dimensions supported, ", 
                "as checked by dim_"))
  }
  inds = getEmptyImageDimensions(img = img,
                                 reorient = reorient,
                                 ...)
  
  mask = niftiarr(img, 0)
  mask[inds[[1]], inds[[2]], inds[[3]]] = 1
  mask@datatype <- convert.datatype()[["UINT8"]]
  mask@bitpix <- convert.bitpix()[["UINT8"]]  
  mask = cal_img(mask)
  
  return(mask)
}

#' @rdname maskEmptyImageDimensions
#' @export
mask_empty_dim <- function(img, 
                           ...,
                           reorient = FALSE) {
  maskEmptyImageDimensions(img = img, 
                           ... = ...,
                           reorient = reorient)
}
