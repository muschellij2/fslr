#' @title Mask nifti Image
#'
#' @description Takes an image, masks it by 
#' \code{mask}, and returns an object of class \code{nifti}
#' @param img object of class \code{nifti}
#' @param mask array or object of class \code{nifti}, same dimensions as img
#' @param allow.NA allow NAs in the mask
#' @export
#' @return Object of class nifti with values outside mask set to 0 if mask is
#' 0 and NA if mask is NA and img if mask == 1
mask_img <- function(img, # object of class \code{nifti}
                    mask, # array or object of class \code{nifti}
                    allow.NA = TRUE # allow NAs in the mask
){
  stopifnot(inherits(img, "nifti"))
  allowable = c(0, 1, NA)
  if (allow.NA) allowable = c(allowable, NA)
  mask = as(mask, "array")
  class(mask) = "numeric"
  umask = unique(mask)
  stopifnot(umask %in% allowable)
  niftiarr(img, img * mask)
}