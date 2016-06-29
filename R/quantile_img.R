#' @title Create Quantile Image
#' @description Creates output image of the quantiles that each voxel is in, 
#' after applying the mask
#' @param img Character vector, or object of class \code{nifti}
#' @param mask Mask to determine cumulative distribution function (cdf) from
#' @param ... Addictional arguments to pass to \code{\link{ecdf}}
#' @return Object of class \code{nifti}
#' @export
quantile_img = function(img, 
                        mask = NULL,
                        ...)
{
  img = check_nifti(img)
  
  if (!is.null(mask)) {
    mask = check_nifti(mask, allow.array = TRUE)
  } else { 
    mask = niftiarr(img, 1)  
  }
  
  allowable = c(0, 1)
  mask = as(mask, "array")
  class(mask) = "numeric"
  umask = unique(c(mask))
  if (!all(umask %in% allowable)) {
    stop("Mask must be binary 0/1.")
  }
  
  vals = img[mask == 1]
  e = ecdf(vals)
  qimg = niftiarr(img, e(c(img)))
  
  return(qimg)
}