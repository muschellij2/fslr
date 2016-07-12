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
  
  check_mask_fail(mask, allow.NA = FALSE)

  vals = img[mask == 1]
  e = ecdf(vals)
  qimg = niftiarr(img, e(c(img)))
  
  return(qimg)
}