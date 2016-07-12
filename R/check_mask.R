#' @title Check Mask is Binary
#' @description Determine if only values in a mask are 0/1
#' @param mask Object of class \code{nifti}
#' @param allow.NA allow NAs in the mask
#' @return Logical indicating if object is binary mask with only 0, 1, and \code{NA}
#' if applicable
#' @export
#' @examples 
#' arr = array(rbinom(1000, size = 1, prob = 0.2), dim = c(10,10,10))
#' nim = oro.nifti::nifti(arr)
#' check_mask(nim)
check_mask <- function(mask, allow.NA = FALSE){
  mask = check_nifti(mask)
  allowable = c(0, 1)
  if (allow.NA) {
    allowable = c(allowable, NA)
  }
  mask = as(mask, "array")
  class(mask) = "numeric"
  umask = unique(c(mask))
  res = all(umask %in% allowable)
  return(res)
}

#' @title Check Mask is Binary, Fail otherwise
#' @description Determine if only values in a mask are 0/1.  Will error otherwise.
#' @param ... arguments to pass to \code{\link{check_mask}}
#' @return Either will error if conditions not met or an invisible \code{NULL}
#' @export
#' @examples 
#' arr = array(rbinom(1000, size = 1, prob = 0.2), dim = c(10,10,10))
#' nim = oro.nifti::nifti(arr)
#' check_mask_fail(nim)
check_mask_fail <- function(...){
  res = check_mask(...)
  if (!res) {
    stop("Mask must be binary 0/1.  If it has NAs, allow.NA must be TRUE")
  } else {
    return(invisible(NULL))
  }
}

