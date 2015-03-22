#' @docType methods
#' @rdname cal_max-methods
#' @title Extract Image cal.max attribute 
#' @name cal.max-methods
#' @aliases cal.max,character-method
#' @import oro.nifti
#' @param object is a filename to pass to \link{fslval}
#' @export
#' 
setMethod("cal.max", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "cal_max", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
