#' @docType methods
#' @rdname scl_slope-methods
#' @title Extract Image scl_slope attribute 
#' @name scl_slope-methods
#' @aliases scl_slope,character-method
#' @import oro.nifti
#' @export
#' @description scl_slope method for character types
#' @param object is a filename to pass to \link{fslval}
#' 
setMethod("scl_slope", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "scl_slope", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
