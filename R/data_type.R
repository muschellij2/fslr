#' @docType methods
#' @rdname data_type-methods
#' @title Extract Image data_type attribute 
#' @name data_type-methods
#' @aliases data_type,character-method
#' @import oro.nifti
#' @param object is a filename to pass to \link{fslval}
#' @export
#' 
setMethod("data_type", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "data_type", verbose = FALSE)
  res = (res)
  return(res)
})
