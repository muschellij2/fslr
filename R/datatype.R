#' @docType methods
#' @rdname datatype-methods
#' @title Extract Image datatype attribute 
#' @name datatype-methods
#' @aliases datatype,character-method
#' @import oro.nifti
#' @param object is a filename to pass to \link{fslval}
#' @export
#' 
setMethod("datatype", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "datatype", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
