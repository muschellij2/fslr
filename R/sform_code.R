#' @docType methods
#' @rdname sform_code-methods
#' @title Extract Image sform_code attribute 
#' @name sform_code-methods
#' @aliases sform_code,character-method
#' @import oro.nifti
#' @param object is a filename to pass to \link{fslval}
#' @export
#' 
setMethod("sform_code", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "sform_code", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
