#' @docType methods
#' @rdname intent_code-methods
#' @title Extract Image intent_code attribute 
#' @name intent_code-methods
#' @aliases intent_code,character-method
#' @import oro.nifti
#' @param object is a filename to pass to \link{fslval}
#' @export
#' 
setMethod("intent_code", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "intent_code", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
