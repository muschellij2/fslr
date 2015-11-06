#' @docType methods
#' @rdname intent_name-methods
#' @title Extract Image intent_name attribute 
#' @name intent_name-methods
#' @aliases intent_name,character-method
#' @import oro.nifti
#' @export
#' @description intent_name method for character types
#' @param object is a filename to pass to \link{fslval}
#' 
setMethod("intent_name", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "intent_name", verbose = FALSE)
  res = (res)
  return(res)
})
