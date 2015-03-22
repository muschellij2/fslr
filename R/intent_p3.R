#' @docType methods
#' @rdname intent_p3-methods
#' @title Extract Image intent_p3 attribute 
#' @name intent_p3-methods
#' @aliases intent_p3,character-method
#' @import oro.nifti
#' @param object is a filename to pass to \link{fslval}
#' @export
#' 
setMethod("intent_p3", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "intent_p3", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
