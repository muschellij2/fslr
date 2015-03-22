#' @docType methods
#' @rdname intent_p1-methods
#' @title Extract Image intent_p1 attribute 
#' @name intent_p1-methods
#' @aliases intent_p1,character-method
#' @import oro.nifti
#' @param object is a filename to pass to \link{fslval}
#' @export
#' 
setMethod("intent_p1", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "intent_p1", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
