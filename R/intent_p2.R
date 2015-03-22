#' @docType methods
#' @rdname intent_p2-methods
#' @title Extract Image intent_p2 attribute 
#' @name intent_p2-methods
#' @aliases intent_p2,character-method
#' @import oro.nifti
#' @param object is a filename to pass to \link{fslval}
#' @export
#' 
setMethod("intent_p2", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "intent_p2", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
