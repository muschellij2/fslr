#' @docType methods
#' @rdname qform_code-methods
#' @title Extract Image qform_code attribute 
#' @name qform_code-methods
#' @aliases qform_code,character-method
#' @import oro.nifti
#' @export
#' @description qform_code method for character types
#' @param object is a filename to pass to \link{fslval}
#' 
setMethod("qform_code", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "qform_code", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
