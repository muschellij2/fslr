#' @docType methods
#' @rdname slice_code-methods
#' @title Extract Image slice_code attribute 
#' @name slice_code-methods
#' @aliases slice_code,character-method
#' @import oro.nifti
#' @export
#' @description slice_code method for character types
#' @param object is a filename to pass to \link{fslval}
#' 
setMethod("slice_code", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "slice_code", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
