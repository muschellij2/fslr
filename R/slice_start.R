#' @docType methods
#' @rdname slice_start-methods
#' @title Extract Image slice_start attribute 
#' @name slice_start-methods
#' @aliases slice_start,character-method
#' @import oro.nifti
#' @param object is a filename to pass to \link{fslval}
#' @export
#' 
setMethod("slice_start", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "slice_start", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
