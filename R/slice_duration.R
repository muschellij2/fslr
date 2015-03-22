#' @docType methods
#' @rdname slice_duration-methods
#' @title Extract Image slice_duration attribute 
#' @name slice_duration-methods
#' @aliases slice_duration,character-method
#' @import oro.nifti
#' @param object is a filename to pass to \link{fslval}
#' @export
#' 
setMethod("slice_duration", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "slice_duration", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
