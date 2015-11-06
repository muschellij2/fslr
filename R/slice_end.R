#' @docType methods
#' @rdname slice_end-methods
#' @title Extract Image slice_end attribute 
#' @name slice_end-methods
#' @aliases slice_end,character-method
#' @import oro.nifti
#' @export
#' @description slice_end method for character types
#' @param object is a filename to pass to \link{fslval}
#' 
setMethod("slice_end", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "slice_end", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
