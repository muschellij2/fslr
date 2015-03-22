#' @docType methods
#' @rdname vox_offset-methods
#' @title Extract Image vox_offset attribute 
#' @name vox_offset-methods
#' @aliases vox_offset,character-method
#' @import oro.nifti
#' @param object is a filename to pass to \link{fslval}
#' @export
#' 
setMethod("vox_offset", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "vox_offset", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
