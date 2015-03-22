#' @docType methods
#' @rdname aux_file-methods
#' @title Extract Image aux.file attribute 
#' @name aux.file-methods
#' @aliases aux.file,character-method
#' @import oro.nifti
#' @param object is a filename to pass to \link{fslval}
#' @export
#' 
setMethod("aux.file", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "aux_file", verbose = FALSE)
  res = (res)
  return(res)
})
