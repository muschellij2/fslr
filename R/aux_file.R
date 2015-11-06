#' @docType methods
#' @rdname aux_file-methods
#' @title Extract Image aux.file attribute 
#' @name aux.file-methods
#' @aliases aux.file,character-method
#' @import oro.nifti
#' @export
#' @description aux_file method for character types
#' @param object is a filename to pass to \link{fslval}
#' 
setMethod("aux.file", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "aux_file", verbose = FALSE)
  res = (res)
  return(res)
})
