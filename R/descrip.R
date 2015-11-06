#' @docType methods
#' @rdname descrip-methods
#' @title Extract Image descrip attribute 
#' @name descrip-methods
#' @aliases descrip,character-method
#' @import oro.nifti
#' @export
#' @description descrip method for character types
#' @param object is a filename to pass to \link{fslval}
#' 
setMethod("descrip", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "descrip", verbose = FALSE)
  res = (res)
  return(res)
})
