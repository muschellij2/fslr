#' @docType methods
#' @rdname magic-methods
#' @title Extract Image magic attribute 
#' @name magic-methods
#' @aliases magic,character-method
#' @import oro.nifti
#' @param object is a filename to pass to \link{fslval}
#' @export
#' 
setMethod("magic", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "magic", verbose = FALSE)
  res = (res)
  return(res)
})
