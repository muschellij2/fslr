#' @docType methods
#' @rdname toffset-methods
#' @title Extract Image toffset attribute 
#' @name toffset-methods
#' @aliases toffset,character-method
#' @param object is a filename to pass to \link{fslval}
#' @import oro.nifti
#' @export
setMethod("toffset", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "time_offset", verbose = FALSE)
  res = (res)
  return(res)
})
