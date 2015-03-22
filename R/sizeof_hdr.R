#' @docType methods
#' @rdname sizeof_hdr-methods
#' @title Extract Image sizeof_hdr attribute 
#' @name sizeof_hdr-methods
#' @aliases sizeof_hdr,character-method
#' @import oro.nifti
#' @param object is a filename to pass to \link{fslval}
#' @export
#' 
setMethod("sizeof_hdr", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "sizeof_hdr", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
