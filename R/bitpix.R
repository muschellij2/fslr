#' @docType methods
#' @rdname bitpix-methods
#' @title Extract Image bitpix attribute 
#' @name bitpix-methods
#' @aliases bitpix,character-method
#' @import oro.nifti
#' @export
#' @description bitpix method for character types
#' @param object is a filename to pass to \link{fslval}
#' 
setMethod("bitpix", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "bitpix", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
