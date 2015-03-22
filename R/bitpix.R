#' @import oro.nifti
#' @export
setMethod("bitpix", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "bitpix", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
