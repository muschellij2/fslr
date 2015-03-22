#' @import oro.nifti
#' @export
setMethod("slice_code", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "slice_code", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
