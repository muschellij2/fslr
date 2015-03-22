#' @import oro.nifti
#' @export
setMethod("slice_start", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "slice_start", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
