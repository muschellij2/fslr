#' @import oro.nifti
#' @export
setMethod("slice_duration", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "slice_duration", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
