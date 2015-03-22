#' @import oro.nifti
#' @export
setMethod("slice_end", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "slice_end", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
