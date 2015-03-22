#' @import oro.nifti
#' @export
setMethod("scl_slope", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "scl_slope", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
