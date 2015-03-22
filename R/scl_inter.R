#' @import oro.nifti
#' @export
setMethod("scl_inter", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "scl_inter", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
