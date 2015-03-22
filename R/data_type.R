#' @import oro.nifti
#' @export
setMethod("data_type", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "data_type", verbose = FALSE)
  res = (res)
  return(res)
})
