#' @import oro.nifti
#' @export
setMethod("magic", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "magic", verbose = FALSE)
  res = (res)
  return(res)
})
