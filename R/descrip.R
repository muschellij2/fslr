#' @import oro.nifti
#' @export
setMethod("descrip", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "descrip", verbose = FALSE)
  res = (res)
  return(res)
})
