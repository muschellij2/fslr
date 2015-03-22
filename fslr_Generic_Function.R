#' @import oro.nifti
#' @export
setMethod("%%", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "%ff%", verbose = FALSE)
  res = %numeric%(res)
  return(res)
})
