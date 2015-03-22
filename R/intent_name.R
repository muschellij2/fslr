#' @import oro.nifti
#' @export
setMethod("intent_name", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "intent_name", verbose = FALSE)
  res = (res)
  return(res)
})
