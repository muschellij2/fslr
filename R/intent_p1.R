#' @import oro.nifti
#' @export
setMethod("intent_p1", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "intent_p1", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
