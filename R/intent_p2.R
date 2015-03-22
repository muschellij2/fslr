#' @import oro.nifti
#' @export
setMethod("intent_p2", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "intent_p2", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
