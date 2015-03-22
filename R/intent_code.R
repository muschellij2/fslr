#' @import oro.nifti
#' @export
setMethod("intent_code", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "intent_code", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
