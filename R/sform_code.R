#' @import oro.nifti
#' @export
setMethod("sform_code", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "sform_code", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
