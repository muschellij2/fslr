#' @import oro.nifti
#' @export
setMethod("qform_code", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "qform_code", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
