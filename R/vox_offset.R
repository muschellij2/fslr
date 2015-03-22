#' @import oro.nifti
#' @export
setMethod("vox_offset", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "vox_offset", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
