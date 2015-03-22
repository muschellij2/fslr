#' @import oro.nifti
#' @export
setMethod("aux.file", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "aux_file", verbose = FALSE)
  res = (res)
  return(res)
})
