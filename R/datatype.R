#' @import oro.nifti
#' @export
setMethod("datatype", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "datatype", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
