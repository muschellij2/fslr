#' @import oro.nifti
#' @export
setMethod("sizeof_hdr", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "sizeof_hdr", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
