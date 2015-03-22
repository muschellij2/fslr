#' @import oro.nifti
#' @export
setMethod("cal.max", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "cal_max", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
