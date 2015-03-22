#' @import oro.nifti
#' @export
setMethod("cal.min", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "cal_min", verbose = FALSE)
  res = as.numeric(res)
  return(res)
})
