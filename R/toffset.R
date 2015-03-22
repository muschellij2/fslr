#' @export
setMethod("toffset", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "time_offset", verbose = FALSE)
  res = (res)
  return(res)
})
