#' @docType methods
#' @rdname dim_-methods
#' @title Extract Image dim_ attribute 
#' @name dim_-methods
#' @aliases dim_,character-method
#' @param object is a filename to pass to \link{fslval}
#' @import oro.nifti
#' @export
setMethod("dim_", "character", function(object){
  object = path.expand(object)
  stopifnot(file.exists(object))
  slots = paste0("dim0", 0:7)
  res = sapply(slots, function(key) {
    fslval(object, keyword = key, verbose = FALSE)
  })
  res = as.numeric(res)
  return(res)
})