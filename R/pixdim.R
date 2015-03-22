#' @docType methods
#' @rdname pixdim-methods
#' @title Extract Image pixdim attribute 
#' @name pixdim-methods
#' @aliases pixdim,character-method
#' @param object is a filename to pass to \link{fslval} 
#' @import oro.nifti
#' @export
setMethod("pixdim", "character", function(object){
  object = path.expand(object)
  stopifnot(file.exists(object))
  slots = paste0("pixdim", 0:7)
  res = sapply(slots, function(key) {
    fslval(object, keyword = key, verbose = FALSE)
  })
  res = as.numeric(res)
  return(res)
})