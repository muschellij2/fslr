#' @name pixdim-methods
#' @title Extract Image pixdim attribute
#' @description Methods that extract the ``pixdim'' slot from the 
#' NIfTI/ANALYZE header.
#' @docType methods 
#' @param object is an object of class \code{character} 
#' @aliases pixdim-methods 
#' @aliases pixdim,character,character-method
#' @aliases pixdim
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