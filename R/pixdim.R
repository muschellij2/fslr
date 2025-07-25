#' @docType methods
#' @rdname pixdim-methods
#' @title Extract Image pixdim attribute 
#' @description Gets pixdim from a character
#' @name pixdim-methods
#' @aliases pixdim,character-method
#' @param object is a filename to pass to \link{fslval} 
#' @importMethodsFrom oro.nifti pixdim
#' 
#' @import oro.nifti
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
