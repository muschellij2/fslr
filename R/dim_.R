#' @name dim_-methods
#' @title Extract Image dim_ attribute
#' @description Methods that extract the ``dim_'' slot from the 
#' NIfTI/ANALYZE header.
#' @docType methods 
#' @param object is an object of class \code{character} 
#' @aliases dim_-methods 
#' @aliases dim_,character,character-method
#' @aliases dim_
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