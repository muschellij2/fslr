#' @rdname cal.max-methods
#' @param object is an object of class \code{nifti} 
#' @title Extract or Replace NIfTI/Analyze Min or Max Values
#' @aliases cal.max,character,character-method
setMethod("cal.max", "character", function(object){
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "cal_max", verbose = FALSE)
  res = as.numeric(res)
})


#' @rdname cal.min-methods
#' @param object is an object of class \code{nifti} 
#' @title Extract or Replace NIfTI/Analyze Min or Max Values
#' @aliases cal.min,character,character-method
setMethod("cal.min", "character", function(object){
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "cal_min", verbose = FALSE)
  res = as.numeric(res)
})


#' @rdname pixdim-methods
#' @param object is an object of class \code{nifti}
#' @title Extract or Replace NIfTI/Analyze Pixel Dimensions
#' @aliases pixdim,character,character-method
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


#' @rdname descrip-methods
#' @param object is an object of class \code{nifti} 
#' @title Extract or Replace NIfTI/Analyze Description
#' @aliases descrip,character,character-method
setMethod("descrip", "character", function(object){
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "descrip", verbose = FALSE)
  return(res)
})






