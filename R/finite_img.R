#' @name finite_img-methods
#' @docType methods 
#' @aliases finite_img 
#' @title Finite Image
#' @description Simple wrapper for setting non-finite values to zero 
#' @return nifti object 
#' @param img character path of image or 
#' an object of class \code{nifti}, or list of images
#' @param replace Value to replace non-finite values to
#' @export 
#' @author John Muschelli \email{muschellij2@@gmail.com} 
setGeneric("finite_img", function(img, replace = 0) {
  standardGeneric("finite_img")
})

#' @rdname finite_img-methods
#' @aliases finite_img,finite_img-method
#' @export
setMethod("finite_img", "nifti", function(img, replace = 0) { 
  img[ !is.finite(img) ] = replace
  img = cal_img(img)
  return(img)
})

#' @rdname finite_img-methods
#' @aliases finite_img,character-method
#'  
#' @export
setMethod("finite_img", "character", function(img, replace = 0) { 
  img = check_nifti(img)
  img = finite_img(img, replace = replace)
  return(img)
})


#' @rdname finite_img-methods
#' @aliases finite_img,list-method
#' @export
setMethod("finite_img", "list", function(img, replace = 0) { 
  ### add vector capability
  img = lapply(img, finite_img, replace = replace)
  return(img)
})

