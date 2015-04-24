#' @name check_nifti-methods
#' @docType methods 
#' @aliases check_nifti 
#' @title Check if nifti image or read in a nifti image
#' @description Simple check to see if input is character or of 
#' class nifti
#' @return nifti object or array if allow.array=TRUE and x is an array
#' @seealso \link{readNIfTI}
#' @param x character path of image or 
#' an object of class nifti, or array
#' @param reorient (logical) passed to \code{\link{readNIfTI}} 
#' if the image
#' is to be re-oriented
#' @param allow.array (logical) Are array types allowed (TRUE) or
#' should there be an error if the object is not character or class
#' nifti.
#' @export 
#' @author John Muschelli \email{muschellij2@@gmail.com} 
setGeneric("check_nifti", function(x, reorient=FALSE, 
                                   allow.array=FALSE) {
  standardGeneric("check_nifti")
})

#' @rdname check_nifti-methods
#' @aliases check_nifti,nifti-method
#' @export
setMethod("check_nifti", "nifti", function(x, 
                                           reorient=FALSE, 
                                           allow.array=FALSE) { 
  return(x)
})

#' @rdname check_nifti-methods
#' @aliases check_nifti,character-method
#'  
#' @export
setMethod("check_nifti", "character", function(x, 
                                            reorient=FALSE, 
                                            allow.array=FALSE) { 
  ### add vector capability
  if (length(x) > 1){
    file = lapply(x, check_nifti,  
                  reorient = reorient, 
                  allow.array = allow.array)
    return(file)
  } else {
    file = readNIfTI(x, reorient = reorient)    
    return(file)
  }
})


#' @rdname check_nifti-methods
#' @aliases check_nifti,list-method
#' @export
setMethod("check_nifti", "list", function(x,  
                                          reorient=FALSE, 
                                          allow.array=FALSE) { 
  ### add vector capability
  file = lapply(x, check_nifti, 
                reorient = reorient, 
                allow.array = allow.array)
  return(file)
})


#' @rdname check_nifti-methods
#' @aliases check_nifti,array-method
#' @export
setMethod("check_nifti", "array", function(x,  
                                          reorient=FALSE, 
                                          allow.array=FALSE) { 
  if (!allow.array){
    stop("x is array but allow.array = FALSE")
  }
  return(x)
})



