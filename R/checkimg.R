#' @name checkimg-methods
#' @docType methods 
#' @aliases checkimg
#' 
#' @title Force object to filename with .nii extension
#' @param file character or \code{nifti} object
#' @param ... options passed to \code{\link{tempimg}}
#' @return character filename of image or temporary nii, 
#' with .nii extension
#' 
#' @export
#' @author John Muschelli \email{muschellij2@@gmail.com}
setGeneric("checkimg", function(file, ...) standardGeneric("checkimg"))

#' @rdname checkimg-methods
#' @aliases checkimg,nifti-method
#' @export
setMethod("checkimg", "nifti", function(file, ...) { 
  file = tempimg(file, ...)
  return(file)
})

#' @rdname checkimg-methods
#' @aliases checkimg,character-method
#' @importFrom R.utils gunzip
#'  
#' @export
setMethod("checkimg", "character", function(file, ...) { 
  ### add vector capability
  if (length(file) > 1){
    file = sapply(file, checkimg, ...)
    return(file)
  } else {
    file = path.expand(file)
    file = file.path(dirname(file), basename(file))
    suppressWarnings({
      file = normalizePath(file)
    })
    return(file)
  }
})


#' @rdname checkimg-methods
#' @aliases checkimg,list-method
#' @export
setMethod("checkimg", "list", function(file, ...) { 
  ### add vector capability
  file = sapply(file, checkimg, ...)
  return(file)
})



