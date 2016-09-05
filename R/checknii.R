#' @name checknii-methods
#' @docType methods 
#' @aliases checknii
#' @description Ensures the output to be a character filename (or vector) from an input
#' image or \code{nifti}, but not gzipped and has \code{.nii} extension
#' 
#' @title Force object to filename with .nii extension
#' @param file character or \code{nifti} object
#' @param ... options passed to \code{\link{checkimg}}
#' @return character filename of image or temporary nii, 
#' with .nii extension
#' 
#' @export
#' @author John Muschelli \email{muschellij2@@gmail.com}
setGeneric("checknii", function(file, ...) standardGeneric("checknii"))

#' @rdname checknii-methods
#' @aliases checknii,nifti-method
#' @export
setMethod("checknii", "nifti", function(file, ...) { 
  file = checkimg(file, gzipped = FALSE, ...)
  return(file)
})

#' @rdname checknii-methods
#' @aliases checknii,character-method
#' @importFrom R.utils gunzip
#'  
#' @export
setMethod("checknii", "character", function(file, ...) { 
  ### add vector capability
  if (length(file) > 1){
    file = sapply(file, checknii, ...)
    return(file)
  } else {
    file = checkimg(file, gzipped = FALSE, ...)
    if (grepl("[.]gz$", file)){
      file = gunzip(filename = file, 
                    temporary = TRUE, 
                    overwrite = TRUE, 
                    remove = FALSE)
    }
    if (!grepl("[.]nii$", file)) {
      stop("Original file was not .nii or .nii.gz")
    }
  } 
  return(file)
})


#' @rdname checknii-methods
#' @aliases checknii,list-method
#' @export
setMethod("checknii", "list", function(file, ...) { 
  ### add vector capability
  file = sapply(file, checknii, ...)
  return(file)
})

#' @rdname checknii-methods
#' @aliases ensure_nii
#' @export
ensure_nii = function(file, ...) { 
  checknii(file = file, ...)
}

