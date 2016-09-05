#' @name checkniigz-methods
#' @docType methods 
#' @aliases checkniigz
#' @description Ensures the output to be a character filename (or vector) 
#' from an input
#' image or \code{nifti}, to be gzipped and has \code{.nii.gz} extension
#' 
#' @title Force object to filename with .nii.gz extension
#' @param file character or \code{nifti} object
#' @param ... options passed to \code{\link{checkimg}}
#' @return Character filename of image or temporary nii, 
#' with .nii.gz extension
#' 
#' @export
#' @author John Muschelli \email{muschellij2@@gmail.com}
setGeneric("checkniigz", function(file, ...) standardGeneric("checkniigz"))

#' @rdname checkniigz-methods
#' @aliases checkniigz,nifti-method
#' @export
setMethod("checkniigz", "nifti", function(file, ...) { 
  file = checkimg(file, gzipped = TRUE, ...)
  return(file)
})

#' @rdname checkniigz-methods
#' @aliases checkniigz,character-method
#' @importFrom R.utils gzip
#'  
#' @export
setMethod("checkniigz", "character", function(file, ...) { 
  ### add vector capability
  if (length(file) > 1) {
    file = sapply(file, checkniigz, ...)
    return(file)
  } else {
    file = checkimg(file, gzipped = TRUE, ...)
    if (!grepl("[.]gz$", file)) {
      file = R.utils::gzip(filename = file, 
                  temporary = TRUE, 
                  overwrite = TRUE, 
                  remove = FALSE)
    }
    if (!grepl("[.]nii[.]gz$", file)) {
      stop("Original file was not .nii or .nii.gz")
    }
  } 
  return(file)
})


#' @rdname checkniigz-methods
#' @aliases checkniigz,list-method
#' @export
setMethod("checkniigz", "list", function(file, ...) { 
  ### add vector capability
  file = sapply(file, checkniigz, ...)
  return(file)
})

#' @rdname checkniigz-methods
#' @aliases ensure_nii_gz
#' @export
ensure_nii_gz = function(file, ...) { 
  checkniigz(file = file, ...)
}