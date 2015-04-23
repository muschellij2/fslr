#' @title Force object to filename with .nii extension
#' @param file character or \code{nifti} object
#' @param ... options passed to \code{\link{tempimg}}
#' @return character filename of image or temporary nii, 
#' with .nii extension
#' @export
checknii <- function(file, ...){
  ### add vector capability
  if (length(file) > 0){
    file = sapply(file, checknii, ...)
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
}