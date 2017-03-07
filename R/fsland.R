#' @title Logical AND with Images using FSL 
#' @description This function multiplies two images 
#' using \code{\link{fslmul}}) after
#' binarizing the images (using \code{\link{fslbin}}
#' @param file (character) input image 
#' @param file2 (character) image to be multiplied
#' @param ... additional arguments passed to \code{\link{fslmul}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' result from system command, depends if intern is TRUE or FALSE.
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping 
fsland = function(
  file,
  file2,
  ...){
  file = fsl_bin(file, ...)
  file2 = fsl_bin(file2, ...)
  res = fslmul(file = file, file2 = file2, ...)
  return(res)  
}


#' @rdname fsland
#' @aliases fsl_and
#' @export
fsl_and = function(
  file,
  file2,
  ...){
  file = fsl_bin(file, ...)
  file2 = fsl_bin(file2, ...)
  res = fsl_mul(file = file, file2 = file2, ...)
  return(res)  
}