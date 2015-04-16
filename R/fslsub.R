
#' @title Subtract Images using FSL 
#' @description This function calls \code{fslmaths -sub}.  
#' The R functions wraps \code{fslmaths}
#' @param file (character) input image 
#' @param file (character) image to be subtracted
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented 
#' when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fslmaths}
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fslsub = function(
  file,
  file2,
  outfile=NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  opts = "", 
  ...){
  
  file2 = checkimg(file2, ...)
  all.opts = paste(paste("-sub ", file2), 
                   opts, collapse=" ")
  res = fslmaths(file=file, 
                 outfile=outfile, 
                 retimg=retimg, reorient=reorient,
                 intern=intern, opts = all.opts, ...)
  
  return(res)  
}

#' @title fslsub Help
#' @description This function calls \code{fslmaths}'s help, as 
#' \code{fslsub}is a wrapper for \code{fslmaths}
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslsub.help() 
#' }
fslsub.help = function(...){
  fslmaths.help(...)
}
 
