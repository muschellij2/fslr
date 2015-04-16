
#' @title %propername% Image using FSL 
#' @description This function calls \code{fslmaths -%opt%}.  
#' The R functions wraps \code{fslmaths}
#' @param file (character) input image to %longname%
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
fsl%opt% = function(
  file,
  outfile=NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  opts = "", 
  ...){
  
  all.opts = paste("-%opt% ", opts, collapse=" ")
  res = fslmaths(file=file, 
                 outfile=outfile, 
                 retimg=retimg, reorient=reorient,
                 intern=intern, opts = all.opts, ...)
  
  return(res)  
}

#' @title fsl%opt% Help
#' @description This function calls \code{fslmaths}'s help, as 
#' \code{fsl%opt%}is a wrapper for \code{fslmaths}
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fsl%opt%.help() 
#' }
fsl%opt%.help = function(...){
  fslmaths.help(...)
}
