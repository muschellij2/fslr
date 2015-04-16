
#' @title Edge Strength Image using FSL 
#' @description This function calls \code{fslmaths -edge}.  
#' The R functions wraps \code{fslmaths}
#' @param file (character) input image to estimate edge strength
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
fsledge = function(
  file,
  outfile=NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  opts = "", 
  ...){
  
  all.opts = paste("-edge ", opts, collapse=" ")
  res = fslmaths(file=file, 
                 outfile=outfile, 
                 retimg=retimg, reorient=reorient,
                 intern=intern, opts = all.opts, ...)
  
  return(res)  
}
