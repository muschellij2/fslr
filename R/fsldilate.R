#' @name fsldilate
#' @title Dilate image using FSL
#' @description This function calls \code{fslmaths -ero} after inverting the image
#' to dilate an image with either 
#' the default FSL kernel or the kernel specified in \code{kopts}.  The function
#' either saves the image or returns an object of class nifti.
#' @param file (character) image to be dilated
#' @param outfile (character) resultant dilated image name 
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link[neurobase]{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param kopts (character) options for kernel
#' @param opts (character) additional options to be passed to fslmaths
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link[neurobase]{readnii}}.
#' @return Result from system command, depends if intern is TRUE or FALSE.  If 
#' retimg is TRUE, then the image will be returned. 
#' @import oro.nifti
#' @export
#' @examples
#' if (have.fsl()){
#' system.time({
#' dims = c(50, 50, 20)
#' x = array(rnorm(prod(dims)), dim = dims) 
#' img = nifti(x, dim= dims, 
#' datatype = convert.datatype()$FLOAT32, cal.min = min(x), 
#' cal.max = max(x), pixdim = rep(1, 4))
#' mask = img > .5
#' dilated = fsldilate(mask, kopts = "-kernel boxv 5", retimg=TRUE)
#' })
#' }    
fsldilate <- function(file, outfile=NULL,   
                     retimg = TRUE,
                     reorient = FALSE,
                     intern=FALSE, kopts = "", opts="", 
                     verbose = TRUE,
                     ...){
  
  cmd = get.fsl()
  outfile = check_outfile(outfile = outfile, retimg = retimg, fileext = "")
  outfile = nii.stub(outfile)
  file = checkimg(file, ...)    
  opts = paste0("-bin -mul -1 -add 1 ", kopts, " -ero -mul -1 -add 1 ", opts)
  cmd <- paste0(cmd, sprintf('fslmaths "%s" %s "%s"', 
                             file, opts, outfile))
  if (verbose){
    message(cmd, "\n")
  }
  res = system(cmd, intern=intern)
  ext = get.imgext()
  outfile = paste0(outfile, ext)
  stopifnot(file.exists(outfile))
  if (retimg){
    img = readnii(outfile, reorient=reorient, ...)
    return(img)
  }
  return(res)
}
