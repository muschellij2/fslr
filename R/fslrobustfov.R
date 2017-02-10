#' @title FSL Robust Field of View 
#' @description This function calls \code{robustfov} to
#' automatically crop the image
#' @param file (character) image to be manipulated
#' @param brain_size size of brain in z-dimension (default 150mm)
#' @param mat_name matrix output name
#' @param roi_name ROI volume output name
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If \code{retimg}, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fslrobustfov = function(
  file,
  brain_size = NULL, 
  mat_name = NULL, 
  roi_name = NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  verbose = TRUE,
  ...){
  
  opts = ""
  if (!is.null(brain_size)) {
    opts = paste(opts, "-b", brain_size)
  }
  if (!is.null(mat_name)) {
    opts = paste(opts, "-m", shQuote(mat_name))
  }  
  if (!is.null(roi_name)) {
    opts = paste(opts, "-m", shQuote(roi_name))
  }  
  if (verbose) {
    opts = paste(opts, "--verbose")
  }
  
  res = fslcmd(func = "robustfov", 
               file = file,
               outfile = NULL,
               retimg = retimg,
               reorient = reorient,
               intern = intern,
               opts = opts,
               frontopts = "-i",
               verbose = verbose,
               ... = ..., 
               samefile = TRUE,
               no.outfile = TRUE)
  
  return(res)  
}

#' @rdname fslrobustfov
#' @export
fsl_robustfov = function(
  retimg = FALSE,
  ...){

  res = fslrobustfov(..., retimg = retimg)
  return(res)  
}



#' @title FSL Robust Field of View Help
#' @description This function calls \code{robustfov} help
#' @export
fslrobustfov.help = function(){
  fslhelp(func_name = "robustfov")
}
