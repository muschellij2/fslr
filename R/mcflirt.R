#' @title FSL Motion Correction
#' @description This function calls \code{mcflirt}
#' @param file (character) image to be manipulated
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If \code{retimg}, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{mcflirt}.  Cannot use 
#' \code{-o} or \code{-verbose}, as output file should be specified in \code{outfile}.
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
mcflirt = function(
  file,
  outfile = NULL,
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  opts = "", 
  verbose = TRUE,
  ...){
  
  outfile = check_outfile(outfile = outfile, retimg = retimg)
  opts = paste(opts, "-verbose", as.numeric(verbose), "-o")
  
  res = fslcmd(func = "mcflirt", 
               file = file,
               outfile = outfile,
               retimg = retimg,
               reorient = reorient,
               intern = intern,
               opts = opts,
               verbose = verbose,
               ... = ..., 
               frontopts = "-in",
               samefile = FALSE)
  
  return(res)  
}

#' @title MCFLIRT help
#' @description This function calls \code{mcflirt}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  mcflirt.help()
#' } 
mcflirt.help = function(){
  return(fslhelp("mcflirt", help.arg = "-help"))
}
