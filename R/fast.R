#' @title FSL FAST
#' @description This function calls \code{fast} from FSL
#' @param file (character) image to be manipulated
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fslmaths}
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fast = function(
  file,
  outfile=NULL, 
  retimg = FALSE,
  reorient = FALSE,
  intern=TRUE, 
  opts = "", 
  verbose = TRUE,
  ...){
    
  res = fslcmd("fast", 
               file=file, 
               outfile = outfile, retimg= retimg,
               reorient=reorient, intern=intern, opts=opts, 
               ... = ..., verbose = verbose, samefile = FALSE)
  
  return(res)  
}

