#' @title Eddy Current Correction
#' @description This function calls \code{eddy_correct} from FSL for 
#' DTI Processing
#' @param infile input filename of 4D image.
#' @param outfile Output filename 
#' @param retimg (logical) return image of class nifti
#' @param reference_no Set the volume number for the reference volume 
#' that will be used as a target to register all other volumes to. 
#' (default=0, i.e. the first volume)
#' @param ... Additional arguments passed to \code{\link{fslcmd}}
#'
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
eddy_correct = function(infile, 
                        outfile = NULL, 
                        retimg = TRUE, 
                        reference_no = 0, ...) {
  res = fslcmd(
    func = "eddy_correct",
    file = infile,
    outfile = outfile, 
    retimg = retimg,
    opts = reference_no,
    opts_after_outfile = TRUE,
    ...)
  return(res)   
}
