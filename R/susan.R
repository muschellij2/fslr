#' @title FSL SUSAN noise reduction
#'
#' @description Implements Smallest Univalue Segment Assimilating Nucleus 
#' (SUSAN) noise reduction technique from FSL
#' @param file (character) image to be manipulated
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param bthresh brightness threshold and should be greater than noise level and less than contrast of edges to be preserved.
#' @param sigma spatial size (sigma i.e. half-width) of smoothing in mm.
#' @param dimg dimensionality (2 or 3) depending on whether smoothing is to be within-plane (2) or fully 3D (3).
#' @param use_median determines whether to use a local median filter in the cases where single-point noise is detected (0 or 1).
#' @param n_usans determines whether the smoothing area (USAN) is to be found from secondary images (0 1 or 2).
#' @param extra.scans List of extra scans for USAN.  List of 
#' \code{n_usans} elements, where each element has 2 named objects
#' \code{bthresh} and \code{filename}
#' @param opts (character) operations to be passed to \code{susan}, not 
#' currently used. 
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{fslcmd}}. 
#' @export
#' @references S.M. Smith and J.M. Brady. SUSAN -a new approach to low level image processing. 
#' International Journal of Computer Vision, 23(1):45-78, May 1997.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
susan <- function(file,
                 outfile = NULL,
                 retimg = TRUE,
                 reorient = FALSE,
                 intern = FALSE,                  
                 bthresh = 0.1, # brightness threshold and should be greater than noise level and less than contrast of edges to be preserved.
                 sigma = 3, # spatial size (sigma, i.e., half-width) of smoothing, in mm.
                 dimg = c(3, 2), # dimensionality (2 or 3), depending on whether smoothing is to be within-plane (2) or fully 3D (3).
                 use_median = FALSE, # determines whether to use a local median filter in the cases where single-point noise is detected (0 or 1).
                 n_usans = c(0, 1, 2), # determines whether the smoothing area (USAN) is to be found from secondary images (0, 1 or 2).    
                 extra.scans = list(), # Extra scans for
                 opts = "",
                 verbose = TRUE,
                 ...){
  ### get the values
  dimg = match.arg(dimg, c(3, 2))
  n_usans = match.arg(n_usans, c(0, 1, 2))
  l.extra = length(extra.scans)
  
  use_median = as.numeric(use_median)
  stopifnot(use_median %in% c(0, 1))
  stopifnot(n_usans == l.extra)
  ## put the options together
  susan_opts = paste(bthresh, sigma, dimg, use_median, n_usans)
  for (iextra in seq_along(extra.scans)){
    bt = extra.scans[[iextra]]$bthresh
    fname = extra.scans[[iextra]]$filename
    susan_opts = paste(susan_opts, fname, bt)
  }
  
  ## no additional options available
#   susan_opts = paste(susan_opts, opts)
  res = fslcmd("susan", 
               file=file, 
               outfile = outfile, retimg= retimg,
               reorient=reorient, intern=intern, opts= susan_opts, 
               ... = ..., verbose = verbose, samefile = FALSE)
  return(res)
  
}


#' @title FSL SUSAN Help
#' @description This function calls \code{susan}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  susan.help() 
#' }
susan.help = function(){
  return(fslhelp("susan"))
}