#' @title Binarize Image using FSL 
#' @description This function calls \code{fslmaths -bin}.  The R functions wraps
#' \code{fslmaths}
#' @param file (character) image to be binarized
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fslmaths}
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
#' @examples 
#'   set.seed(5)
#' dims = rep(10, 3)
#' arr = array(rnorm(prod(dims)), dim = dims)
#' nim = oro.nifti::nifti(arr)
#' if (have.fsl()){
#'  fslbin(nim)
#'  fsl_bin(nim)
#'  }
#' 
fslbin = function(
  file,
  outfile=NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  opts = "", 
  ...){
  
  all.opts = paste("-bin ", opts, collapse = " " )
  res = fslmaths(file = file, outfile = outfile, 
                 retimg = retimg, reorient = reorient,
                 intern = intern, opts = all.opts, ...)
  
  return(res)  
}

