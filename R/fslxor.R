#' @title Perform XOR/Exclusive Or operation on Images using FSL 
#' @description This function calls \code{fslmaths file -add file2 -bin}
#' after binarizing \code{file} and \code{file2} using
#' \code{\link{fslbin}} and then uses \code{\link{fsl_thresh}} 
#' to threshold any values greater than 1 back to zero.
#' @param file (character) input image 
#' @param file2 (character) image to be XOR'd
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented 
#' when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fslxor = function(
  file,
  file2,
  outfile = NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  ...){
  
  file = fsl_bin(file, retimg = FALSE)
  file2 = fsl_bin(file2, retimg = FALSE)  
  res = fsl_add(
    file = file, 
    file2 = file2,
    ...)
  res = fslthresh(
    file = res, 
    thresh = 0, 
    uthresh = 1,
    outfile = outfile, 
    retimg = retimg,
    reorient = reorient,
    intern = intern,
    ...)
  
  return(res)  
}


#' @rdname fslxor
#' @aliases fsl_xor
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_xor = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslxor(..., outfile = outfile, retimg = retimg)
  return(outfile)
}

