
#' @title Perform OR/Union operation on Images using FSL 
#' @description This function calls \code{fslmaths file -add file2 -bin}.  
#' The R functions wraps \code{fslmaths}
#' @param file (character) input image 
#' @param file2 (character) image to be union
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented 
#' when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fslmaths}
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fslor = function(
  file,
  file2,
  outfile = NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  opts = "", 
  ...){
  
  res = fsl_add(file = file, 
                file2 = file2, 
                opts = opts, ...)
  res = fsl_bin(
    file = res, 
    outfile = outfile, 
    retimg = retimg,
    reorient = reorient,
    intern = intern,
    opts = opts,
    ...)
  
  return(res)  
}


#' @rdname fslor
#' @aliases fsl_or
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_or = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslor(..., outfile = outfile, retimg = retimg)
  return(outfile)
}

