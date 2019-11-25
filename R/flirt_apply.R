#' @title Apply Warp from FLIRT
#' @description This function applies a matrix from \code{\link{flirt}}
#' to other images
#' @param infile (character) input filename
#' @param reffile (character) reference image to be registered to
#' @param initmat (character) Matrix of transformation
#' @param outfile (character) output filename
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}. 
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) additional options to FLIRT
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return character or logical depending on intern
#' @export
flirt_apply = function(infile, 
                         reffile, 
                         initmat,                  
                         outfile = NULL,                  
                         retimg = TRUE,
                         reorient = FALSE,                 
                         intern=FALSE,
                         opts="", verbose = TRUE, ...){
  cmd <- get.fsl()
  if (retimg){
    if (is.null(outfile)) {
      outfile = tempfile()
    }
  } else {
    stopifnot(!is.null(outfile))
  }
  #   infile = path.expand(infile)
  #   outfile = path.expand(outfile)
  #   reffile = path.expand(reffile)
  infile = checkimg(infile, ...)  
  reffile = checkimg(reffile, ...)  
  outfile = checkimg(outfile, ...)  
  outfile = nii.stub(outfile)
  
  initmat = path.expand(initmat)
  cmd <- paste0(cmd, sprintf(
    'flirt -in "%s" -ref "%s" -applyxfm -init "%s" %s -out "%s" ', 
    infile, reffile, initmat, opts, outfile))
  
  if (verbose){
    message(cmd, "\n")
  }
  res = system(cmd, intern=intern)
  ext = get.imgext()
  outfile = paste0(outfile, ext)  
  if (retimg){
    img = readnii(outfile, reorient=reorient, ...)
    return(img)
  }
  return(outfile)
}

