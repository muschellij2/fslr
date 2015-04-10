 #' @title Apply Warp from FNIRT
#' @description This function applies a coefficient map from \code{\link{fnirt}}
#' to other images
#' @param infile (character) input filename
#' @param reffile (character) reference image to be registered to
#' @param warpfile (character) reference image to be registered to
#' @param outfile (character) output filename
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}. 
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) additional options to FLIRT
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return character or logical depending on intern
#' @export
fsl_applywarp = function(infile, 
                 reffile, 
                 warpfile,                  
                 outfile = NULL,                  
                 retimg = TRUE,
                 reorient = FALSE,                 
                 intern = FALSE,
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
  
  cmd <- paste0(cmd, sprintf(
    'applywarp --in="%s" --ref="%s" --out="%s" --warp="%s" %s', 
    infile, reffile, outfile, warpfile, opts))
  
  if (verbose){
    cat(cmd, "\n")
  }
  res = system(cmd, intern=intern)
  ext = get.imgext()
  outfile = paste0(outfile, ext)  
  if (retimg){
    img = readNIfTI(outfile, reorient=reorient, ...)
    return(img)
  }
  return(res)
}


#' @title FSL applywarp help
#' @description This function calls \code{applywarp}'s help
#' @return Prints help output and returns output as character vector
#' @export
fsl_applywarp.help = function(){
  return(fslhelp("applywarp"))
}
