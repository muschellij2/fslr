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
    
  cmd = get.fsl()
  file = checkimg(file)
  cmd <- paste0(cmd, 'fast ')
  no.outfile = is.null(outfile)
  if (retimg){
    if (is.null(outfile)) {
      outfile = tempfile()
    }
  } else {
    stopifnot(!is.null(outfile))
  }
  outfile = nii.stub(outfile)
  cmd <- paste(cmd, sprintf(' %s --out="%s" "%s";', opts, outfile, file))
  ext = get.imgext()
  if (verbose){
    cat(cmd, "\n")
  }
  res = system(cmd, intern=intern)
  outfile = paste0(outfile, ext)  
  if (retimg){
    img = readNIfTI(outfile, reorient=reorient, ...)
    return(img)
  }
  
  return(res)  
}

#' @title FAST help
#' @description This function calls \code{fast}'s help
#' @return Prints help output and returns output as character vector
#' @export
fast.help = function(){
  return(fslhelp("fast"))
}
