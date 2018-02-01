#' @title FSL FAST
#' @description This function calls \code{fast} from FSL
#' @param file (character) image to be manipulated
#' @param outfile (character) resultant image name (optional)
#' @param bias_correct (logical) if \code{FALSE}, then 
#' \code{"--nobias"} is passed to FAST.  Additional options can be 
#' sent using \code{opts}, but this is the most commonly one changed.
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fast}
#' @param out_type (character) Suffix to grab from outfile.  For 
#' example, output filename is \code{paste0(outfile, "_", out_type)}
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fast = function(
  file,
  outfile=NULL, 
  bias_correct = TRUE,
  retimg = TRUE,
  reorient = FALSE,
  intern=FALSE, 
  opts = "", 
  out_type = c("seg", "mixeltype", "pve_0", 
               "pve_1", "pve_2", "pveseg"),  
  verbose = TRUE,
  ...){
    
  cmd = get.fsl()
  file = checkimg(file, ...)
  cmd <- paste0(cmd, 'fast ')
  no.outfile = is.null(outfile)
  outfile = check_outfile(outfile=outfile, retimg=retimg, fileext = "")
  outfile = nii.stub(outfile)
  if (!bias_correct) {
    opts = c(opts, "--nobias")
  }
  if (verbose) {
    opts = c(opts, "--verbose")
  }
  opts = trimws(opts)
  opts = opts[ opts != "" ]
  opts = paste(opts, collapse = " ")
  
  cmd <- paste(cmd, sprintf('%s --out="%s" "%s";', opts, 
    outfile, file))
  ext = get.imgext()
  if (verbose){
    message(cmd, "\n")
  }
  res = system(cmd, intern=intern)  
  if (retimg){
    out_type = match.arg(out_type, c("seg", "mixeltype", "pve_0", 
                 "pve_1", "pve_2", "pveseg"))
    outfile = paste0(outfile, "_", out_type, ext)  
    img = readnii(outfile, reorient = reorient, ...)
    return(img)
  }
  outfile = paste0(outfile, "_", out_type, ext)  
  return(outfile)
  # return(res)  
}

#' @title FAST help
#' @description This function calls \code{fast}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fast.help()
#' }
fast.help = function(){
  return(fslhelp("fast", help.arg=""))
}

#' @title FSL Bias Correct
#' @description This function wraps a call to \code{fast} that performs bias
#' correction
#' @param file (character) image to be manipulated
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fast}
#' @param verbose (logical) print out command before running
#' @param remove.seg (logical) Should segmentation from FAST be removed? 
#' @param ... additional arguments passed to \code{\link{readnii}}. 
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fsl_biascorrect = function(
  file,
  outfile=NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern=FALSE, 
  opts = "", 
  verbose = TRUE,
  remove.seg = TRUE,
  ...){
  
  cmd = get.fsl()
  file = checkimg(file, ...)
  cmd <- paste0(cmd, 'fast ')
  no.outfile = is.null(outfile)
  outfile = check_outfile(outfile=outfile, retimg=retimg, fileext = "")
  
  outfile = nii.stub(outfile)

  cmd <- paste(cmd, sprintf(' %s -B --nopve --out="%s" "%s";', 
    opts, outfile, file))
  ext = get.imgext()
  if (verbose){
    message(cmd, "\n")
  }
  res = system(cmd, intern=intern)

  # if (have.outfile){
  ext = get.imgext()
  
  stub = nii.stub(outfile)
  ### remove extra files from fast
  seg_file = paste0(stub, "_seg", ext)
  if (remove.seg) file.remove(seg_file)
  
  output = paste0(stub, "_restore", ext)
  outfile = paste0(stub, ext)
  file.rename(output, outfile)
  # }

  if (retimg){
    img = readnii(outfile, reorient=reorient, ...)
    return(img)
  }
  
  return(res)  
}



#' @rdname fast
#' @aliases fsl_fast
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_fast = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fast(..., outfile = outfile, retimg = retimg)
  return(outfile)
}

#' @rdname fast
#' @aliases fslfast
#' @export
fslfast = function(
  ...
) {
  fast(...)
}
