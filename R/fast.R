#' @title FSL FAST
#' @description This function calls \code{fast} from FSL
#' @param file (character) image to be manipulated
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fast}
#' @param out_type (character) Suffix to grab from outfile.  For 
#' example, output filename is \code{paste0(outfile, "_", out_type)}
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fast = function(
  file,
  outfile=NULL, 
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
  cmd <- paste(cmd, sprintf(' %s --out="%s" "%s";', opts, 
    outfile, file))
  ext = get.imgext()
  if (verbose){
    cat(cmd, "\n")
  }
  res = system(cmd, intern=intern)  
  if (retimg){
    out_type = match.arg(out_type, c("seg", "mixeltype", "pve_0", 
                 "pve_1", "pve_2", "pveseg"))
    outfile = paste0(outfile, "_", out_type, ext)  
    img = readNIfTI(outfile, reorient=reorient, ...)
    return(img)
  }
  
  return(res)  
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
#' corretion
#' @param file (character) image to be manipulated
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fast}
#' @param verbose (logical) print out command before running
#' @param remove.seg (logical) Should segmentation from FAST be removed? 
#' @param ... additional arguments passed to \code{\link{readNIfTI}}. 
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
    cat(cmd, "\n")
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
    img = readNIfTI(outfile, reorient=reorient, ...)
    return(img)
  }
  
  return(res)  
}

