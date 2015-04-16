#' @title FSL Copy Geometry 
#' @description This function calls \code{fslcpgeom}
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
fslcpgeom = function(
  file,
  file_with_header,
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  opts = "",
  verbose = TRUE,
  ...){
  
  file_with_header = checkimg(file_with_header, ...)
  all.opts = paste(file_with_header, opts, collapse=" ")
  res = fslcmd(func="fslcpgeom", 
               file= file,
               outfile = NULL,
               retimg = retimg,
               reorient = reorient,
               intern = intern,
               opts = all.opts,
               verbose = verbose,
               ... = ..., 
               samefile = TRUE)  
  
  return(res)  
}


#' @title fslcpgeom help
#' @description This function calls \code{fslcpgeom}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslcpgeom.help()
#' }  
fslcpgeom.help = function(){
  return(fslhelp("fslcpgeom", help.arg=""))
}