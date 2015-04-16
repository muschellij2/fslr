#' @title FSL Copy Geometry 
#' @description This function calls \code{fslcpgeom}
#' @param file (character) image to be manipulated
#' @param file_with_header image with header to be copied over
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
  all.opts = paste(shQuote(file), opts, collapse = " ")
  ### need to switch up file and file_with_header
  ### first file is the one copying information from 
  res = fslcmd(func="fslcpgeom", 
               file= file_with_header,
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