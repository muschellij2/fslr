#' @title FSL Change file type
#' @description This function calls \code{fslchfiletype}
#' @param file (character) image to be manipulated
#' @param filetype filetype to change image to
#' @param outfile Output filename. If NULL, will overwrite input file
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If \code{retimg}, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fslchfiletype = function(
  file,
  filetype = "NIFTI_GZ",
  outfile=NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  verbose = TRUE,
  ...){

  filetype = match.arg(filetype, c("NIFTI_GZ", "ANALYZE", 
                                   "NIFTI", "NIFTI_PAIR",
                                   "ANALYZE_GZ", "NIFTI_PAIR_GZ"))
  
  res = fslcmd(func=paste0("fslchfiletype ", filetype), 
               file= file,
               outfile = outfile,
               retimg = retimg,
               reorient = reorient,
               intern = intern,
               opts = "",
               verbose = verbose,
               ... = ..., 
               samefile = TRUE)
  
    return(res)  
}
  


#' @title fslchfiletype help
#' @description This function calls \code{fslchfiletype}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslchfiletype.help()
#' }  
fslchfiletype.help = function(){
  return(fslhelp("fslchfiletype"))
}
