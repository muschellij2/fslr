#' @title FSL Anatomical Processing Script
#' @description This function calls \code{fsl_anat} from FSL
#' @param file (character) image to be manipulated, should be full path
#' @param modality (character) Modality of Image to be run
#' @param outdir (character) output directory, if none specified, will 
#' default to \code{dirname(file)}
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fsl_anat}
#' @param verbose (logical) print out command before running
#' @param ... options passed to \code{\link{checkimg}}
#' @return Result from system command, depends if intern is TRUE or FALSE.
#' @export
fsl_anat = function(
  file,
  modality = c("T1", "T2", "PD"),
  outdir = NULL, 
  intern = FALSE, 
  opts = "", 
  verbose = TRUE, ...){
  
  ## check modality - should match fsl_anat
  modality = match.arg(modality)
  cmd = get.fsl()
  
  file = checkimg(file, ...)
  cmd <- paste0(cmd, 'fsl_ant ')
  if (is.null(outdir)){
    outdir = dirname(file)
  }
  cmd <- paste(cmd, sprintf(' %s -o "%s" -t %s -i "%s";', 
                            opts, outdir, modality, file))
  if (verbose){
    cat(cmd, "\n")
  }
  res = system(cmd, intern=intern)
  return(res)  
}

#' @title fsl_anat help
#' @description This function calls \code{fsl_anat}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fsl_anat.help()
#' }
fsl_anat.help = function(){
  return(fslhelp("fsl_anat"))
}
