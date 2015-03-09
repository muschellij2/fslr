#' @title Check output filename
#'
#' @description This function checks if an output filename is not NULL in conjunction
#' whether the user would like to return an iamge
#' @param outfile output filename or NULL
#' @param retimg Should an image be returned
#' @param fileext a non-empty character vector giving the file extension
#' @export
#' @return Filename of output file or a temporary filename
check_outfile <- function(
  outfile, # output filename or NULL 
  retimg, # Should an image be returned
  fileext = ".nii.gz"
  ){
  if (retimg){
    if (is.null(outfile)) {
      outfile = tempfile(fileext = fileext)
    } 
  } else {
    stopifnot(!is.null(outfile))
  }  
  return(path.expand(outfile))
}