#' @title fslthresh Help
#' @description This function calls \code{fslmaths}'s help, as 
#' \code{fslthresh} is a wrapper for \code{fslmaths}
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslthresh.help() 
#' }
fslthresh.help = function(...){
  fslmaths.help(...)
}
