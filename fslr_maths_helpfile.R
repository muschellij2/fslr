#' @title fsl%opt% Help
#' @description This function calls \code{fslmaths}'s help, as 
#' \code{fsl%opt%} is a wrapper for \code{fslmaths}
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fsl%opt%.help() 
#' }
fsl%opt%.help = function(...){
  fslmaths.help(...)
}
