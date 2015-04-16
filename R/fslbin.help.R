#' @title fslbin Help
#' @description This function calls \code{fslmaths}'s help, as 
#' \code{fslbin} is a wrapper for \code{fslmaths}
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslbin.help() 
#' }
fslbin.help = function(...){
  fslmaths.help(...)
}
