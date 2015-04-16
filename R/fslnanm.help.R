#' @title fslnanm Help
#' @description This function calls \code{fslmaths}'s help, as 
#' \code{fslnanm} is a wrapper for \code{fslmaths}
#' @param ... passed to \code{\link{fslmaths.help}}
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslnanm.help() 
#' }
fslnanm.help = function(...){
  fslmaths.help(...)
}
