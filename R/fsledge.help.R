#' @title fsledge Help
#' @description This function calls \code{fslmaths}'s help, as 
#' \code{fsledge} is a wrapper for \code{fslmaths}
#' @param ... passed to \code{\link{fslmaths.help}}
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fsledge.help() 
#' }
fsledge.help = function(...){
  fslmaths.help(...)
}
