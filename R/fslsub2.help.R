#' @title fslsub2 Help
#' @description This function calls \code{fslmaths}'s help, as 
#' \code{fslsub2} is a wrapper for \code{fslmaths}
#' @param ... passed to \code{\link{fslmaths.help}}
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslsub2.help() 
#' }
fslsub2.help = function(...){
  fslmaths.help(...)
}
