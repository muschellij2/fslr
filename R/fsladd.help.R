#' @title fsladd Help
#' @description This function calls \code{fslmaths}'s help, as 
#' \code{fsladd} is a wrapper for \code{fslmaths}
#' @param ... passed to \code{\link{fslmaths.help}}
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fsladd.help() 
#' }
fsladd.help = function(...){
  fslmaths.help(...)
}
