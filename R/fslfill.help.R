#' @title fslfill Help
#' @description This function calls \code{fslmaths}'s help, as 
#' \code{fslfill} is a wrapper for \code{fslmaths}
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslfill.help() 
#' }
fslfill.help = function(...){
  fslmaths.help(...)
}
