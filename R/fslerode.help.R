#' @title fslerode Help
#' @description This function calls \code{fslmaths}'s help, as 
#' \code{fslerode} is a wrapper for \code{fslmaths}
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslerode.help() 
#' }
fslerode.help = function(...){
  fslmaths.help(...)
}
