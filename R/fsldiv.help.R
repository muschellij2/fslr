#' @title fsldiv Help
#' @description This function calls \code{fslmaths}'s help, as 
#' \code{fsldiv} is a wrapper for \code{fslmaths}
#' @param ... passed to \code{\link{fslmaths.help}}
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fsldiv.help() 
#' }
fsldiv.help = function(...){
  fslmaths.help(...)
}
