#' @title readNIfTI with default non-reoirientation
#' 
#' @description  This function calls the \code{\link{readNIfTI}} function from the 
#' \code{oro.nifti} package, but sets the reorientation to \code{FALSE} by default
#' @param ... Arguments to pass to \code{\link{readNIfTI}}
#' @param reorient Reorientation argument to pass to \code{\link{readNIfTI}}
#' @return \code{nifti} object
#' @rdname readNIfTI2
#' @export
readNIfTI2 <- function(..., reorient = FALSE){
  readNIfTI(..., reorient = reorient)
}

#' @rdname readNIfTI2
#' @export
readnii <- function(..., reorient = FALSE){
  readNIfTI(..., reorient = reorient)
}