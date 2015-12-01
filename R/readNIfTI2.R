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
#' @param dtype Should \code{\link{datatyper}} be run after reading?
#' @param drop_dim Should \code{\link{drop_img_dim}} be run after reading?
#' @param warn Should warnings from \code{\link{readNIfTI}} be 
#' printed?  If not, \code{\link{suppressWarnings}} is called
#' @export
readnii <- function(..., reorient = FALSE, dtype = TRUE, 
                    drop_dim = TRUE,
                    warn = FALSE){
  if (warn) {
    nim = readNIfTI(..., reorient = reorient)
  } else {
    suppressWarnings({
      nim = readNIfTI(..., reorient = reorient)
    })
  }
  if (drop_dim) {
    nim = drop_img_dim(nim)
  }
  if (dtype) {
    nim = datatyper(nim)
  }
  return(nim)
}