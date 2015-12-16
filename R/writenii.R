#' @title writeNIfTI with default non-reoirientation
#' 
#' @description  This function calls the \code{\link{writeNIfTI}} function from the 
#' \code{oro.nifti} package, but makes sure to remove \code{.nii} extension and
#' warnings can be suppressed.
#' @param nim object of class \code{nifti}, passed to \code{\link{writeNIfTI}}
#' @param filename path to save the NIfTI file.  Suffix will be removed
#' @param dtype Should \code{\link{datatyper}} be run before writing?
#' @param ... Additional arguments pased to \code{\link{writeNIfTI}}
#' @note While \code{writeNIfTI2} does not run \code{\link{datatyper}} as default, 
#' \code{writenii} does.  Additional functionality will be added to \code{writenii} likely
#' but will not to \code{writeNIfTI2}
#' @return Nothing
#' @rdname writeNIfTI2
#' @export
writeNIfTI2 <- function(nim, filename, dtype = FALSE, ...){
  if (dtype) {
    nim = datatyper(nim)
  }
  oro.nifti::writeNIfTI(nim, nii.stub(filename), ...)
}

#' @rdname writeNIfTI2
#' @param drop_dim Should \code{\link{drop_img_dim}} be run before writing?
#' @param warn Should warnings from \code{\link{writeNIfTI}} be 
#' printed?  If not, \code{\link{suppressWarnings}} is called 
#' @export
writenii <- function(nim, filename, 
                     dtype = TRUE, 
                     drop_dim = TRUE, 
                     warn = FALSE, ...){
  if (drop_dim) {
    nim = drop_img_dim(nim)
  }
  if (dtype) {
    nim = datatyper(nim, warn = warn)
  }
  if (warn) {
    x = oro.nifti::writeNIfTI(nim, nii.stub(filename), ...)
  } else {
    suppressWarnings({
      x = oro.nifti::writeNIfTI(nim, nii.stub(filename), ...)
    })
  }
  return(invisible(x))
}