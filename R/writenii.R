#' @title writeNIfTI with default non-reoirientation
#' 
#' @description  This function calls the \code{\link{writeNIfTI}} function from the 
#' \code{oro.nifti} package, but makes sure to remove \code{.nii} extension
#' @param nim object of class \code{nifti}, passed to \code{\link{writeNIfTI}}
#' @param filename path to save the NIfTI file.  Suffix will be removed
#' @param dtype Should \code{\link{datatyper}} be run before writing?
#' @param ... Additional arguments pased to \code{\link{writeNIfTI}}
#' @return Nothing
#' @rdname writeNIfTI2
#' @export
writeNIfTI2 <- function(nim, filename, dtype = FALSE, ...){
  if (dtype){
    nim = datatyper(nim)
  }
  oro.nifti::writeNIfTI(nim, nii.stub(filename), ...)
}

#' @rdname writeNIfTI2
#' @export
writenii <- function(nim, filename, dtype = FALSE, ...){
  if (dtype){
    nim = datatyper(nim)
  }
  oro.nifti::writeNIfTI(nim, nii.stub(filename), ...)
}