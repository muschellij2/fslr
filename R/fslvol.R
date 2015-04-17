#' @title FSL Volume in mL (or cubic centimeters)
#' @description This function wraps \code{\link{fslsum}} and 
#' \code{\link{voxdim}}
#' @param file (character) filename of image to be checked
#' @param ... options passed to \code{\link{fslsum}}
#' @return Numeric value of volume in mL
#' @note This may be approximate due to rounding
#' @export
fslvol <- function(file, ...){
  file = checkimg(file)
  res = fslsum(file, ...)
  
  vdim = prod(voxdim(file))
  vol = res * vdim / 1000
  return(vol)
}
