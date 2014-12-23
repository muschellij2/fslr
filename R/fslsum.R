#' @title FSL Sum  
#' @description This function calls \code{fslstats -M -V} to get product, 
#' aka the approximate sum.
#' @param file (character) filename of image to be checked
#' @param ... options passed to \code{\link{fslstats}}
#' @return Numeric value
#' @note This may be approximate due to rounding
#' @export
fslsum <- function(file, ...){
  opts = "-M -V"
  res = fslstats(file, opts= opts, ...)
  res = strsplit(res, " ")[[1]]
  res = res[1:2]
  res = prod(as.numeric(res))
}
