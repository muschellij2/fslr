#' @title FSL Sum  
#' @description This function calls \code{fslstats -M -V} to get product, 
#' aka the approximate sum.
#' @param file (character) filename of image to be checked
#' @param opts Additional options to pass to \code{\link{fslstats}}
#' @param ... options passed to \code{\link{fslstats}}
#' @return Numeric value
#' @note This may be approximate due to rounding
#' @export
fslsum <- function(file, opts = "", ...){
  all.opts = paste("-M -V ", opts)
  res = fslstats(file, opts= all.opts, ...)
  res = strsplit(res, " ")[[1]]
  res = res[1:2]
  res = prod(as.numeric(res))
  return(res)
}
