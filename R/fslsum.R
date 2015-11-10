#' @title FSL Sum  
#' @description This function calls \code{fslstats -M -V} to get product, 
#' aka the approximate sum.
#' @param file (character) filename of image to be checked
#' @param opts Additional options to pass to \code{\link{fslstats}}
#' @param ts (logical) is the series a timeseries (4D), invoking \code{-t} 
#' option  
#' @param ... options passed to \code{\link{fslstats}}
#' @return Numeric value
#' @note This may be approximate due to rounding
#' @export
fslsum <- function(file, opts = "", ts = FALSE, ...){
  all.opts = paste("-M -V ", opts)
  res = fslstats(file, opts = all.opts, ts = ts, ...)
  res = strsplit(res, " ")
  res = sapply(res, function(x){
    x = x[1:2]
    x = prod(as.numeric(x))
  })
  if (length(res) == 1){
    res = res[[1]]
  }
  return(res)
}
