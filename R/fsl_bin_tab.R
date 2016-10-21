#' @title Quick Tabulation for logical images
#'
#' @description Creates a 2 by 2 table for 
#' @param x filename of logical or 0/1 image
#' @param y filename of logical or 0/1 vimage
#' @param dnames names for table
#' @note \code{fsl_bin} will be run to make these images binary before running
#' 
#' @return table of x vs y 
fsl_bin_tab = function(
  x, y,
  dnames = c("x", "y")) {
  
  bin_x = fsl_bin(x)
  bin_y = fsl_bin(y)
  
  t1 = fslsum(bin_x)
  t2 = fslsum(bin_y)
  tt = fslsum(fsl_mul(
    file = bin_x,
    file2 = bin_y
  ))
  
  tab = matrix(c(length(x) - t1 - t2 + tt,  t1 - tt, t2 - tt, tt), 2, 2)
  n = list(c("FALSE", "TRUE"), c("FALSE", "TRUE"))
  names(n) = dnames
  
  return(tab)
}
