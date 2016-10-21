#' @title Quick Tabulation for logical images
#'
#' @description Creates a 2 by 2 table for 
#' @param x filename of logical or 0/1 image
#' @param y filename of logical or 0/1 vimage
#' @param dnames names for table
#' @param verbose Should fsl commands be printed?
#' @note \code{fsl_bin} will be run to make these images binary before running
#' 
#' @return table of x vs y 
#' @export
fsl_bin_tab = function(
  x, y,
  dnames = c("x", "y"),
  verbose = FALSE) {
  
  dx = dim_(x)[2:4]
  dy = dim_(y)[2:4]
  if (!all(dx == dy)) {
    stop("Dimensions of x and y are not the same!")
  }
  n_vox = prod(dx)
  bin_x = fsl_bin(x, verbose = verbose)
  bin_y = fsl_bin(y, verbose = verbose)
  
  t1 = fslsum(bin_x, verbose = verbose)
  t2 = fslsum(bin_y, verbose = verbose)
  tt = fsl_mul(
    file = bin_x,
    file2 = bin_y, verbose = verbose
  )
  tt = fslsum(tt, verbose = verbose)
  
  tab = matrix(c(n_vox - t1 - t2 + tt,  t1 - tt, t2 - tt, tt), 2, 2)
  n = list(c("FALSE", "TRUE"), c("FALSE", "TRUE"))
  names(n) = dnames
  dimnames(tab) = n
  tab = as.table(tab)  
  
  return(tab)
}
