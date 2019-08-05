
#' @title Calculate Dice Coefficient of 2 Binary images
#'
#' @description Creates a 2 by 2 table for 
#' @param x filename of logical or 0/1 image
#' @param y filename of logical or 0/1 image
#' @param ... arguments passed to \code{\link{fsl_bin_tab}}
#' 
#' @return Single number of the dice coefficient
#' @export
#' 
fsl_dice = function(
  x, y, ...) {
  
  tab = fsl_bin_tab(x, y, ...)
  dtab = dim(tab)
  stopifnot(all(dtab == c(2,2)))
  good = 2 * tab[2, 2]
  dice = good/(good + tab[1, 2] + tab[2, 1])
  
  return(dice)
}