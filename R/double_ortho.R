#' @title Double Orthographic Display
#' @description Copy of \code{oro.nifti}'s \code{\link{orthographic}} function 
#' with some tweaks such as adding L/R designations for left and right
#' @return NULL
#' @seealso \link{orthographic}
#' @param x is an object of class nifti or similar.
#' @param y is an object of class nifti or similar to be set aside x.
#' @param col.y is grayscale (by default).
#' @param mfrow (numeric) layout of the 3 slices
#' @param add Should the y-plot be added or its own plot?  Used
#' in \code{double_ortho} 
#' @param ... other arguments to \code{\link{ortho2}}
#' @export
double_ortho = function (x, y = NULL, col.y = gray(0:64/64), 
                         mfrow=c(2,4), add = FALSE, 
                   ...) 
{
  ortho2(x=x,  y=y, col.y = col.y, 
         add = add, mfrow= mfrow, ...)
}

