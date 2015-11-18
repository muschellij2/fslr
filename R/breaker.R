#' @title Find Breaks for nifti Image Ploting
#' @description Helper function for plotting - returns breaks for \code{\link[graphics]{image}} 
#' plot function for object of class \code{nifti}
#' @param x Object of class \code{nifti}
#' @param zlim A user-specified \code{zlim}.  If \code{NULL}, will calculate how 
#' \code{\link{ortho2}} would calculate \code{zlim}
#' @param col colors to be plotted.  Only used for \code{length(col)}, so can be a
#' vector of length cols to be plotted
#' @param breaks if \code{!is.null(breaks)}, then will calculate breaks.  Otherwise will
#' return this breaks vector
#' @return Vector of length 2
#' @return If \code{breaks = NULL}, then vector of \code{length(col) + 1}, 
#' otherwise returns \code{breaks}
#' @export
breaker = function(x, zlim, col, breaks = NULL){
  if (is.null(x)){
    return(NULL)
  }
  x = check_nifti(x)
  if (is.null(zlim)){
    zlim = zlimmer(x, zlim)
  }
  if (is.null(breaks)){
    breaks <- c(min(x, zlim, na.rm = TRUE), 
                seq(min(zlim, na.rm = TRUE),
                    max(zlim, na.rm = TRUE), 
                    length = length(col) - 1),
                max(x, zlim, na.rm = TRUE))
  }
  return(breaks)
}