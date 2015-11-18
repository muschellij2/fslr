#' @title Find Image z-limits
#' @description Helper function for plotting - returns zlim for \code{\link[graphics]{image}} 
#' plot function
#' @param x Object of class \code{nifti}
#' @param zlim A user-specified \code{zlim}.  If \code{NULL}, will calculate how 
#' \code{\link{ortho2}} would calculate \code{zlim}
#' @return If \code{zlim = NULL}, then vector of length 2, otherwise returns \code{zlim}
#' @export
zlimmer = function(x, zlim = NULL){
  if (is.null(x)){
    return(NULL)
  }
  x = check_nifti(x)
  if (is.null(zlim)) {
    zlim <- c(x@cal_min, x@cal_max)
    if (any(!is.finite(zlim)) || diff(zlim) == 0) {
      zlim <- c(x@glmin, x@glmax)
    }
    if (any(!is.finite(zlim)) || diff(zlim) == 0) {
      zlim <- range(x, na.rm = TRUE)
    }
  }
  return(zlim)
}