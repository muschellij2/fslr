#' @title nifti image windower
#' @description Windows an image to min and max values and also changes 
#' cal_max and cal_min parameters
#' @return object of class nifti
#' @seealso \link{readNIfTI}
#' @param x is either a character name for the image or an object of class nifti
#' @param window numeric of length 2 that gives min and max for window
#' @param replace either "window" if the any values outside of c(min, max) are
#' set to the min or max or "missing" for these to be set to NA
#' @export
window_img = function(x, window=c(0, 100), 
                      replace = c("window", "missing")) {
  if (inherits(x, "character")) {
    x= readNIfTI(x, reorient=FALSE)
  }
  if (is.null(window)){
    return(x)
  }
  stopifnot(length(window) == 2)
  x@cal_min = window[1]
  x@cal_max = window[2]
  repper = replace[1]
  
  x[ x < window[1] ] = 
    ifelse(repper == "window", window[1], NA)
  x[ x > window[2] ] = 
    ifelse(repper == "window", window[2], NA)    
  return(x)
}