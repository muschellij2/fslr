#' @title Remake Image from Vector
#' @description Wrapper function to take a vector of values and result in a 
#' \code{\link{nifti}} object
#' @param vec vector of values to be in resulting image
#' @param img object of class \code{\link{nifti}} to put vector into
#' @param mask binary array/ \code{\link{nifti}} object to denote where
#' vector values are to be.
#' @param warn Should a warning be issued if defaulting to FLOAT32?
#' @param ... additional arguments passed to \code{\link{datatyper}}
#' @seealso \code{\link{niftiarr}}
#' @return object of class  \code{\link{nifti}}
#' @export
remake_img = function(vec, img, mask = NULL, warn = FALSE, ...){
  if (is.null(mask)) {
    mask = array(1, dim = dim(img))
  }
  img2 = niftiarr(img, 0)
  img2[mask == 1] = vec
  img2 = datatyper(img2, warn = warn, ...)
  img2 = cal_img(img2)
  return(img2)
}
