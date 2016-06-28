#' @title Remake Image from Vector
#' @description Wrapper function to take a vector of values and result in a 
#' \code{\link{nifti}} object
#' @param vec vector of values to be in resulting image
#' @param img object of class \code{\link{nifti}} to put vector into
#' @param mask binary array/ \code{\link{nifti}} object to denote where
#' vector values are to be.
#' @seealso \code{\link{niftiarr}}
#' @return object of class  \code{\link{nifti}}
#' @export
remake_img = function(vec, img, mask = NULL){
  if (is.null(mask)) {
    mask = array(1, dim = dim(img))
  }
  img2 = niftiarr(img, 0)
  img2[mask == 1] = vec
  img2 = datatyper(img2)
  img2 = cal_img(img2)
  return(img2)
}
