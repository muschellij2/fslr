#' @title Replace non-finite values 
#' @description Replaces non-finite values of nifti image with a replacement
#' @return object of class nifti
#' @param x is either a character name for the image or an object of class nifti
#' @param replace Value to replace non-finite values
#' @export
finite_img = function(x, replace = 0) {
  x = check_nifti(x)
  x[ !is.finite(x) ] = replace
  return(x)
}