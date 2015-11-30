#' @title Remake Dropped Dimensions
#' @description This funtion is the reverse of \code{dropEmptyImageDimensions}.
#' If \code{dropEmptyImageDimensions} was run, and the output is a list, 
#' usually if \code{keep_ind = TRUE}, this function reverses that.
#'
#' @param img Object of class \code{nifti} where image dimensions were dropped.
#' @param inds List of length 3 of indices from 
#' \code{\link{dropEmptyImageDimensions}}
#' @param orig.dim Original dimension of pre-dropped image. Output image will
#' have dimensions same as this
#'
#' @return Object of class nifti
#' @export
replace_dropped_dimensions = function(img, inds, orig.dim){
  stopifnot(length(orig.dim) == 3)
  arr = array(0, dim = orig.dim)
  arr[inds[[1]], inds[[2]], inds[[3]]] = img
  outimg = copyNIfTIHeader(img = img, 
                           arr = arr, 
                           drop = TRUE)
  return(outimg)
}
