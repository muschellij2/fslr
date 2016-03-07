#' @title Remake Dropped Dimensions
#' @description This funtion is the reverse of \code{dropEmptyImageDimensions}.
#' If \code{dropEmptyImageDimensions} was run, and the output is a list, 
#' usually if \code{keep_ind = TRUE}, this function reverses that.
#'
#' @param img Object of class \code{nifti} where image dimensions were dropped.
#' @param inds List of length 3 of indices from 
#' \code{\link{dropEmptyImageDimensions}} or \code{\link{getEmptyImageDimensions}}
#' @param orig.dim Original dimension of pre-dropped image. Output image will
#' have dimensions same as this value
#'
#' @return Object of class \code{\link{nifti}}
#' @export
#' @examples \dontrun{
#' # nim is an object of class nifti
#' dd = dropEmptyImageDimensions(nim, keep_ind = TRUE)
#' remake = replace_dropped_dimensions(img = dd$outimg, 
#' inds = dd$inds,
#' orig.dim = dd$orig.dim)
#' all.equal(nim, remake)
#' }
replace_dropped_dimensions = function(img, inds, orig.dim){
  stopifnot(length(orig.dim) == 3)
  arr = array(0, dim = orig.dim)
  arr[inds[[1]], inds[[2]], inds[[3]]] = img
  outimg = copyNIfTIHeader(img = img, 
                           arr = arr, 
                           drop = TRUE)
  return(outimg)
}
