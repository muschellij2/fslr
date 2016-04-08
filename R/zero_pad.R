#' @title Zero pads an image
#' @description This function zero pads an image by a certain number of 
#' dimensions, usually for convolution
#' @param img Array or class nifti
#' @param kdim Dimensions of kernel
#' @param invert (logical) If \code{FALSE}, does zero padding.  If \code{TRUE}, 
#' reverses the process.
#' @param ... Options to \code{\link{copyNIfTIHeader}}
#' @return Object of class nifti
#' @export
#' @examples
#' kdim = c(3,3,5)
#' img = array(rnorm(30*30*36), dim = c(30, 30, 36))
#' pad = zero_pad(img, kdim)
#' back = zero_pad(pad, kdim, invert=TRUE)
#' all.equal(back, img)
zero_pad = function(img, 
                    kdim, 
                    invert = FALSE, ...){
  
  dimg = dim(img)
  #   img[1:dimg[1], 1:dimg[2], c(1:7, 36:29)] = 0
  #   img[1:dimg[1], c(1:7, 512:505), 1:dimg[3]] = 0
  #   img[c(1:7, 512:505), 1:dimg[2], 1:dimg[3]] = 0
  stopifnot(length(dimg) == length(kdim))
  stopifnot(all(kdim > 0))
  adder = 1
  if (invert) adder = -1
  newdim = dimg + adder * kdim*2
  inds = list()
  idim = 1
  for (idim in seq_along(dimg)){
    x = kdim[idim]
    top = newdim[idim]
    if (invert) top = dimg[idim]
    ind = seq(x+1, top - x)
    #     stopifnot(length(ind) == dz)    
    inds[[idim]] = ind
  }
  ### need to rev for correct indices
  inds = as.matrix(expand.grid(inds))
  if (invert){
    arr = array(img[inds, drop=FALSE], dim = newdim)
  } else {
    arr = array( 0 , dim = newdim)    
    arr[inds] = img
  }
  if (is.nifti(img)) {
    arr = copyNIfTIHeader(img, arr, ...)
  }
  
  return(arr)
}


