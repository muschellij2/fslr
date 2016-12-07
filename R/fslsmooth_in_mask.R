#' @rdname fslsmooth_in_mask
#' @title Smooth Image Within a Mask Only
#' @description This function smooth an image within a mask and replaces the values of the
#' original image with the smoothed values.
#' @param file (character) image to be smoothed
#' @param sigma (numeric) sigma (in mm) of Gaussian kernel for smoothing
#' @param mask (character) optional mask given for image
#' @param ... additional arguments passed to \code{\link{fslsmooth}}.
#' @return Object of class \code{nifti}
#' @examples
#' if (have.fsl()){
#' system.time({
#' dims = c(50, 50, 20)
#' x = array(rnorm(prod(dims)), dim = dims)
#' img = nifti(x, dim= dims, 
#' datatype = convert.datatype()$FLOAT32, cal.min = min(x), 
#' cal.max = max(x), pixdim = rep(1, 4))
#' mask = abs(img ) > 1
#' s.img = fslsmooth_in_mask(img, mask = mask)
#' })
#' }
#' @export
fslsmooth_in_mask <- function(
  file,
  sigma = 10, 
  mask = NULL, 
  ...){
  
  if (is.null(mask)) {
    stop("Mask must be specified for fslsmooth_in_mask!")
  }
  res = fslsmooth(
    file = file,
    sigma = sigma,
    mask = mask,
    ...)
  mask = check_nifti(mask)
  file = check_nifti(file)
  check_mask_fail(mask)
  ind = mask %in% 1
  file[ ind ] = res[ ind ]
  file = cal_img(file)
  args = list(...)
  outfile = args$outfile  
  if (!is.null(outfile)) {
    writenii(file, filename = outfile)
  }  
  return(file)
}

#' @rdname fslsmooth_in_mask
#' @export
fsl_smooth_in_mask = function(...) {
  res = fslsmooth_in_mask(...)
  args = list(...)
  retimg = args$retimg
  if (!is.null(retimg)) {
    if (retimg) {
      return(res)
    }
  }
  outfile = args$outfile
  if (is.null(outfile)) {
    outfile = tempfile(fileext = ".nii.gz")
  }
  writenii(res, filename = outfile)
  return(outfile)
}
