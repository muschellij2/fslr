#' Smoothness Estimation using \code{smoothest}
#'
#'
#' @param file filename of input brain mask
#' @param dof number of degrees of freedom
#' @param residual_image 4d residual image.  If specified, then
#' \code{dof} must be specified.
#' @param z_image z-statistic image.  Cannot be specified if 
#' \code{residual_image} is specified
#' @param opts (character) operations to be passed to \code{smoothest}
#' @param verbose (logical) print out command before running
#' @param ... additional arguments to pass to \code{\link{fslcmd}}
#'
#' @return An output of smoothness estimate
#' @export
#'
#' @examples
#' if (have_fsl()) {
#' file = mni_fname(mm = 2, brain = TRUE, mask = TRUE)
#' img = mni_img(mm = 2, brain = TRUE, mask = FALSE)
#' mask = mni_img(mm = 2, brain = TRUE, mask = TRUE)
#' img = zscore_img(img = img, mask = mask)
#' est = fsl_smoothest(file = file, z_image = img)
#' }
fsl_smoothest = function(
  file,
  residual_image,
  z_image,
  dof = NULL,
  opts = "",  
  verbose = TRUE,
  ...) {
  
  opts = ""  
  # outfile = tempfile() 
  
  
  file = checkimg(file)
  if (!missing(residual_image) && !missing(z_image)) {
    stop(paste0("z_image or residual_image must", 
                " be specified, but not both!"))
  }
  
  if (!missing(residual_image)) {
    if (is.null(dof)) {
      stop("If residual image is specified, dof must be!")
    }
    opts = c(opts, paste0("--dof=", dof))
    residual_image = checkimg(residual_image)
    opts = c(opts, paste0("--res=", shQuote(residual_image)))
  } else {
    z_image = checkimg(z_image)
    opts = c(opts, paste0("--zstat=", shQuote(z_image)))
  }
  
  if (verbose) {
    opts = c(opts, "--verbose")
  }

  res = fslcmd(
    "smoothest", 
    file = file, 
    frontopts = "--mask=",
    trim_front = TRUE,
    outfile = tempfile(), retimg = FALSE,
    no.outfile = TRUE,
    reorient = reorient, intern = TRUE, opts = opts, 
    ... = ..., verbose = verbose)
  res = attributes(res)$result
  return(res)
  
}

