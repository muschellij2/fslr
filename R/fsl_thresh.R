#' @rdname fslthresh
#' @aliases fsl_thresh
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_thresh = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslthresh(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
