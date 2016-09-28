#' @rdname fslrecip
#' @aliases fsl_recip
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_recip = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslrecip(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
