#' @rdname fslnan
#' @aliases fsl_nan
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_nan = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslnan(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
