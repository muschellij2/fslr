#' @rdname fslsqr
#' @aliases fsl_sqr
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_sqr = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslsqr(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
