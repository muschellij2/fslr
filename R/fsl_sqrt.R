#' @rdname fslsqrt
#' @aliases fsl_sqrt
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_sqrt = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslsqrt(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
