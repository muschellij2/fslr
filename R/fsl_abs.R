#' @rdname fslabs
#' @aliases fsl_abs
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_abs = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslabs(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
