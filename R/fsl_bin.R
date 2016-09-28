#' @rdname fslbin
#' @aliases fsl_bin
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_bin = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslbin(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
