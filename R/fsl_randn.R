#' @rdname fslrandn
#' @aliases fsl_randn
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_randn = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslrandn(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
