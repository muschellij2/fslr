#' @rdname fslrem
#' @aliases fsl_rem
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_rem = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslrem(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
