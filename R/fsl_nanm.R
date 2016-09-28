#' @rdname fslnanm
#' @aliases fsl_nanm
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_nanm = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslnanm(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
