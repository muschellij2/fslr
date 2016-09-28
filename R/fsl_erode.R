#' @rdname fslerode
#' @aliases fsl_erode
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_erode = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslerode(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
