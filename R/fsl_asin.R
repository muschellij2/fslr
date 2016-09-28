#' @rdname fslasin
#' @aliases fsl_asin
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_asin = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslasin(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
