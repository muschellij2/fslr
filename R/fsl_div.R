#' @rdname fsldiv
#' @aliases fsl_div
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_div = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fsldiv(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
