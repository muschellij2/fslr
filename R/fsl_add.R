#' @rdname fsladd
#' @aliases fsl_add
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_add = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fsladd(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
