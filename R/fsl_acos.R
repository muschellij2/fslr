#' @rdname fslacos
#' @aliases fsl_acos
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_acos = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslacos(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
