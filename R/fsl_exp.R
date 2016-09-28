#' @rdname fslexp
#' @aliases fsl_exp
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_exp = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslexp(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
