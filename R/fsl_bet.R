#' @rdname fslbet
#' @aliases fsl_bet
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_bet = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslbet(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
