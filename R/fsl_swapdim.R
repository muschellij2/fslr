#' @rdname fslswapdim
#' @aliases fsl_swapdim
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_swapdim = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslswapdim(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
