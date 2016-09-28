#' @rdname fslsub
#' @aliases fsl_sub
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_sub = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslsub(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
