#' @rdname fslatan
#' @aliases fsl_atan
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_atan = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslatan(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
