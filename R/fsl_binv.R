#' @rdname fslbinv
#' @aliases fsl_binv
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_binv = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslbinv(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
