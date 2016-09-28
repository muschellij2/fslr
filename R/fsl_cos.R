#' @rdname fslcos
#' @aliases fsl_cos
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_cos = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslcos(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
