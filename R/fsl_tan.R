#' @rdname fsltan
#' @aliases fsl_tan
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_tan = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fsltan(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
