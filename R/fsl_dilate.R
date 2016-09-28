#' @rdname fsldilate
#' @aliases fsl_dilate
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_dilate = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fsldilate(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
