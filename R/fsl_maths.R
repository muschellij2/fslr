#' @rdname fslmaths
#' @aliases fsl_maths
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_maths = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslmaths(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
