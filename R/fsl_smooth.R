#' @rdname fslsmooth
#' @aliases fsl_smooth
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_smooth = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslsmooth(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
