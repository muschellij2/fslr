#' @rdname fsllog
#' @aliases fsl_log
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_log = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fsllog(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
