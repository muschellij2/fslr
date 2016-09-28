#' @rdname fsledge
#' @aliases fsl_edge
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_edge = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fsledge(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
