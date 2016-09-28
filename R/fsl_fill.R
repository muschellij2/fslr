#' @rdname fslfill
#' @aliases fsl_fill
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_fill = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslfill(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
