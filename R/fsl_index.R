#' @rdname fslindex
#' @aliases fsl_index
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_index = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslindex(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
