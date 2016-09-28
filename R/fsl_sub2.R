#' @rdname fslsub2
#' @aliases fsl_sub2
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_sub2 = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslsub2(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
