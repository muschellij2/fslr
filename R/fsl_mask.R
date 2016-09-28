#' @rdname fslmask
#' @aliases fsl_mask
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_mask = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslmask(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
