#' @rdname fslmul
#' @aliases fsl_mul
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_mul = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslmul(..., outfile = outfile, retimg = retimg)
  return(outfile)
}

