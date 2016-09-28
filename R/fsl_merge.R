#' @rdname fslmerge
#' @aliases fsl_merge
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_merge = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslmerge(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
