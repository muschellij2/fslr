#' @rdname %nounderscore%
#' @aliases %underscore%
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
%underscore% = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  %nounderscore%(..., outfile = outfile, retimg = retimg)
  return(outfile)
}
