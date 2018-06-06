#' Read FSL Transformation
#'
#' @param file transformation file from \code{\link{flirt}},
#' usually ending in `.mat`
#'
#' @return A numeric matrix of numeric class
#' @export
read_xfm = function(file) {
  mat = readLines(file)
  mat = trimws(mat)
  mat = gsub("\\s+", " ", trimws(mat))
  mat = strsplit(mat, split = " ")
  mat = do.call("rbind", mat)
  class(mat) = "numeric"
  return(mat)
}

fsl_read_xfm = read_xfm