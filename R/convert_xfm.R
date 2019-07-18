#' Convert a Transformation
#'
#' @param inmat input matrix transformation
#' @param omat output matrix transformation
#' @param verbose print diagnostic messages
#'
#' @return A filename of the output matrix file
#' @export
#'
#' @examples
#' if (have_fsl()) {
#' img = mni_fname()
#' mat = fslreorient2std_mat(img)
#' inverted = invert_xfm(mat)
#' readLines(inverted)
#' catted = concat_xfm(mat, mat)
#' readLines(catted)
#' fixed = fixscaleskew_xfm(mat, mat)
#' readLines(fixed)
#' 
#' }
invert_xfm = function(inmat, omat = tempfile(fileext = ".mat"),
                       verbose = TRUE) {
  frontopts = paste0("-omat ", omat, " -inverse")
  res = fslcmd("convert_xfm", 
         frontopts = frontopts,
         file = inmat,
         retimg = FALSE, 
         no.outfile = TRUE,
         samefile = TRUE,
         verbose = verbose)
  attr(omat, "result") = attr(res, "result")
  return(omat)
}


#' @param inmat2 second matrix filename to be concatenated or fixscaleskew
#'  to first
#' @export
#' @rdname invert_xfm
concat_xfm = function(inmat, inmat2, omat = tempfile(fileext = ".mat"),
                      verbose = TRUE) {
  frontopts = paste0("-omat ", omat, " -concat ", inmat2)
  res = fslcmd("convert_xfm", 
               frontopts = frontopts,
               file = inmat,
               retimg = FALSE, 
               no.outfile = TRUE,
               samefile = TRUE,
               verbose = verbose)
  attr(omat, "result") = attr(res, "result")
  return(omat)
}

#' @export
#' @rdname invert_xfm
fixscaleskew_xfm = function(inmat, inmat2, omat = tempfile(fileext = ".mat"),
                      verbose = TRUE) {
  frontopts = paste0("-omat ", omat, " -fixscaleskew ", inmat2)
  res = fslcmd("convert_xfm", 
               frontopts = frontopts,
               file = inmat,
               retimg = FALSE, 
               no.outfile = TRUE,
               samefile = TRUE,
               verbose = verbose)
  attr(omat, "result") = attr(res, "result")
  return(omat)
}  