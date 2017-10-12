#' @title Find FSL Version
#' @description Finds the FSL version from \code{FSLDIR/etc/fslversion}
#' 
#' @note This will use \code{fsldir()} to get the directory
#' @return If the version file does not exist, it will throw a warning, but 
#' it will return an empty string.  Otherwise it will be a string of the version.
#' @export
#' @examples 
#' if (have_fsl()) {
#'  fslversion()
#'  fsl_version()
#' }
fsl_version = function(){

  fsldir = fsldir()
  version_file = file.path(fsldir, "etc", "fslversion")
  if (!file.exists(version_file)) {
    warning("No version file exists, fun fsl to see version")
    version = ""
  } else {
    suppressWarnings({
      version = readLines(version_file)
    })
  }
  return(version)
}

#' @rdname fsl_version
#' @aliases fslversion
#' @export
fslversion = function(){
  return(fsl_version())
}
