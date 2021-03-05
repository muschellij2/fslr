#' @title Find FSL Version
#' @description Finds the FSL version from \code{FSLDIR/etc/fslversion}
#' 
#' @param full provide the full version, versus the numeric version
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
fsl_version = function(full = FALSE){
  
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
  if (!full) {
    version = strsplit(version, ":")[[1]]
    version = version[1]
  }
  return(version)
}

#' @rdname fsl_version
#' @aliases fslversion
#' @export
fslversion = function(){
  return(fsl_version())
}

#' @rdname fsl_version
#' @export
fsl_version_gt5 = function() {
  package_version(fsl_version()) >= package_version("6.0.0")
}
