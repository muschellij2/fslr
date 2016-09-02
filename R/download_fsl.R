#' @title Download FSL
#' @description Download FSL Tarball
#' @param os Operating system
#' @param outdir Output directory for tarball
#' @param overwrite If \code{file.path(outdir, tarball_name)} exists,
#' should it be overwritten?
#' @param ... Arguments to pass to \code{\link{download.file}}
#' @return Filename of destination file
#' @importFrom utils browseURL download.file
download_fsl = function(
  os = c("macosx", "redhat5", "redhat6", "centos5", "centos6", "debian", "ubuntu"),
  outdir = tempdir(),
  overwrite = TRUE,
  ...
) {
  
  os = match.arg(os)
  if (os %in% c("debian", "ubuntu")) {
    message("Please install using neurodebian");
    browseURL("http://neuro.debian.net/pkgs/fsl.html")
  }
  os = sub("redhat", "centos", os)
  stub = "http://fsl.fmrib.ox.ac.uk/fsldownloads/fsl-"
  version = readLines("http://fsl.fmrib.ox.ac.uk/fsldownloads/latest-version.txt") 
  app = ".tar.gz"
  if (grepl("centos", os)) {
    app = "_64.tar.gz"
  }
  html = paste0(stub, version, "-", os, app)
  destfile = file.path(outdir, basename(html))
  if (file.exists(destfile) & !overwrite) {
    return(destfile)
  }
  res = download.file(url = html, destfile = destfile, ...)
  return(destfile)
}