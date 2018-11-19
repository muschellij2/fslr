#' Form clusters, report information about clusters 
#' and/or perform cluster-based inference.  Wrapper for \code{cluster}
#'
#' @param file filename of input volume
#' @param threshold threshold for input volume
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param opts (character) operations to be passed to \code{cluster}  
#' @param cope_image filename of input cope volume
#' @param pthresh p-threshold
#' @param peakdist minimum distance between local maxima/minima, in mm (default 0)
#' @param volume number of voxels in the mask
#' @param smooth_est smoothness estimate = sqrt(det(Lambda))
#' @param voxel_resel Size of one resel in voxel units
#' @param fractional interprets the threshold as a fraction of the robust range
#' @param connectivity the connectivity of voxels (default 26)
#' @param mm use mm, not voxel, coordinates
#' @param find_minima find minima instead of maxima
#' @param verbose (logical) print out command before running
#' @param ... additional arguments to pass to \code{\link{fslcmd}}
#' @param standard_image filename for standard-space volume
#'
#' @return A list of filenames of outputs and tables:
#' \itemize{
#' \item{\code{opvals}}{filename for image output of log pvals}
#' \item{\code{oindex}}{filename for output of cluster index (in size order)}
#' \item{\code{othresh}}{filename for output of thresholded image}
#' \item{\code{olmax}}{filename for output of local maxima text file}
#' \item{\code{olmaxim}}{filename for output of local maxima volume}
#' \item{\code{osize}}{filename for output of size image}
#' \item{\code{omax}}{filename for output of max image}
#' \item{\code{omean}}{filename for output of mean image}
#' }
#' 
#' @export
#'
#' @examples
#' if (have_fsl()) { 
#' file = mni_fname(brain = TRUE, mask = FALSE)
#' threshold = 6000
#' clus = fsl_cluster(file, threshold)
#' }
fsl_cluster = function(
  file,
  threshold,
  retimg = FALSE,
  reorient = FALSE,
  opts = "",
  cope_image = NULL,
  pthresh = NULL,
  peakdist = 0,
  volume = FALSE,
  smooth_est = NULL,
  voxel_resel = NULL,
  fractional = FALSE,
  connectivity = 26,
  mm = FALSE,
  find_minima = FALSE,
  standard_image = NULL,
  verbose = TRUE,
  ...) {
  
  opts = c(opts, "--minclustersize")
  threshold = paste0("--thresh=", threshold)
  
  ext = get.imgext()
  
  
  if (!is.null(cope_image)) {
    cope_image = checkimg(cope_image)
    cope_image = paste0("--cope=", cope_image)
    opts = c(cope_image, opts)
  }
  
  if (peakdist != 0) {
    peakdist = paste0("--peakdist=", peakdist)
    opts = c(peakdist, opts)    
  }
  
  if (connectivity != 26) {
    connectivity = paste0("--connectivity=", connectivity)
    opts = c(connectivity, opts) 
  }
  
  if (!is.null(standard_image)) {
    standard_image = paste0("--stdvol=", standard_image)
    opts = c(standard_image, opts) 
  }
  
  if (fractional) {
    opts = c("--fractional", opts)
  }    
  
  if (!is.null(pthresh)) {
    pthresh = paste0("--pthresh=", pthresh)
    opts = c(pthresh, opts)
  }  
  
  if (!is.null(smooth_est)) {
    smooth_est = paste0("--dlh=", smooth_est)
    opts = c(smooth_est, opts)
  }  
  
  if (!is.null(voxel_resel)) {
    voxel_resel = paste0("--resels=", voxel_resel)
    opts = c(voxel_resel, opts)
  }    
  
  if (volume) {
    opts = c("--volume", opts)
  }     
  
  if (mm) {
    opts = c("--mm", opts)
  }     
  
  if (find_minima) {
    opts = c("--min", opts)
  }
  
  if (verbose) {
    opts = c(opts, "--verbose")
  }
  
  func = "cluster"
  
  
  ####################################
  # All the outputs
  ####################################
  output_images = c(
    "oindex" = TRUE, 
    "othresh" = TRUE, 
    "olmax" = FALSE, 
    "olmaxim" = TRUE, 
    "osize" = TRUE, 
    "omax" = TRUE, 
    "omean" = TRUE)
  outputs = lapply(output_images, function(x) {
    tempfile(fileext = ifelse(x, ext, ""))
  })
  output_opts = mapply(function(x, y) {
    paste0("--", x, "=", y)
  }, names(outputs), outputs, SIMPLIFY = TRUE)
  output_opts = paste(output_opts, collapse = " ")
  
  
  opts = c(output_opts, opts)
  opts = c(threshold, opts)
  
  outfile = tempfile()
  opts = c(opts, paste0("> ", outfile))
  opts = paste(opts, collapse = " ")
  
  res = fslcmd(
    func, 
    file = file, 
    frontopts = "--in=",
    trim_front = TRUE,
    outfile = NULL, retimg = FALSE,
    reorient = reorient, intern = FALSE, opts = opts, 
    ... = ..., verbose = verbose, no.outfile = TRUE, samefile = TRUE)
  fe = sapply(outputs, file.exists)
  if (retimg) {
    outputs[output_images] = lapply(
      outputs[output_images],
      readnii, reorient = reorient)
  }  
  L = outputs
  result = attributes(res)$result


  # L$olmax = table_read(L$olmax)
  
  if (result != 0) {
    warning("Result was not 0!  May not work as expected!")
  }
  if (!all(fe)) {
    warning("Not all output files exist!")
    
  }
  attr(outfile, "result") = result
  L$outfile = outfile
  # if retimg stuff here
  return(L)
  
}


# @export
# @rdname fsl_cluster
# cluster = fsl_cluster

#' @export
#' @rdname fsl_cluster
fslcluster = function(..., retimg = TRUE) {
  res = fsl_cluster(..., retimg = retimg)
  return(res)
}

#' @export
#' @rdname fsl_cluster
read_cluster_table = function(file) {
  x = do.call("rbind", strsplit(readLines(file), "\t"))
  colnames(x) = x[1,]
  x = x[-1,]
  class(x) = "numeric"
  x = as.data.frame(x, stringsAsFactors = FALSE)
  return(x)
}