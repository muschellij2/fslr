#' @title Image Mean Entropy
#' @description Estimates Mean Entropy of Image from FSL
#' @param img Object of class nifti, or path of file
#' @param nonzero (logical) Should the statistic be taken over non-zero voxels
#' @param verbose (logical) print out command before running 
#' @param ts (logical) is the series a timeseries (4D), invoking \code{-t} 
#' option  
#' @note This uses option -e or -E in \code{\link{fslstats}}
#' @return Vector of unless ts option invoked, then matrix
#' @export
fslentropy = function(img, nonzero = FALSE, verbose = TRUE, ts = FALSE){
  opts = "-e"
  opts = ifelse(nonzero, toupper(opts), opts)
  
  val = fslstats(img, opts = opts, verbose = verbose, ts = ts)
  val = strsplit(val, " ")
  if (length(val) == 1) {
    val = as.numeric(val[[1]])
  } else {
    val = sapply(val, as.numeric)
  }
  val
}
