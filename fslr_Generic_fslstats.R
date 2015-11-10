#' @title Image %FUNC%
#' @description Estimates %FUNC% of Image from FSL
#' @param img Object of class nifti, or path of file
#' @param verbose (logical) print out command before running 
#' @param ts (logical) is the series a timeseries (4D), invoking \code{-t} 
#' option  
#' @note This uses option -%opt% in \code{\link{fslstats}}
#' @return Vector of length 1 unless ts option invoked, then vector of with length the number of timepoints.
#' @export
fsl%func% = function(img, verbose = TRUE, ts = FALSE){
  opts = "-%opt%"
  
  val = fslstats(img, opts = opts, verbose = verbose, ts = ts)
  val = strsplit(val, " ")
  if (length(val) == 1) {
    val = as.numeric(val[[1]])
  } else {
    val = t(sapply(val, as.numeric))
  }
  val
}
