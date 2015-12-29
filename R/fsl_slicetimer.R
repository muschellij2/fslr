#' @title FSL Slice Timing Correction
#' @description This function calls \code{slicetimer} and performs
#' slice timing correction for fMRI data
#' @param file (character) image to be manipulated
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param tr (numeric) Repeat time in seconds
#' @param direction (character) Direction of acquisition
#' @param indexing (character) Whether indexing was bottom up (default) or
#' down using \code{--down} option
#' @param acq_order (character) Order of acquisition, either contiguous 
#' or interleaved
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fsl_slicetimer = function(
  file,
  outfile = NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  tr = 3,
  direction = "z",
  indexing = c("up", "down"),
  acq_order = c("contiguous", "interleaved"),
  verbose = TRUE,
  ...){
  
  opts = ""
  direction = match.arg(direction, c("x", "y", "z"))
  direction = switch(direction, 
                     x = 1,
                     y = 2,
                     z = 3)
  acq_order = match.arg(acq_order)
  acq_order = switch(acq_order,
                     contiguous = "",
                     interleaved = "--odd")
  
  indexing = match.arg(indexing)
  indexing = switch(indexing,
                    up = "",
                    down = "--down")
  
  if (verbose) {
    opts = paste(opts, "-v")
  }
  
  opts = paste(opts, paste0("-r ", tr), 
               paste0("-d ", direction),
               acq_order,
               indexing)
  # need to do this for slicetimer syntax
  opts = paste(opts, "-o ")  
  opts = gsub("\\s+", " ", opts)
  
  opts = trimws(opts)
  
  res = fslcmd(func = "slicetimer -i", 
               file = file,
               outfile = outfile,
               retimg = retimg,
               reorient = reorient,
               intern = intern,
               opts = opts,
               verbose = verbose,
               ... = ..., 
               samefile = FALSE)
  
  return(res)  
}
