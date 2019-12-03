
bvecs_file =  function(bvecs) {
  if (is.matrix(bvecs)) {
    stopifnot(ncol(bvecs) == 3)
    tfile = tempfile(fileext = ".txt")
    bvecs = apply(bvecs, 1, paste, collapse = " ")
    writeLines(bvecs, con = tfile)
    bvecs = tfile
  }
  bvecs = unname(bvecs)
  bvecs = path.expand(bvecs)
  return(bvecs)
}

bvals_file = function(bvals) {
  if (is.numeric(bvals)) {
    tfile = tempfile(fileext = ".txt")
    bvals = as.character(bvals)
    writeLines(bvals, con = tfile)
    bvals = tfile
  }
  bvals = unname(bvals)
  bvals = path.expand(bvals)
  return(bvals)
}


#' @title Eddy Current Correction
#' @description This function calls \code{eddy} from FSL for 
#' DTI Processing
#'
#' @param infile input filename of 4D image.
#' @param outfile Output file basename
#' @param retimg (logical) return image of class nifti
#' @param verbose print diagnostic messages
#' 
#' @param ... Not currently used
#' @param mask Mask filename (or class nifti)
#' @param acq_file A text-file describing the acquisition parameters for the 
#' different images in \code{infile} The format of this file is identical 
#' to that used by topup (though the parameter is called \code{--datain} 
#' there).
#' @param topup This should only be specified if you have previously run 
#' `topup` on your data and should be the same name that you gave as an 
#' argument to the --out parameter when you ran topup, aka
#' the base name for output files from topup.
#' @param index_file A text-file that determines the relationship between 
#' on the one hand the images in \code{infile} and on the other hand the 
#' acquisition parameters in \code{acq_file}.
#' @param bvecs A text file with normalised vectors describing 
#' the direction of the diffusion weighting.
#' @param bvals A text file with b-values describing the "amount of" 
#' diffusion weighting
#' @param opts Additional options to pass to arguments 
#' passed to \code{\link{eddy}}
#' @param eddy_cmd The version of \code{eddy} to run.
#'
#' @return Result from system command currently
#' @export
eddy = function(
  infile, 
  mask,
  acq_file,
  index_file,
  bvecs, 
  bvals,
  topup = NULL,
  outfile = NULL,
  retimg = TRUE,  
  opts = "",
  verbose = TRUE,
  eddy_cmd = c("eddy", "eddy_openmp", "eddy_cuda"),
  ...) {
  
  eddy_cmd = match.arg(eddy_cmd)
  # interp = c("spline", "trilinear"),
  
  # interp = match.arg(interp)
  # interp = unname(interp)
  
  
  infile = checkimg(infile)
  infile = unname(infile)
  infile = path.expand(infile)
  
  mask = checkimg(mask)
  mask = path.expand(mask)
  mask = unname(mask)
  
  acq_file = unname(acq_file)
  acq_file = path.expand(acq_file)
  
  index_file = unname(index_file)
  index_file = path.expand(index_file)
  
  bvecs = bvecs_file(bvecs)
  bvals = bvals_file(bvals)
  
  parse_args = function(x){
    x = paste0(names(x), '="', x, '"')
    x = paste(x, collapse = " ")
    x
  }

  parse_log_args = function(x) {
    nn = names(x)
    x = as.logical(x)
    names(x) = nn
    x = x[ x ]
    nn = names(x)
    return(nn)
  }
  
  
  outfile = check_outfile(
    outfile = outfile, 
    retimg = retimg, 
    fileext = "")
  
  outfile = nii.stub(outfile)
  outfile = unname(outfile)
  
  vec = c("--imain" = infile,
          "--mask" = mask,
          "--bvecs" = bvecs,
          "--index" = index_file,
          "--bvals" = bvals,
          "--acqp" = acq_file,
          "--out" = outfile)
  if (!is.null(topup)) {
    vec = c(vec, "--topup" = topup)
  }
  vec = parse_args(vec)
  
  verbose = unname(verbose)
  log_vec = c("--verbose" = verbose)
  log_vec = parse_log_args(log_vec)
  if (length(log_vec) > 0) {
    log_vec = paste(log_vec, collapse = " ")
    vec = paste(vec, log_vec)
  }
  vec = paste0(vec, " ", opts)
  
  
  cmd = get.fsl()
  ##########################
  # Add frontopts
  ##########################
  s = sprintf('%s %s', eddy_cmd, vec)
  cmd <- paste0(cmd, s)
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd)
  return(res)   
}




