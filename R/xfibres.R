#' @title Bayesian Estimation of Diffusion Parameters 
#' Obtained using Sampling Techniques with Crossing Fibers
#' @description Calls \code{xfibres} from FSL to fit, also known as
#' \code{bedpostx}
#' @aliases bedpostx
#'
#' @param infile Input filename
#' @param bvecs b-vectors: matrix of 3 columns or 
#' filename of ASCII text file
#' @param bvals b-values: vector of same length as number of rows of 
#' b-vectors or filename of ASCII text file 
#' @param mask Mask filename
#' @param nfibres Maximum number of fibres to fit in each voxel (default 1)
#' @param bet.opts Options for \code{\link{fslbet}} if mask is not supplied
#' @param verbose print diagnostic messages
#' @param njumps num of jumps to be made by MCMC (default is 5000)
#' @param burnin Total num of jumps at start of MCMC to be discarded (default is 0)
#' @param burnin_noard num of burnin jumps before the ard is imposed (default is 0)
#' @param sampleevery num of jumps for each sample (MCMC) (default is 1)
#' @param updateproposalevery num of jumps for each update to the proposal density std (MCMC) (default is 40)
#' @param seed for pseudo random number generator
#' @param noard Turn ARD off on all fibres
#' @param allard Turn ARD on on all fibres
#' @param nospat Initialise with tensor, not spatially
#' @param nonlinear Initialise with nonlinear fitting
#' @param cnonlinear Initialise with constrained nonlinear fitting
#' @param rician Use Rician noise modelling
#' @param f0 Add to the model an unattenuated signal compartment
#' @param ardf0	Use ard on f0 
#' @param opts Additional options for \code{xfibres}.  
#' There should not be any left out in the current arguments, but 
#' \code{opts} may be a way some prefer to input options.
#' @return Output from \code{\link{system}}
#' @export
xfibres = function(infile, 
                   bvecs,
                   bvals,
                   mask = NULL,
                   nfibres = 1,
                   bet.opts = "",
                   verbose = TRUE,
                   njumps = NULL,
                   burnin = NULL,	
                   burnin_noard = NULL,
                   sampleevery	= NULL,
                   updateproposalevery = NULL,
                   seed = NULL,
                   noard = FALSE,
                   allard	= FALSE,
                   nospat	= FALSE,
                   nonlinear = FALSE,
                   cnonlinear	= FALSE,
                   rician	= FALSE,
                   f0	= FALSE,
                   ardf0 = FALSE,
                   opts = ""
) {
  
  infile = checkimg(infile)
  
  ############################
  # If no mask, then run bet (can be 4D)
  ############################  
  if (is.null(mask)) {
    tfile = tempfile(fileext = ".nii.gz")
    bet = fslbet(infile, outfile = tfile,
                 retimg = FALSE, opts = bet.opts)
    mask = tempfile(fileext = ".nii.gz")
    res = fslbin(tfile, retimg = FALSE, outfile = mask)
  }
  mask = checkimg(mask)
  
  parse_args = function(x){
    x = paste0(names(x), '="', x, '"')
    x = paste(x, collapse = " ")
    x
  }
  parse_num_args = function(x){
    x = paste0(names(x), '=', x)
    x = paste(x, collapse = " ")
    x
  }  
  if (is.matrix(bvecs)) {
    stopifnot(ncol(bvecs) == 3)
    tfile = tempfile(fileext = ".txt")
    bvecs = apply(bvecs, 1, paste, collapse = " ")
    writeLines(bvecs, con = tfile)
    bvecs = tfile
  }
  
  if (is.numeric(bvals)) {
    tfile = tempfile(fileext = ".txt")
    bvals = as.character(bvals)
    writeLines(bvals, con = tfile)
    bvals = tfile
  }
  
  vec = c("--data" = infile,
          "--mask" = mask,
          "--bvecs" = bvecs,
          "--bvals" = bvals)
  vec = parse_args(vec)
  
  num_vec = c(
           "--nfibres" = nfibres,
           "--njumps" = njumps,
           "--burnin" = burnin, 
           "--burnin_noard" = burnin_noard, 
           "--sampleevery" = sampleevery, 
           "--updateproposalevery" = updateproposalevery)  
  num_vec = parse_num_args(num_vec)
  vec = paste(vec, num_vec)
  
  if (is.null(seed)) {
    vec = c(vec, "--seed" = seed)
  }
  
  ########################################
  # Logical switches, no =
  ########################################  
  log_vec = c(
    "--noard" = noard, 
    "--allard" = allard, 
    "--nospat" = nospat, 
    "--nonlinear" = nonlinear, 
    "--cnonlinear" = cnonlinear, 
    "--rician" = rician, 
    "--f0" = f0, 
    "--ardf0" = ardf0,
    "--verbose" = verbose)
  nn = names(log_vec)
  log_vec = as.logical(log_vec)
  names(log_vec) = nn
  log_vec = log_vec[ log_vec ]
  nn = names(log_vec)
  if (length(nn) > 0) {
    nn = paste(nn, collapse = " ")
    vec = paste(vec, nn)
  }
  vec = paste0(vec, " ", opts)
  
  cmd = get.fsl()
  ##########################
  # Add frontopts
  ##########################
  s = sprintf('%s %s', "xfibres", vec)
  cmd <- paste0(cmd, s)
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd)
  
  return(res)
}
# Optional arguments (You may optionally specify one or more of):
#   -V,--verbose	switch on diagnostic messages
# -h,--help	display this message
# --ld,--logdir	log directory (default is logdir)
# --forcedir	Use the actual directory name given - i.e. don't add + to make a new directory
# --nf,--nfibres	Maximum number of fibres to fit in each voxel (default 1)
# --model	Which model to use. 1=mono-exponential (default and required for single shell). 2=continous exponential (for multi-shell experiments)
# --fudge	ARD fudge factor
# --nj,--njumps	Num of jumps to be made by MCMC (default is 5000)
# --bi,--burnin	Total num of jumps at start of MCMC to be discarded (default is 0)
# --bn,--burnin_noard	num of burnin jumps before the ard is imposed (default is 0)
# --se,--sampleevery	Num of jumps for each sample (MCMC) (default is 1)
# --upe,--updateproposalevery	Num of jumps for each update to the proposal density std (MCMC) (default is 40)
# --seed	seed for pseudo random number generator
# --noard	Turn ARD off on all fibres
# --allard	Turn ARD on on all fibres
# --nospat	Initialise with tensor, not spatially
# --nonlinear	Initialise with nonlinear fitting
# --cnonlinear	Initialise with constrained nonlinear fitting
# --rician	Use Rician noise modelling
# --f0	Add to the model an unattenuated signal compartment
# --ardf0	Use ard on f0

