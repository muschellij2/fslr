#' @title DTI Fitting Procedure from FSL
#' @description Calls \code{dtifit} from FSL
#'
#' @param infile Input filename
#' @param bvecs b-vectors: matrix of 3 columns or 
#' filename of ASCII text file
#' @param bvals b-values: vector of same length as number of rows of b-vectors
#' or filename of ASCII text file 
#' @param mask Mask filename
#' @param outprefix Output prefix
#' @param opts Additional options for \code{dtifit}
#' @param bet.opts Options for \code{\link{fslbet}} if mask is not supplied
#' @param verbose print diagnostic messages
#' @param sse Save sum of squared errors
#' @param save_tensor Save tensor file out
#' @note On successful completion of the command, the following files
#' will be output, which are:
#' \code{mask} - the mask used in the analysis
#'\code{outprefix}_V1 - 1st eigenvector
#'\code{outprefix}_V2 - 2nd eigenvector
#'\code{outprefix}_V3 - 3rd eigenvector
#'\code{outprefix}_L1 - 1st eigenvalue
#'\code{outprefix}_L2 - 2nd eigenvalue
#'\code{outprefix}_L3 - 3rd eigenvalue
#'\code{outprefix}_MD - mean diffusivity
#'\code{outprefix}_FA - fractional anisotropy
#'\code{outprefix}_MO - mode of the anisotropy (oblate ~ -1; isotropic ~ 0; prolate ~ 1)
#'\code{outprefix}_S0 - raw T2 signal with no diffusion weighting
#' optional output
#' If \code{sse = TRUE}, then the additional file will be present:
#' \code{outprefix}_sse - Sum of squared error
#' If \code{save_tensor = TRUE}, then the additional file will be present:
#' \code{outprefix}_tensor - tensor as a 4D file in this order: Dxx,Dxy,Dxz,Dyy,Dyz,Dzz
#' @return Vector of character filenames of output.  See Note
#' @export
dtifit = function(infile, 
                  bvecs,
                  bvals,
                  mask = NULL,
                  outprefix = NULL,
                  opts = "",
                  bet.opts = "",
                  verbose = TRUE,
                  sse = FALSE,
                  save_tensor = FALSE) {
  infile = checkimg(infile)
  
  if (is.null(outprefix)) {
    outprefix = tempfile()
  }
  outprefix = nii.stub(outprefix)
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
          "--out" = outprefix,
          "--mask" = mask,
          "--bvecs" = bvecs,
          "--bvals" = bvals)
  vec = parse_args(vec)
  
  if (verbose) {
    vec = paste(vec, "--verbose")
  }
  if (sse) {
    vec = paste(vec, "--sse")
  }
  if (save_tensor) {
    vec = paste(vec, "--save_tensor")
  }  
  vec = paste0(vec, " ", opts)
  
  cmd = get.fsl()
  ##########################
  # Add frontopts
  ##########################
  s = sprintf('%s %s', "dtifit", vec)
  cmd <- paste0(cmd, s)
  if (verbose) {
    message(cmd, "\n")
  }
  system(cmd)
  suffixes = c(paste0("V", 1:3),
               paste0("L", 1:3),
               "MD", "FA", "MO", "S0")
  
  if (sse) {
    suffixes = c(suffixes, "sse")
  }
  if (save_tensor) {
    suffixes = c(suffixes, "tensor")
  }  
  
  outfile = paste0(outprefix, "_", suffixes)
  ext = get.imgext()
  outfile = paste0(outfile, ext)    
  names(outfile) = suffixes
  outfile = c(outfile, mask = mask)
  return(outfile)
}
#  
# Optional arguments (You may optionally specify one or more of):
#   -V,--verbose	switch on diagnostic messages
# -h,--help	display this message
# --cni	Input confound regressors
# --sse	Output sum of squared errors
# -w,--wls	Fit the tensor with weighted least squares
# --littlebit	Only process small area of brain
# --save_tensor	Save the elements of the tensor
# -z,--zmin	min z
# -Z,--zmax	max z
# -y,--ymin	min y
# -Y,--ymax	max y
# -x,--xmin	min x
# -X,--xmax	max x

