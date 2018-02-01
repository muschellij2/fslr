#' @title topup - calling FSL topup
#' @description A tool for estimating and correcting susceptibility 
#' induced distortions
#' 
#' @param infile name of 4D file with images
#' @param datain name of text file with PE directions/times
#' @param out base-name of output files (spline coefficients (Hz) and 
#' movement parameters)
#' @param fout name of image file with field (Hz)
#' @param iout name of 4D image file with unwarped images
#' @param logout Name of log-file
#' @param warpres (approximate) resolution (in mm) of warp basis for 
#' the different sub-sampling levels, default 10
#' @param subsamp sub-sampling scheme, default 1
#' @param fwhm FWHM (in mm) of gaussian smoothing kernel, default 8
#' @param config Name of config file specifying command line arguments
#' @param miter Max # of non-linear iterations, default 5
#' @param lambda Weight of regularisation, default depending on 
#' \code{ssqlambda} and \code{regmod} switches. See user documentation.
#' @param ssqlambda If set (=1), lambda is weighted by current ssq, default 1
#' @param regmod Model for regularisation of warp-field 
#' [membrane_energy bending_energy], default bending_energy
#' @param estmov Estimate movements if set, default 1 (true)
#' @param minmet Minimisation method 0=Levenberg-Marquardt, 1=Scaled 
#' Conjugate Gradient, default 0 (LM)
#' @param splineorder Order of spline, 2->Qadratic spline, 3->Cubic 
#' spline. Default=3
#' @param numprec Precision for representing Hessian, double or float. 
#' Default double
#' @param interp Image interpolation model, linear or spline. Default spline
#' @param scale If set (=1), the images are individually scaled to a 
#' common mean, default 0 (false)
#' @param regrid If set (=1), the calculations are done in a different 
#' grid, default 1 (true)
#' @param verbose Print diagnostic information while running
topup = function(
  infile, 
  datain, 
  out = NULL, 
  fout = NULL, 
  iout = NULL, 
  logout = NULL, 
  warpres = 10, 
  subsamp = 1, 
  fwhm = 8, 
  config = NULL, 
  miter = 5, 
  lambda = NULL, 
  ssqlambda = 1, 
  regmod = c("bending_energy", "membrane_energy"), 
  estmov = 1, 
  minmet = c(0, 1), 
  splineorder = c(3, 2), 
  numprec = c("double", "float"), 
  interp = c("spline", "linear"), 
  scale = c(0, 1), 
  regrid = c(0, 1), 
  verbose = TRUE ){
  
  regmod = match.arg(regmod)
  minmet = minmet[1]
  splineorder = splineorder[1]
  numprec = match.arg(numprec)
  interp = match.arg(interp)
  scale = scale[1]
  regrid = regrid[1]
  
  
  parse_num_args = function(x){
    x = paste0(names(x), '=', x)
    x = paste(x, collapse = " ")
    x
  }  
  
  # if (is.null(out)) {
  #   out = tempfile()
  # }
  L = list(out = out, 
           fout = fout, 
           iout = iout, 
           logout = logout, 
           warpres = warpres, 
           subsamp = subsamp, 
           fwhm = fwhm, 
           config = config, 
           miter = miter, 
           lambda = lambda, 
           ssqlambda = ssqlambda, 
           regmod = regmod, 
           estmov = estmov, 
           minmet = minmet, 
           splineorder = splineorder, 
           numprec = numprec, 
           interp = interp, 
           scale = scale, 
           regrid = regrid)
  
  nulls = sapply(L, is.null)
  L = L[!nulls]
  names(L) = paste0("--", names(L))
  # print(L)
  
  opts = parse_num_args(L)
  # print(opts)
  if (verbose) {
    opts = paste(opts, "--verbose")
  }
  
  cmd = get.fsl()
  cmd = paste0(cmd, "topup")
  infile = checkimg(infile)
  infile = unname(infile)
  infile = normalizePath(infile)
  
  datain = normalizePath(datain)
  datain = unname(datain)
  
  args = paste0("--infile=", infile, " --datain=", datain)
  cmd = paste(cmd, args, opts)
  
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd)
  return(res)
}

#' @param ... arguments passed to \code{topup} if using \code{fsl_topup}
#' @rdname topup
fsl_topup = function(...){
  res = topup(...)
  return(res)
}