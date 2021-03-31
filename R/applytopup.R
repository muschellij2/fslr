#' @title applytopup - calling FSL applytopup
#' @description A tool for applying and correcting 
#' estimated susceptibility induced distortions
#' 
#' @param infile list of names of input image (to be corrected)
#' @param datain name of text file with PE directions/times
#' @param topup_files name of field/movements (from topup)
#' @param index list of indices into --datain of 
#' the input image (to be corrected)
#' @param method Use jacobian modulation (jac) or 
#' least-squares resampling (lsr), default=lsr.
#' @param out basename for output (warped) image
#' @param interp Image interpolation model, trilinear or spline. Default spline
#' @param verbose Print diagnostic information while running
#' 
#' @export
applytopup = function(
  infile, 
  datain, 
  index,
  topup_files,
  out = NULL, 
  method = c("lsr", "jac"),
  interp = c("spline", "trilinear"), 
  verbose = TRUE ){
  
  warning("This function has not been tested")
  
  index = paste(index, collapse = ",")
  
  topup_files = checkimg(topup_files)
  topup_files = unname(topup_files)

  method = match.arg(method)
  interp = match.arg(interp)

  
  parse_num_args = function(x){
    x = paste0(names(x), '=', x)
    x = paste(x, collapse = " ")
    x
  }  
  
  if (is.null(out)) {
    out = tempfile()
  }
  
  L = list(out = out, 
           inindex = index,
           topup = topup_files,
           interp = interp, 
           method = method
           )
  
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
  cmd = paste0(cmd, "applytopup")
  infile = checkimg(infile)
  infile = unname(infile)
  infile = normalizePath(infile)
  infile = paste(infile, collapse = ",")
  
  datain = normalizePath(datain)
  datain = unname(datain)
  
  args = paste0("--imain=", infile, " --datain=", datain)
  cmd = paste(cmd, args, opts)
  
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd)
  return(res)
}


#' @rdname applytopup
#' @export
apply_topup = function(...){
  res = applytopup(...)
  return(res)
}


#' @param ... arguments passed to \code{topup} if using \code{fsl_topup}
#' @rdname applytopup
#' @export
fsl_applytopup = function(...){
  res = applytopup(...)
  return(res)
}