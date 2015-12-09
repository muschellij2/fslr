#' @title Wrapper for FSL Get Orientation
#' @description This function calls \code{fslorient -getorient} and is a 
#' simple wrapper of \code{\link{fslorient}}
#' @param file (character) image to be manipulated
#' @param opts option to send to fslorient
#' @param verbose (logical) print out command before running
#' @return Result from system command, output from FSL
fslorienter = function(
  file,
  opts = "",
  verbose = TRUE){
  
  res = fslorient(file = file, verbose = verbose, retimg = FALSE,
                  intern = TRUE, opts = opts)
  
  return(res)  
}


#' @title FSL Orientation Wrappers
#' @description This function calls \code{fslorient -get*} and is a 
#' simple wrapper of \code{\link{fslorient}}
#' @rdname fslorient_wrap
#' @param file (character) image to be manipulated
#' @param verbose (logical) print out command before running
#' @return Result from system command, output from FSL
#' @export
fslgetorient = function(
  file,
  verbose = TRUE){
  fslorienter(file = file, opts = "-getorient", verbose = verbose)
}

#' @rdname fslorient_wrap
#' @export
fslgetsform = function(
  file,
  verbose = TRUE){
  fslorienter(file = file, opts = "-getsform", verbose = verbose)
}

#' @rdname fslorient_wrap
#' @export
fslgetqform = function(
  file,
  verbose = TRUE){
  fslorienter(file = file, opts = "-getqform", verbose = verbose)
}


#' @rdname fslorient_wrap
#' @export
fslgetsformcode = function(
  file,
  verbose = TRUE){
  fslorienter(file = file, opts = "-getsformcode", verbose = verbose)
}

#' @rdname fslorient_wrap
#' @export
fslgetqformcode = function(
  file,
  verbose = TRUE){
  fslorienter(file = file, opts = "-getqformcode", verbose = verbose)
}


