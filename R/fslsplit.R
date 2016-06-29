#' @title Split images using FSL
#' @description This function calls \code{fslsplit} to merge files on some dimension
#' and either saves the image or returns an object of class nifti   
#' @param infile (character) input filename
#' @param direction (character) direction to split over: t (time), x, y, z
#' @param output_basename (character) prefix to have for output
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param verbose (logical) print out command before running 
#' @return List of output files 
#' @export
fslsplit = function(infile, 
                    direction = c("t", "x", "y", "z"), 
                    output_basename = NULL, 
                    retimg = TRUE,
                    reorient = FALSE,                   
                    verbose = TRUE
                    ){
  
  infile = checkimg(infile)
  stopifnot(length(infile) == 1)
  cmd <- get.fsl()
  direction = match.arg(direction)
  direction = direction[1]
  ndir = switch(direction,
                "t" = "dim4",
                "x" = "dim1",
                "y" = "dim2",
                "z" = "dim3")
  n_values = fslval(infile, keyword = ndir)
  n_values = as.numeric(n_values)
  
  stopifnot(!is.na(n_values))
  if (verbose) {
    message(paste0("There should be ", n_values, " output files."))
  }    
  
  if (is.null(output_basename)) {
    output_basename = tempfile(fileext = "_")
  }
  cmd <- paste0(cmd, sprintf('fslsplit "%s" %s -%s', 
                             infile, output_basename, direction))
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd, intern = FALSE)
  stopifnot(res == 0)
  
  endings = sprintf("%04.0f", seq(0, n_values - 1))
  outfiles = paste0(output_basename, endings)
  
  ext = get.imgext()
  outfiles = paste0(outfiles, ext)  
  if (retimg) {
    outfiles = check_nifti(outfiles, reorient = reorient)
    return(outfiles)
  }
  return(outfiles)
}


#' @title FSL Split help
#' @description This function calls \code{fslsplit}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslsplit.help()
#' }  
fslsplit.help = function(){
  return(fslhelp("fslsplit", help.arg = ""))
}