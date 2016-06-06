#' @title Run FIRST All
#' @description Wrapper for \code{run_first_all} from FSL for FIRST analysis 
#' segmentation of subcortical structures
#' @param img pecifies the input image (T1-weighted)
#' @param oprefix specifies the output image basename (extensions will be added to this)
#' @param brain_extracted specifies that the input image has been brain extracted
#' @param structures a restricted set of structures to be segmented 
#' @param affine specifies the affine registration matrix to standard space (optional)
#' @param opts (character) operations to be passed to \code{run_first_all} 
#' @param verbose (logical) print out command before running
#' @return Result of \code{\link{system}}
#' @export
run_first_all <- function(img, oprefix, 
                          brain_extracted = FALSE,
                          structures = NULL,
                          affine = NULL,
                          opts = "",
                          verbose = TRUE
                          ){
  img = checkimg(img)
  args = list(i = shQuote(img),
              o = shQuote(oprefix))
  if (brain_extracted) {
    args$b = ""
  }
  args$a = affine
  args$structures = structures
  
  name_args = names(args)
  if (is.null(name_args)) {
    name_args = rep("", length(args))
  }
  stopifnot(length(name_args) == length(args))
  name_args = paste0("-", name_args)
  args = paste(name_args, args)
  args = paste(args, sep = "", collapse = " ")
  
  cmd = get.fsl()

  ##########################
  # Add frontopts
  ##########################
  cmd <- paste0(cmd, "run_first_all ", args)

  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd, intern = FALSE)
  
  return(res)
}


#' @title Run FIRST All Help
#' @description This function calls \code{run_first_all}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  run_first_all.help() 
#' }
run_first_all.help  = function(){
  return(fslhelp("run_first_all", help.arg = "-h"))
}

