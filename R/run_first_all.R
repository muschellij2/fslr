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
#' @return List of results, including result of \code{\link{system}}
#' and some output files
#' @export
run_first_all <- function(
  img, 
  oprefix = tempfile(), 
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
  if (verbose) {
    args$v = ""
  }
  
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
  
  L = list(result = res)
  stubs = c(
    "_all_fast_firstseg.nii.gz", 
    "_all_fast_origsegs.nii.gz",
    "-BrStem_first.bvars", "-BrStem_first.vtk", 
    "-L_Accu_first.bvars", "-L_Accu_first.vtk", 
    "-L_Amyg_first.bvars", "-L_Amyg_first.vtk", 
    "-L_Caud_first.bvars", "-L_Caud_first.vtk", 
    "-L_Hipp_first.bvars", "-L_Hipp_first.vtk", 
    "-L_Pall_first.bvars", "-L_Pall_first.vtk", 
    "-L_Puta_first.bvars", "-L_Puta_first.vtk", 
    "-L_Thal_first.bvars", "-L_Thal_first.vtk", 
    "-R_Accu_first.bvars", "-R_Accu_first.vtk", 
    "-R_Amyg_first.bvars", "-R_Amyg_first.vtk", 
    "-R_Caud_first.bvars", "-R_Caud_first.vtk", 
    "-R_Hipp_first.bvars", "-R_Hipp_first.vtk", 
    "-R_Pall_first.bvars", "-R_Pall_first.vtk", 
    "-R_Puta_first.bvars", "-R_Puta_first.vtk", 
    "-R_Thal_first.bvars", "-R_Thal_first.vtk", 
    ".com", ".com2", ".com3", ".logs")  

  seg = paste0(
    oprefix, 
    "_all_fast_firstseg.nii.gz")
  if (file.exists(seg)) {
    L$segmentation = seg
  } 
  seg_list = paste0(
    oprefix, 
    "_all_fast_origsegs.nii.gz")
  if (file.exists(seg_list)) {
    L$segmentation_list = seg_list
  }
  
  outfiles = paste0(oprefix, stubs)
  outfiles = outfiles[ file.exists(outfiles)]
  
  L$outfiles = outfiles
  
  return(L)
}


#' @title Run FIRST All Help
#' @description This function calls \code{run_first_all}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' library(fslr)
#' 
#' if (have.fsl()){
#'  run_first_all.help() 
#' }
run_first_all.help  = function(){
  return(fslhelp("run_first_all", help.arg = "-h"))
}

