#' @title Grab nii file stubname
#' @description Quick helper function to strip of .nii or .nii.gz from filename
#' @return NULL
#' @param x character vector of filenames ending in .nii or .nii.gz
#' @export

nii.stub = function(x){
  stub = gsub("\\.gz$", "", x)
  stub = gsub("\\.nii$", "", stub)
  return(stub)
}
