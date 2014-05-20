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


#' @title Set Max/Min for nifti object by range of data
#' @return object of type nifti
#' @param img nifti object
#' @description Rescales image cal_max and cal_min slots to be the max and min, 
#' respectively, of an object of class nifti, with \code{na.rm=TRUE}. This is so that 
#' when images are rendered/written, the values correspond to those
#' in the array (stored in .Data slot) are plotted on correct grayscale and no error is
#' given by \code{writeNIfTI}.
#' @name cal_img
#' @export
cal_img = function(img){
  cmax = max(img, na.rm=TRUE) 
  cmax = ifelse(is.finite(cmax), cmax, 0)
  cmin = min(img, na.rm=TRUE) 
  cmin = ifelse(is.finite(cmin), cmin, 0)  
  img@cal_max = cmax
  img@cal_min = cmin
  img
}

#' @title Change intercept to 0 and slope to 1
#' @return object of type nifti
#' @param img nifti object (or character of filename)
#' @description Forces image scl_slope to 1 and scl_inter  to be 0 of slots of class
#' nifti.  This is so that when images are rendered/written, the values correspond to those
#' in the array (stored in Data slot) and are not scaled.
#' @name zero_trans
#' @export
zero_trans = function(img){
  img = check_nifti(img)  
  img@scl_slope = 1
  img@scl_inter = 0
  return(img)
}



#' @title Gets Voxel Dimensions
#' @return Vector of length 3
#' @param img nifti object
#' @description Grabs the pixdim and takes the correct elements
#' @export
voxdim = function(img){
  pixdim(img)[2:4]
}
