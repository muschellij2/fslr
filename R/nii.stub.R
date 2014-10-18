#' @title Grab nii file stubname
#' @description Quick helper function to strip of .nii or .nii.gz from filename
#' @return NULL
#' @param x character vector of filenames ending in .nii or .nii.gz
#' @param bn Take \code{\link{basename}} of file?
#' @export
nii.stub = function(x, bn=FALSE){
  x = path.expand(x)
  stub = gsub("\\.gz$", "", x)
  stub = gsub("\\.nii$", "", stub)
  if (bn) stub = basename(stub)
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
  cal.max(img) = cmax
  cal.min(img) = cmin
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


#' @title Drop Image Dimension
#' @return Object of class nifti
#' @param img nifti object
#' @description Drops a dimension of an image that has one-dimension and 
#' sets respective values to 0 in pixdim or 1 in dim
#' @export
drop_img_dim = function(img){
  dim_  = img@dim_
  pdim = pixdim(img)
  no.data = dim_ <= 1
  no.data[1] = FALSE
  pdim[no.data] = 0
  pixdim(img) = pdim
  ### subtract 1 for first observation
  ndim = sum(!no.data) - 1
  dim_[1] = ndim
  dim_[no.data] = 1
  img@dim_ = dim_
  img@.Data = drop(img@.Data)
  img
}

#' @title Creates onefile specification
#' @return Object of class nifti
#' @param img nifti object
#' @description Changes the magic and vox_offset slots to be consistent 
#' with onefile option in \code{\link{writeNIfTI}}.  As of version 0.4.0,
#' \code{oro.nifti} did not support "ni1" magic outputs for output.
#' @export
onefile = function(img){
  img@magic = "n+1"
  img@vox_offset = 352
  img
}
