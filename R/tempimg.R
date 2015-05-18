#' @title Create temporary nii.gz file for FSL
#' @description Takes in a object of class nifti, writes it 
#' to a temp file, appends
#' .nii.gz as \code{\link{writeNIfTI}} adds it.
#' @param nim object of class nifti
#' @param gzipped Should file be gzipped? Passed to 
#' \code{\link{writeNIfTI}}
#' @param checknan Check for NAs or NaNs
#' @param check_type Check the datatype for an image.  Will run \code{\link{datatyper}}.
#' @param ... Not used
#' @return filename of output nii.gz
#' @export
tempimg = function(nim, gzipped= TRUE, checknan = TRUE, check_type = FALSE, ...){
  f = tempfile()
  nim = cal_img(nim)
  nim = zero_trans(nim)
  if (checknan){
    cnim = c(nim)
    if (any(is.na(cnim) | is.nan(cnim))){
      warning("NAs and NaNs in image file, replacing with zeros")
      nim[is.na(nim)| is.nan(cnim)] = 0
    }
  }
  if (check_type){
    nim = datatyper(nim)
  }
  ########### added for weird stuff of NIfTI
  nim = drop_img_dim(nim)
  writeNIfTI(nim, filename= f, onefile = TRUE, gzipped = gzipped)
  ext = ".nii"
  if (gzipped) ext = paste0(ext, '.gz')
  f = paste0(f, ext)
  f = normalizePath(f)
  return(f)
}
