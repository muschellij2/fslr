#' @title Construct MNI Filename
#' @description Finds the standard data directory for FSL and pastes together
#' the string for an MNI template image
#' @param mm Resolution (in mm) of the brain image (isotropic)
#' @param brain Should the brain be returned (default) or the T1 with the skull
#' @param linear Should the linearized MNI template be used
#' @param mask should the mask be given?  Generally, only
#' MNI152_T1_1mm_brain_mask exists.
#' @return Character path of filename, warning if that file does not exist
#' @export
mni_fname = function(mm = c("1", "0.5", "2"),
                   brain = FALSE,
                   linear = FALSE,
                   mask = FALSE){
  mm = as.character(mm)
  mm = match.arg(mm)
  stub = "MNI152"
  stub = paste0(stub, ifelse(linear, "lin", ""), "_T1")
  stub = paste0(stub, "_", mm, "mm")
  stub = paste0(stub, ifelse(brain, "_brain", ""))
  stub = paste0(stub, ifelse(mask, "_mask", ""))
  
  fname = paste0(stub, ".nii.gz") 
  fname = file.path(fsl_std_dir(), fname)
  if (!file.exists(fname)) {
    warning(paste0("file ", fname, " does not exist!"))
  }
  return(fname)
}

#' @title Read MNI Filename
#' @description Simple wrapper for reading in the MNI image constructed from 
#' \code{\link{mni_fname}}
#' @param ... Arguments passed to \code{\link{mni_fname}}
#' @return Object of class \code{\link{nifti}}
#' @export
mni_img = function(...){
  fname = mni_fname(...)
  if (!file.exists(fname)) {
    stop(paste0("file ", fname, " does not exist!"))
  }
  img = readnii(fname)
  return(img)
}
