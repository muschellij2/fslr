#' @title Get FSL's Data Directory 
#' @description Finds the FSLDIR from system environment or \code{getOption("fsl.path")}
#' and pastes on ``data''
#' @return Character path
#' @export
fsl_data_dir = function(){
  file.path(fsldir(), "data")
}

#' @title Get FSL's Standard Data Directory 
#' @description Finds the FSLDIR from system environment or \code{getOption("fsl.path")}
#' and pastes on ``data/standard''
#' @return Character path
#' @export
fsl_std_dir = function(){
  file.path(fsl_data_dir(), "standard")
}

#' @param file A file from the standard data file
#' @rdname fsl_std_dir
#' @export
fsl_std_file = function(file = NULL) {
  if (is.null(file)) {
    file = list.files(path = fsl_std_dir(), pattern = ".nii.gz", 
               recursive = TRUE, include.dirs = FALSE, 
               full.names = TRUE)
    return(file)
  }
  file = file.path(fsl_std_dir(), file)
  if (!all(file.exists(file))) {
    warning("Not all files exist for fsl_std_file")
  }
}


#' @title Get FSL's Standard Data Directory 
#' @description Finds the FSLDIR from system environment or \code{getOption("fsl.path")}
#' and pastes on ``data/standard''
#' @return Character path
#' @export
fsl_atlas_dir = function(){
  file.path(fsl_data_dir(), "atlases")
}



