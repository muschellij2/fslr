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


#' @title Get FSL's Standard Data Directory 
#' @description Finds the FSLDIR from system environment or \code{getOption("fsl.path")}
#' and pastes on ``data/standard''
#' @return Character path
#' @export
fsl_atlas_dir = function(){
  file.path(fsl_data_dir(), "atlases")
}