#' @title Get Image file extension
#'
#' @description Get the image file extension, either .nii, .hdr, .nii.gz, 
#' or .hdr.gz
#' @param file Vector of character filenames
#' @param withdot Should the extension begin with \code{"."}?
#' @export
#' @return Vector of extensions.  If \code{withdot = FALSE}, then will
#' return \code{nii} instead of \code{.nii}
file_imgext <- function(file, withdot = TRUE){
  file = tolower(file)
  hdrgz = grepl("[.]hdr[.]gz$", file)
  niigz = grepl("[.]nii[.]gz$", file)
  nii = grepl("[.]nii$", file)
  hdr = grepl("[.]hdr$", file)
  
  ext = rep(NA, length=length(file))
  dot = ifelse(withdot, ".", "")
  ext[hdrgz] = paste0(dot, "hdr.gz")
  ext[niigz] = paste0(dot, "nii.gz" )
  ext[nii] = paste0(dot, "nii" )
  ext[hdr] = paste0(dot, "hdr" )
  
  return(ext)
  
}