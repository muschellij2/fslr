#' @title Get min/max of an image
#' @rdname fslmin_max
#' @description This function calls the range or robust range functions from FSL 
#' and then extracts the min/max
#' @param file (character) filename of image to be checked
#' @param ... options passed to \code{\link{fslrange}}
#' @return Numeric vector of mins/maxs or just one depending if \code{ts = TRUE}
#' @export
#' @examples
#' if (have.fsl()){
#'  mnifile = file.path(fsldir(), "data", "standard", 
#'    "MNI152_T1_2mm.nii.gz")
#'  fslmax(mnifile)
#' }  
fslmax <- function(file, ...){
 
  val = fslrange(file = file, ...)
  
  if (is.vector(val)) {
    val = val[2]
  } else {
    val = val[, 2]
  }
  val
}

#' @rdname fslmin_max
#' @export
fslmin <- function(file, ...){
  
  val = fslrange(file = file, ...)
  
  if (is.vector(val)) {
    val = val[1]
  } else {
    val = val[, 1]
  }
  val
}