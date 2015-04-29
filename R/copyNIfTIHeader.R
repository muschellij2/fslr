
#' @title Copy NIfTI Header to an array
#'
#' @description Copies slots of a \code{nifti} object to an array.  This is useful if you're
#' subsetting 4D data and getting an array out
#' @param img object of class nifti to copy header
#' @param arr array to copy header information
#' @param drop_slots Slots not to copy over from header
#' @param drop Should \code{\link{dropImageDimension}} be called before returning?
#' @param ... arguments to pass to \code{\link{niftiarr}}
#' @export
#' @return Object of class nifti the size of \code{arr}
#' @examples 
#' img = nifti(img = array(rnorm(10^4), dim=rep(10, 4)), dim=rep(10, 4), datatype = 16)
#' sub = img[,,,1:3]
#' copyNIfTIHeader(img, sub)
#' sub = img[,,,1, drop=FALSE]
#' copyNIfTIHeader(img, sub) 
#' copyNIfTIHeader(img, sub, drop = FALSE) 
copyNIfTIHeader <- function(img, # object of class nifti to copy header
                      arr, # array to copy header information
                      drop_slots = c(".Data", "dim_"),
                      drop = TRUE,
                      ...
                      ){
  arr = as.array(arr)
  ### get the slot names
  snames = slotNames(img)
  # drop those not to be copied
  snames = snames[ !snames %in% drop_slots ]
  # create new nifti object
  arr = nifti(img = arr, dim = dim(arr))
  # copy over slots
  for (islot in snames){
    slot(arr, islot) = slot(img, islot)
  }
  #calibrate image
  arr = calibrateImage(arr)
  if (drop) {
    arr = dropImageDimension(arr, ...)
  }
  return(arr)
}

