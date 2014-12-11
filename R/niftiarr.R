#' @title Make new nifti from array
#'
#' @description Make new nifti object by passing in old nifti and array
#' @param img object of class nifti
#' @param arr array to be passed in to .Data slot
#' @export
#' @return object of class nifti
niftiarr <- function(img, # object of class nifti
                     arr ## array to be passed in
){
  x = img
  if (!is(arr, "array")){
    arr = array(arr, dim=dim(img))
  }
  arr = as(arr, "array")
  class(arr) = "numeric"
  if (!all(dim(arr) == dim(img))){
    stop(
      paste("Dimensions of Array and Dimensions of Image do not match. Arr:", 
             dim(arr), ". Img:", dim(img), collapse=" "))
  }
  x@.Data = arr
  x = cal_img(x)
  x = zero_trans(x)
}
