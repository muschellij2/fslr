#' @title Window image based on quantiles of Image
#'
#' @description Takes an image, finds the quantiles given by 
#' \code{probs}, then sets values outside these values to other 
#' values, as determined by \code{replace} argument passed to
#' to \link{\code{window_img}}
#' @param img object of class nifti
#' @param probs quantiles to constrain the image these define the window sent to \link{\code{window_img}}
#' @param ... additional arguments sent to \link{\code{window_img}}
#' @export
#' @return Object of class nifti with values outside quantiles replaced
#' by values depending on replace argument passed to \link{\code{window_img}}
robust_window <- function(img, # object of class nifti
           probs = c(0, 0.999), # quantiles to constrain the image, these define the window sent to \link{\code{window_img}}
           ... # additional arguments sent to \link{\code{window_img}}
           ){
  cc = c(img)
  quant = quantile(cc, probs = probs)
  img = window_img(img, window = quant, ...)
  img = cal_img(img)
}