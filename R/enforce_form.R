
#' Enforce Either Qform or Sform is set
#'
#' @param file (character) image filename or character 
#' of class nifti
#' @param ... additional arguments to pass to 
#' \code{\link{getForms}}
#'
#' @return A character filename
#' @export
#' 
#' @examples 
#' if (have_fsl()) {
#' res = enforce_form(mni_fname())
#' }
enforce_form = function(file, ...) {
  file = checkimg(file)
  stopifnot(length(file) == 1)
  form = getForms(file = file, ...)
  both_zero = form$qform_code == 0 & form$sform_code == 0
  
  if (any(both_zero)) {
    us = !any(form$ssor == "UU")
    fs = !any(form$sqor == "UU")
    if (fs && us) {
      stop("both qform and sform are set")
    }
    if (!fs && !us) {
      stop("neither qform or sform has orientation")
    }	
    if (fs && !us) {
      if (all(form$qform == 0)) {
        stop("qform selected but all zero")
      }
      code = "qform_code"
    }
    if (!fs && us) {
      if (all(form$sform == 0)) {
        stop("sform selected but all zero")
      }
      code = "sform_code"
    }
    
    stopifnot(all(code %in% c("qform_code", "sform_code")))
    
    img = readnii(file)
    slot(img, code) = 1
    pixdim(img)[1] = 1
    file = tempimg(img)
  }
  return(file)
}