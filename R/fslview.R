#' @title Open image in FSLView
#' @description This function calls \code{fslview} to view an image 
#' in the FSL viewer
#' @param file (character) filename of image to be thresholded
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) options for FSLView
#' @param verbose (logical) print out command before running
#' @param ... options passed to \code{\link{checkimg}}
#' @return character or logical depending on intern
#' @importFrom utils compareVersion
#' @export
fslview = function(file, intern=TRUE, opts ="", verbose = TRUE, ...){
  cmd <- get.fsl()
  if (is.nifti(file)) {
    file = checkimg(file)
  }
  file = lapply(file, checkimg, ...)
  if (length(file) != length(opts)) {
    opts = rep(opts, length = length(file))
  } else {
    if (length(file) > length(opts)) {
      opts = c(opts, rep("", length = (length(file) - length(opts))))
    } else {
      opts = opts[seq(length(file))]
    }
  }
  file = shQuote(file)
  file = paste(file, opts)
  file = paste(file, collapse = " ")
  fslview_cmd = "fslview"
  fslver = fsl_version()
  ################################## 
  # new version deprecated it
  ################################## 
  comp = utils::compareVersion(fslver, "5.0.10")
  if (comp >= 0) {
    fslview_cmd = paste0(fslview_cmd, "_deprecated")
  }
  string = paste0(fslview_cmd, " %s")
  cmd <- paste0(cmd, sprintf(string, file))  
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd, intern = intern)
  return(res)
}

#' @title FSLView help
#' @description This function calls \code{fslview}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' library(fslr)
#' if (have.fsl()){
#'  fslview.help()
#' }   
fslview.help = function(){
  return(fslhelp("fslview"))
}