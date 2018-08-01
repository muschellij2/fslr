#' Title
#'
#' @param file Input file to resample
#' @param voxel_size Voxel size (in mm).  This should be a scalar number.
#' @param outfile (character) output filename
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}. 
#' @param verbose (logical) print out command before running
#'
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' the output file.
#' @export
#'
#' @examples
#' if (have_fsl()) {
#' file = mni_fname(mm = 1, brain = TRUE)
#' est2 = fsl_resample(file = file, voxel_size = 1, retimg = FALSE)
#' pixdim(est2)
#' est = fsl_resample(file = file, voxel_size = 1)
#' pixdim(est)
#' }
#' 
fsl_resample = function(
  file, voxel_size,
  outfile = NULL,  
  retimg = TRUE,
  reorient = FALSE,
  verbose = TRUE) {
  
  if (is.null(outfile) && !retimg) {
    outfile = tempfile()
  }
  func = "flirt"
  file = checkimg(file)
  opts = NULL
  voxel_size = voxel_size[1]
  
  opts = c(opts, paste0("-ref ", file))
  opts = c(opts, paste0("-applyisoxfm ", voxel_size))
  opts = c(opts, "-out")
  opts = paste(opts, collapse = " ")
  
  frontopts = "-in"
  cmd = fslcmd(
    func = func,
    file = file,
    opts = opts,
    frontopts = frontopts,
    trim_front = FALSE,
    outfile = outfile,
    retimg = retimg, 
    reorient = reorient,
    verbose = verbose,
    run = TRUE)
  return(cmd)
  
}
