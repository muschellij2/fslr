#' MRI Defacer
#'
#' @param file input file image to remove face/ears
#' @param brain_mask brain mask of file.  If \code{NULL}, \code{\link{fslbet}}
#' will be applied
#' @param bet_opts options to pass to  \code{\link{fslbet}} if applied
#' @param search_radius search radius option to pass to \code{\link{flirt}}
#' @param opts additional options to pass to \code{\link{flirt}}
#' @param template_brain template brain image, may be \code{NULL}
#' @param template_brain_weight template brain weight image, used
#' for registration may be \code{NULL}
#' @param template_biometric_mask template biometric mask.  Everything that 
#' is wanted should be \code{1},  may be \code{NULL}
#' @param verbose print diagnostic messages.  If \code{> 1}, more verbose
#' @param ... not used
#' 
#' @note Adapted from 
#' \url{https://github.com/mih/mridefacer}
#'
#' @return A character filename of the output image
#' @export
#'
#' @examples
#' if (have_fsl()) {
#' file = "~/Downloads/sample_T1_input.nii.gz"
#' if (file.exists(file)) {
#'     res = mridefacer(file)
#' }
#' }
mridefacer = function(file, ...,  verbose = TRUE) {
  outfile = get_mridefacer_mask(file = file, verbose = verbose, ...)
  if (verbose) {
    message("Masking the image with mridefacer mask")
  }
  res = fsl_mask(file, mask = outfile, verbose = verbose > 1)
  outfile = as.character(outfile)
  attr(res, "mask") =  outfile 
  return(res)
}  


#' @export
#' @rdname mridefacer
get_mridefacer_mask =  function(
  file,
  brain_mask = NULL,
  bet_opts = "-f 0.5",
  search_radius = 90,
  opts = NULL,
  template_brain = NULL, 
  template_brain_weight = NULL,
  template_biometric_mask  = NULL,
  verbose = TRUE) {
  
  null_brain = is.null(template_brain)
  null_weight = is.null(template_brain_weight)
  null_mask = is.null(template_biometric_mask)
  res = sum(null_brain, null_weight, null_mask)
  if (!res %in% c(0, 3)) {
    stop("template information must all be NULL or all be specified")
  }
  dl_file = function(fname, filename) {
    url = "https://raw.githubusercontent.com/mih/mridefacer/master/data/"
    if (is.null(fname)) {
      fname = tempfile(fileext = paste0("_", filename))
      in_url = paste0(url, filename)
      dl = download.file(in_url, destfile = fname,
                         quiet = verbose < 2)
      stopifnot(dl == 0)
    }
    fname = checkimg(fname)
    return(fname)
  }
  if (verbose & res == 3) {
    message("Downloading template images")
  }
  template_brain = dl_file(template_brain,  
                           "head_tmpl_brain.nii.gz")
  template_brain_weight = dl_file(template_brain_weight,  
                                  "head_tmpl_weights.nii.gz")
  template_biometric_mask = dl_file(template_biometric_mask, 
                                    "face_teeth_ear_mask.nii.gz")
  
  
  if (verbose) {
    message("Reorienting File")
  }
  rpi_img = rpi_orient_file(file, verbose = verbose > 1)
  file = rpi_img$img
  if (is.null(brain_mask)) {
    # brain extract
    if (verbose) {
      message("Running Brain extraction")
    }
    brain_image = fsl_bet(file, opts = bet_opts, 
                          verbose = verbose > 1)
    bm = rpi_img
  } else {
    bm = rpi_orient_file(brain_mask, verbose = verbose > 1)
    brain_mask = bm$img    
    brain_image = fsl_mask(file = file, 
                           mask = brain_mask, verbose = verbose > 1)
  }
  
  
  # XXX SOME LIKE IT, SOME NOT
  flirt_opts = c("-bins 256", "-cost corratio", 
                 paste("-searchrx", -1*search_radius, search_radius),
                 paste("-searchry", -1*search_radius, search_radius),
                 paste("-searchrz", -1*search_radius, search_radius),
                 paste("-inweight", template_brain_weight),
                 opts)
  flirt_opts = paste(flirt_opts, collapse = " ")
  omat = tempfile(fileext = ".mat")
  trans = tempfile(fileext = ".nii.gz")
  if (verbose) {
    message("Registering template brain to brain image")
  }
  res = flirt(
    infile = template_brain, 
    dof = 12, 
    reffile = brain_image, 
    outfile = trans,
    retimg = FALSE,
    omat = omat, opts = flirt_opts, 
    verbose = verbose > 1)
  
  if (verbose) {
    message("Applying transformation to biometric mask")
  }
  outfile = tempfile(fileext = ".nii.gz")
  outfile = flirt_apply(
    template_biometric_mask,
    reffile = brain_image,
    outfile = outfile,
    retimg = FALSE,
    opts = "-interp nearestneighbour",
    initmat = omat, 
    verbose = verbose > 1)
  
  if (verbose) {
    message("Reversing the reorientation from first step")
  }
  outfile = reverse_rpi_orient_file(
    outfile, 
    convention = rpi_img$convention, 
    orientation = rpi_img$orientation, verbose = verbose > 1)
  attr(outfile, "transformation_matrix") = omat
  return(outfile)
}


