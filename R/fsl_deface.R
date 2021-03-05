#' Tool to deface a structural T1w image.
#'
#' @param file (character) input image to estimate edge strength
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented 
#' when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fsl_deface}
#' @param deface_cropped apply the defacing to the cropped image instead of the original image
#' @param verbose print diagnostic messages
#' @param bet_fractional_intensity fractional intensity for bet (0->1); default=0.5; 
#' @param bias_correct Bias-correct the input image (with fast);
#' @param shift_xyz Shift, in mm, x-, y- and z-directions, to shift face mask by;
#' @param cog_xyz centre-of-gravity for bet (voxels, not mm);
#' @param ... additional arguments passed to \code{\link{fslcmd}}.
#' 
#' @export
#' @examples
#' \donttest{
#' if (have_fsl()) {
#'   file = mni_fname(mm = 1, brain = FALSE)
#'   out = fsl_deface(file, retimg = FALSE)
#' }
#' }
fsl_deface  = function(
  file,
  outfile = NULL, 
  retimg = TRUE,
  opts = "", 
  deface_cropped = FALSE,
  bet_fractional_intensity = NULL,
  bias_correct = FALSE,
  shift_xyz = NULL,
  cog_xyz = NULL,
  reorient = FALSE,
  intern = FALSE,   
  verbose = TRUE,
  ...){
  
  if (package_version(fsl_version()) < package_version("6.0.1")) {
    warning("fsl_deface requires FSL version 6.0.1 or higher")
  }
  if (is.null(outfile)) {
    outfile = tempfile(fileext = ".nii.gz")
  }
  defacing_mask = tempfile(fileext = ".nii.gz")
  cropped_struc = tempfile(fileext = ".nii.gz")
  orig_2_std  = tempfile(fileext = ".mat")
  orig_2_cropped  = tempfile(fileext = ".mat")
  cropped_2_std  = tempfile(fileext = ".mat")
  opts = c(opts, 
           if (deface_cropped) "-k",
           if (bias_correct) "-B",
           paste0("-d ", shQuote(defacing_mask)),
           paste0("-n ", shQuote(cropped_struc)),
           paste0("-m13 ", shQuote(orig_2_std)),
           paste0("-m12 ", shQuote(orig_2_cropped)),
           paste0("-m23 ", shQuote(cropped_2_std))
  )
  if (!is.null(shift_xyz)) {
    shift_xyz = paste(shift_xyz, collapse = " ")
    opts = c(opts, paste0("-nud ", shift_xyz))
  }
  if (!is.null(cog_xyz)) {
    cog_xyz = paste(cog_xyz, collapse = " ")
    opts = c(opts, paste0("-c ", cog_xyz))
  }
  if (!is.null(bet_fractional_intensity)) {
    opts = c(opts, paste0("-f ", as.character(bet_fractional_intensity)))
  }
  out = fslcmd(
    "fsl_deface",
    file = file,
    outfile = outfile, 
    retimg = retimg,
    reorient = reorient,
    intern = intern, 
    opts = opts, 
    verbose = verbose,
    samefile = FALSE,
    opts_after_outfile = TRUE,
    frontopts = "",
    no.outfile = FALSE,
    trim_front = FALSE,
    run = TRUE,
    ...)
  L = list(
    outfile = out,
    defacing_mask = defacing_mask,
    cropped_image = cropped_struc,
    orig_2_std  = orig_2_std,
    orig_2_cropped  = orig_2_cropped,
    cropped_2_std  = cropped_2_std
  )
  if (retimg) {
    L[c("cropped_image", "defacing_mask")] = lapply(
      L[c("cropped_image", "defacing_mask")],
      neurobase::readnii, 
      reorient = reorient)
  } 
  return(L)
  
}