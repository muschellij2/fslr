#' @title Mid-Sagittal Plane Alignment
#' @description This function takes in an image, flips the image over the
#' left/right plane, registers that flipped image to the original image,
#' then applies the half transformation
#'
#' @param file (character) input filename or class nifti
#' @param outfile (character) output filename
#' @param retimg (logical) return image of class nifti
#' @param opts (character) options passed to \code{\link{flirt}}
#' @param verbose (logical) print diagnostic messages
#' @param translation (logical) should the translation parameters be
#' preserved (TRUE) or set to zero (FALSE)
#' @param force_rpi Should \code{\link{rpi_orient_file}} be
#' run?
#'
#' @return Filename of output or nifti depending on \code{retimg}
#' @export
#' @examples
#' if (have.fsl()){
#'   mnifile = file.path(fsldir(), "data", "standard",
#'                       "MNI152_T1_2mm.nii.gz")
#'   aligned = mid_sagittal_align(mnifile)
#'   thresh = readnii(mnifile) > 0
#'   file_mat = attr(aligned, "half_transform")
#'   force_rpi = attr(aligned, "force_rpi")
#'   flipped_thresh = apply_mid_sagittal_align(
#'     file = thresh, 
#'     file_mat = file_mat,
#'     apply_opts = "-interp nearestneighbour",
#'     force_rpi = force_rpi
#'   )
#' }
mid_sagittal_align = function(
    file, 
    outfile = NULL,
    retimg = TRUE,
    opts = "",
    translation = TRUE,
    force_rpi = TRUE,
    verbose = TRUE) {
  
  outfile = check_outfile(outfile = outfile, retimg = retimg)
  if (force_rpi) {
    rp = rpi_orient_file(file, verbose = verbose)
    img = rp$img
  } else {
    img = checkimg(file)
  }
  
  flip_lr = function(x){
    fsl_swapdim(file = x, a = "-x")
  }
  
  flipped = flip_lr(img)
  
  omat = tempfile(fileext = ".mat")
  flirt(infile = img, 
        reffile = flipped,
        omat = omat, 
        dof = 6,
        opts = opts,
        retimg = FALSE, 
        outfile = outfile,
        verbose = verbose)  
  
  parsed = fsl_avscale(file = omat, parsed = TRUE)
  # parsed = parse_avscale(scaled)
  
  mat = parsed$fwd_half_transform
  if (!translation) {
    mat[, 4] = c(0, 0, 0, 1)
  }
  # mat = mat * 0.5
  
  new_omat = tempfile(fileext = ".mat")
  mat = apply(mat, 1, paste, collapse = " ")
  mat = paste0(mat, " ")
  writeLines(mat, new_omat)
  
  tfile = tempfile(fileext = ".nii.gz")
  flirt_apply(
    infile = img, 
    reffile = flipped, 
    initmat = new_omat,
    verbose = verbose,
    retimg = FALSE,
    outfile = outfile)
  if (force_rpi) {
    centered = reverse_rpi_orient_file(
      file = outfile, 
      orientation = rp$orientation,
      convention = rp$convention)
  } else {
    centered = outfile
  }
  if (retimg) {
    centered = readnii(centered)
  }
  
  attr(centered, "half_transform") = new_omat
  attr(centered, "force_rpi") = force_rpi
  attr(centered, "full_transform") = omat
  return(centered)
  
}


#' @export
#' @rdname mid_sagittal_align
#' @param apply_opts options to pass to [fslr::flirt_apply]
#' @param file_mat file name of mat file for half transform from [mid_sagittal_align]
apply_mid_sagittal_align = function(
    file, 
    file_mat,
    outfile = NULL,
    retimg = TRUE,
    apply_opts = "",
    force_rpi = TRUE,
    verbose = TRUE) {
  
  outfile = check_outfile(outfile = outfile, retimg = retimg)
  if (force_rpi) {
    rp = rpi_orient_file(file, verbose = verbose)
    img = rp$img
  } else {
    img = checkimg(file)
  }
  
  flip_lr = function(x){
    fsl_swapdim(file = x, a = "-x")
  }
  
  flipped = flip_lr(img)
  
  flirt_apply(
    infile = img, 
    reffile = flipped, 
    initmat = file_mat,
    verbose = verbose,
    opts = apply_opts,
    retimg = FALSE,
    outfile = outfile)
  if (force_rpi) {
    centered = reverse_rpi_orient_file(
      file = outfile, 
      orientation = rp$orientation,
      convention = rp$convention)
  } else {
    centered = outfile
  }
  if (retimg) {
    centered = readnii(centered)
  }
  return(centered)
  
}