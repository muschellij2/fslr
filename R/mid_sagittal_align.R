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
#'
#' @return Filename of output or nifti depending on \code{retimg}
#' @export
mid_sagittal_align = function(
  file, 
  outfile = NULL,
  retimg = TRUE,
  opts = "",
  verbose = TRUE) {
  
  outfile = check_outfile(outfile = outfile, retimg = retimg)
  rp = rpi_orient_file(file, verbose = verbose)
  
  img = rp$img
  
  flip_lr = function(x){
    fsl_swapdim(file = x, a = "-x")
  }
  
  flipped = flip_lr(img)
  
  omat = tempfile(fileext = ".mat")
  tfile = tempfile(fileext = ".nii.gz")
  flirt(infile = img, 
        reffile = flipped,
        omat = omat, dof = 6,
        opts = opts,
        retimg = FALSE, 
        outfile = tfile,
        verbose = verbose)  
  
  parsed = fsl_avscale(file = omat, parsed = TRUE)
  # parsed = parse_avscale(scaled)
  
  mat = parsed$fwd_half_transform
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
    outfile = tfile)
  centered = reverse_rpi_orient_file(
    file = tfile, 
    orientation = rp$orientation,
    convention = rp$convention)
  if (retimg) {
    centered = readnii(centered)
  }
  
  attr(centered, "half_transform") = new_omat
  attr(centered, "full_transform") = omat
  return(centered)
  
}