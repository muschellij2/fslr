#' @title Register EPI images to Structural image
#' @description This function calls \code{epi_reg}, designed to 
#' register EPI images (typically functional or diffusion) to structural 
#' (e.g. T1-weighted) image.  
#' @param epi EPI image, character or nifti object
#' @param t1 whole head T1 image , character or nifti object
#' @param t1_brain brain extracted T1 image
#' @param outfile output registered image filename 
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fslmaths}
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @param fmap fieldmap image (in rad/s)
#' @param fmap_mag fieldmap magnitude image - whole head extracted
#' @param fmap_mag_brain  fieldmap magnitude image - brain extracted
#' @param echo_spacing Effective EPI echo spacing 
#' (sometimes called dwell time) - in seconds
#' @param phase_enc_dir phase encoding direction, dir = x/y/z/-x/-y/-z
#' @param weight weighting image (in T1 space)
#' @param verbose (logical) print out command before running
#' 
#' @export
fslepi_reg = function(
  epi,
  t1,
  t1_brain,
  outfile = NULL,
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  fmap = NULL,
  fmap_mag = NULL,
  fmap_mag_brain = NULL,
  echo_spacing = NA,
  phase_enc_dir = c("x", "y", "z", "-x", "-y", "-z"),
  weight = NULL,
  verbose = TRUE,
  opts = "", 
  ...){
  

  checkimg_char = function(x) {
    if (is.null(x)) {
      return("")
    }
    return(checkimg(x))
  }
  
  outfile = check_outfile(outfile =  outfile, 
                          retimg = retimg, 
                          fileext = ".nii.gz")
  
  
  all_imgs = list(
    epi = epi,
    t1 = t1,
    t1brain = t1_brain,
    fmap = fmap,
    fmapmag = fmap_mag,
    fmapmagbrain = fmap_mag_brain,
    weight = weight
  )

  n_imgs = names(all_imgs)
  all_imgs = vapply(all_imgs, checkimg_char, FUN.VALUE = "")
  names(all_imgs) = paste0("--", n_imgs, "=")
  all_imgs = trimws(all_imgs)
  all_imgs = all_imgs[ all_imgs != "" ]
  all_imgs = shQuote(all_imgs)
  all_imgs = paste0(names(all_imgs), all_imgs)
  
  if (verbose) {
    opts = c(opts, "-v")
  }
  
  ####################################
  # Phase Encoding Direction
  ####################################
  if (!missing(phase_enc_dir)) {
    phase_enc_dir = match.arg(phase_enc_dir)
    phase_enc_dir = paste0("--pedir=", phase_enc_dir)
    opts = c(opts, phase_enc_dir)
  } 
  # else {
    # pedir = ""
  # }
  if (!is.na(echo_spacing)) {
    echo_spacing = paste0("--echospacing=", echo_spacing)
    opts = c(opts, echo_spacing)
  }
  
  opts = c(all_imgs, opts)

  opts = trimws(opts)
  opts = opts[ opts != "" ]
  opts = paste(opts, collapse = " ")
  
  cmd = get.fsl()
  cmd <- paste(cmd, opts)
  
  outfile = nii.stub(outfile)
  ext = get.imgext()
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd, intern = intern)
  outfile = paste0(outfile, ext)  
  if (retimg) {
    img = readnii(outfile, reorient = reorient, ...)
    return(img)
  }
  return(res)    
}

#' @rdname fslepi_reg
#' @aliases fsl_epi_reg
#' @export
#' @note Functions with underscores have different defaults
#' and will return an output filename, so to be used for piping
fsl_epi_reg = function(
  ...,
  outfile = tempfile(fileext = ".nii.gz"),
  retimg = FALSE
) {
  fslepi_reg(..., outfile = outfile, retimg = retimg)
  return(outfile)
}

#' @rdname fslepi_reg
#' @aliases epi_reg
#' @export
epi_reg = fsl_epi_reg
