#' @title FSL Standard Space ROI
#' @description This masks the input and/or reduces its FOV, on the basis of a
#' standard space image or mask, that is transformed into the space
#' of the input image.
#' 
#' @param file (character) image to be manipulated
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link[neurobase]{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fast}
#' @param verbose (logical) print out command before running
#' @param mask mask output using transformed standard space `"FOV"` (default),
#' `"NONE"` (no mask), or give a mask NIfTI file.
#' @param roi cut down input FOV using bounding box of the transformed standard 
#' space `"FOV"` (default), `"NONE"` (no mask), or give a mask NIfTI file.
#' @param ... additional arguments passed to \code{\link[neurobase]{readnii}}.
#'
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fsl_standard_space_roi = function(
    file,
    outfile = NULL, 
    mask = "FOV",
    roi = "FOV",
    retimg = TRUE,
    reorient = FALSE,
    intern=FALSE, 
    opts = "", 
    verbose = TRUE,
    ...){
  
  cmd = get.fsl()
  file = checkimg(file, ...)
  cmd <- paste0(cmd, "standard_space_roi ")
  no.outfile = is.null(outfile)
  outfile = check_outfile(outfile = outfile, retimg = retimg, fileext = "")
  outfile = nii.stub(outfile)
  
  assertthat::assert_that(
    assertthat::is.string(mask)
  )
  arg_mask = NULL
  if (toupper(mask) %in% c("FOV", "NONE")) {
    arg_mask = paste0("-mask", toupper(mask))
  } else {
    mask = checkimg(mask)
    arg_mask = c("-maskMASK", mask)
  }
  opts = c(opts, arg_mask)
  
  assertthat::assert_that(
    assertthat::is.string(roi)
  )
  arg_mask = NULL
  if (toupper(roi) %in% c("FOV", "NONE")) {
    arg_mask = paste0("-roi", toupper(roi))
  } else {
    roi = checkimg(roi)
    arg_mask = c("-roiMASK", roi)
  }
  opts = c(opts, arg_mask)

  
  opts = trimws(opts)
  opts = opts[ opts != "" ]
  opts = paste(opts, collapse = " ")
  
  cmd <- paste(cmd, sprintf('%s "%s" "%s";', opts, file, outfile))
  ext = get.imgext()
  if (verbose){
    message(cmd, "\n")
  }
  res = system(cmd, intern=intern)  
  if (retimg){
    img = check_nifti(outfile, reorient = reorient)
    return(img)
  }
  return(outfile)
  # return(res)  
}