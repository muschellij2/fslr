#' Face Removal Mask
#'
#' @param file input image 
#' @param template Template image  to register input image to.  Set 
#' to \code{NULL} if want to use from 
#' \url{https://github.com/poldracklab/pydeface}.
#' Alternatively, use \code{\link{mni_fname}}.
#' @param face_mask Mask of image, in same space as \code{template}.
#' Set to \code{NULL} if want to use from 
#' \url{https://github.com/poldracklab/pydeface}. 
#' Alternatively, use \code{\link{mni_face_fname}}.
#' @param outfile Output 
#' @param cost Cost function passed to \code{flirt}
#' @param retimg (logical) return image of class nifti
#' @param ... not used
#'
#' @return An image or filename depending on \code{retimg}
#' @export
#'
#' @examples
#' \donttest{
#' if (have_fsl()) {
#'    file = "~/Downloads/sample_T1_input.nii.gz"
#'    if (file.exists(file)) {
#'        mask = face_removal_mask(file = file,
#'          template = NULL, face_mask = NULL)
#'        image = fslmask(file, mask)
#'    }
#' }
#' }
face_removal_mask = function(
  file, 
  template = mni_fname(mm = "1"), 
  face_mask = mni_face_fname(mm = "1"),
  outfile = NULL,
  cost="mutualinfo",
  retimg = FALSE)  {
  
  if (is.null(template) & is.null(face_mask)) {
    base_url = "https://github.com/poldracklab/pydeface/raw/master/pydeface/data"
    face_mask = tempfile(fileext = ".nii.gz")
    download.file(file.path(base_url, "facemask.nii.gz"), 
                  destfile = face_mask)
    template = tempfile(fileext = ".nii.gz")
    download.file(file.path(base_url, "mean_reg2mean.nii.gz"), 
                  destfile = template)
  }
  
  
  # file = "~/Downloads/sample_T1_input.nii.gz"
  omat = tempfile(fileext = ".mat")
  opts = c(paste0("-cost ", cost))
  
  flirt(infile = template, 
        reffile = file, 
        outfile = tempfile(),
        opts = opts, 
        dof = 12,
        omat = omat,
        retimg = FALSE)
  opts = c("-interp nearestneighbour")
  if (is.null(outfile)) {
    outfile = tempfile(fileext = ".nii.gz")
  }
  face_mask = check_nifti(face_mask)
  face_mask = face_mask + 1
  mask = flirt_apply(
    infile = face_mask, 
    reffile = file, 
    initmat = omat,
    opts = opts,
    outfile = outfile,
    retimg = TRUE)
  mask = mask != 1
  writenii(mask, outfile)
  if (retimg) {
    return(mask)
  }
  return(outfile)
}

#' @export
#' @rdname face_removal_mask
deface_image = function(file, ...) {
  
  args = list(...)
  retimg  = args$retimg
  if (is.null(retimg)) {
    retimg = FALSE
  }
  args$file = file
  args$retimg = TRUE
  mask = do.call(face_removal_mask, args = args)
  out = fsl_mask(file = file, mask = mask, retimg = retimg)
  out
}