#' Face Removal Mask
#'
#' @param file input image 
#' @param template Template image  to register input image to.  Set 
#' to \code{NULL} (recommended)  if want to use from 
#' \url{https://github.com/poldracklab/pydeface}.
#' Alternatively, use \code{\link{mni_fname}}.
#' @param face_mask Mask of image, in same space as \code{template}.
#' Set to \code{NULL} (recommended) if want to use from 
#' \url{https://github.com/poldracklab/pydeface}. 
#' Alternatively, use \code{\link{mni_face_fname}}.
#' @param outfile Output file name
#' @param dof (numeric) degrees of freedom (default 6 - rigid body)
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
  dof = 12,
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
        dof = dof,
        omat = omat,
        retimg = FALSE)
  opts = c("-interp nearestneighbour")
  if (is.null(outfile)) {
    outfile = tempfile(fileext = ".nii.gz")
  }
  face_mask = check_nifti(face_mask)
  # to avoid anti-aliasing with background
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








#' Face Removal Mask using "Quickshear Defacing for Neuroimages" 
#' (Schimke et al. 2011)
#'
#' @param brain_mask Brain mask image.  If \code{NULL}, then
#' \code{\link{fslbet}} will be run
#' @param file input image - same orientation as brain mask
#' @param buffer buffer to add to intercept for face mask equation
#' @param verbose print diagnostic messages
#' @param ... additional arguments passed to \code{\link{fslmask}}
#' 
#' @note adapted from 
#' \url{https://github.com/nipy/quickshear/blob/master/quickshear.py}
#' 
#' @return A binary image of the non-face areas
#' @export
#'
#' @examples
#' \donttest{
#' if (have_fsl()) {
#'    file = "~/Downloads/sample_T1_input.nii.gz"
#'    if (file.exists(file)) {
#'        res = quickshear_deface_image(file)
#'        brain_mask = fslbet(file) > 0
#'        mask = get_quickshear_mask(brain_mask)
#'        image = fslmask(file, mask)
#'    }
#' }
#' }
get_quickshear_mask = function(brain_mask, buffer = 10, verbose = TRUE) {
  if (verbose) {
    message("Reorienting image to RPI")
  }
  bmask = rpi_orient_file(brain_mask, verbose = verbose > 1)
  bmask_img = bmask$img
  if (verbose) {
    message("Getting 2D Mask")
  }
  nii = readnii(bmask_img)
  # Go through the other dimensions
  mask = apply(nii > 0, 2:3, any)
  # need nifti for FSL
  arr = array(mask, dim = c(1, dim(mask)))
  img = copyNIfTIHeader(img = nii, arr, drop = FALSE)
  
  if (verbose) {
    message("Eroding to get outline")
  }
  ero = fsl_erode(img, verbose = verbose > 1)
  edge = fslsub(file = img, file2 = ero, verbose = verbose > 1)
  # this way because RPI
  edge = edge[1,,]
  
  # Get indices
  inds = which(edge > 0, arr.ind = TRUE)
  r = range(inds[,2])
  midpoint = r[1] + diff(r)/2
  
  
  if (verbose) {
    message("Getting Convex Hull") 
  }
  hull = grDevices::chull(inds)
  # for wraparound
  hull = c(hull, hull[1])
  pts = inds[hull, ]
  # lower hull
  lower = pts[ pts[, 2] <= midpoint, ]
  
  # max right point
  slope_points = which.max(lower[,1])
  if (slope_points == nrow(lower)) {
    warning("something is likely wrong ")
  }
  slope_points = c(slope_points, slope_points + 1)
  
  low = lower[slope_points, ]
  diffs = diff(low)
  slope = diffs[2]/diffs[1]
  
  if (slope <= 0) {
    warning("Slope seems off - should be positive")
  }
  
  # yint = y0 - x0 * slope - buffer
  yint = low[1, 2] - (low[1, 1] * slope) - buffer
  # eqn: y = yint + slope * x
  
  # Making an output 
  out_mask = matrix(TRUE, nrow = nrow(edge), ncol = ncol(edge))
  all_inds = expand.grid(x = 1:nrow(edge), y = 1:ncol(edge))
  all_inds$yy = yint + all_inds$x * slope
  # finding y values past the buffered slope
  all_inds$keep = all_inds$y >= all_inds$yy
  bad = all_inds[!all_inds$keep, c("x", "y")]
  bad = as.matrix(bad)
  out_mask[ bad  ] = FALSE
  
  # apply mask to 3D
  arr = array(dim = dim(nii))
  for (i in 1:dim(nii)[1]) {
    arr[i,,] = out_mask
  }
  # Make NIfTI
  out_img = copyNIfTIHeader(img = nii, arr = arr)
  if (verbose) {
    message("Reorienting image to original orientation")
  }  
  out_img = reverse_rpi_orient(
    out_img, convention = bmask$convention,
    orientation = bmask$orientation,
    verbose = verbose > 1)
  return(out_img)
}

#' @export
#' @rdname get_quickshear_mask
quickshear_deface_image = function(
  file, 
  brain_mask = NULL, 
  buffer = 10,
  verbose = TRUE, ...) {
  if (is.null(brain_mask)) {
    brain_mask = fslbet(file, verbose = verbose > 1) > 0
  }
  mask = get_quickshear_mask(brain_mask, verbose = verbose, 
                             buffer = buffer)
  out = fslmask(file, mask = mask, verbose = verbose > 1, 
                ...)
  out
}