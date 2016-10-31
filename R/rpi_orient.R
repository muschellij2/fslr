#' @title Reorient an Image to RPI orientation
#' @description This function uses \code{fslswapdim} to reorient an image
#' @param file Object of class \code{nifti} or character path
#' @param verbose print diagnostic messages
#' @return List of 3 elements
#' \itemize{
#' \item{\code{img}: }{Reoriented image of class \code{nifti}}
#' \item{\code{convention}: }{Convention (Neurological/Radiological) of original image}
#' \item{\code{orientation}: }{Original image orientations}
#' }
#' @export
rpi_orient = function(file, verbose = TRUE){
  file = checkimg(file)
  forms = getForms(file)
  if (forms$sform_code == 0 & forms$qform_code == 0) {
    stop("Cannot swap dimensions - sform_code and qform_code are 0!")
  }
  if (forms$sform_code != 0) {
    sorient = forms$ssor
  } else {
    sorient = forms$sqor
  }
  ori = fslgetorient(file)
  if (ori == "NEUROLOGICAL") {
    # need to copy because fslorient samefile stuff
    tdir = tempfile()
    dir.create(tdir, showWarnings = verbose)
    tfile = file.path(tdir,
                      basename(file))
    file.copy(file, tfile, overwrite = TRUE)
    # changes from NEUROLOGICAL to RADIOLOGICAL
    file = fslorient(tfile,
                     opts = "-swaporient",
                     retimg = TRUE,
                     verbose = verbose)
  }
  # Changes the data
  file = fslswapdim(file = file,
                    retimg = TRUE,
                    a = "RL", b = "PA", c = "IS",
                    verbose = verbose)
  L = list(img = check_nifti(file),
           convention = ori,
           orientation = sorient)
  return(L)
}

#' @title Reverse Reorientation an Image to RPI orientation
#' @description This function uses \code{fslswapdim} to reorient an image
#' @param file Object of class \code{nifti} or character path
#' @param convention Convention of original image (usually from \code{\link{rpi_orient}})
#' @param orientation Vector of length 3 fromoriginal image 
#' (usually from \code{\link{rpi_orient}})
#' @param verbose print diagnostic messages
#' @return Object of class \code{nifti}
#' @export
reverse_rpi_orient = function(file, 
                              convention = c("NEUROLOGICAL", "RADIOLOGICAL"), 
                              orientation, verbose = TRUE){
  file = checkimg(file)
  stopifnot(length(orientation) == 3)
  convention = match.arg(convention)
  
  if (convention == "NEUROLOGICAL") {   
    file = fslorient(file, 
                       opts = "-swaporient",
                       retimg = TRUE, 
                       verbose = verbose)      
  }
  file = fslswapdim(file = file, 
                      retimg = TRUE, 
                      a = orientation[1], 
                      b = orientation[2], 
                      c = orientation[3], 
                      verbose = verbose)
  return(file)
}