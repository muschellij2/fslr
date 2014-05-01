\name{fslsmooth}
\alias{fslsmooth}
\title{Gaussian smooth image using FSL}
\usage{
fslsmooth(file, sigma = 10, mask = NULL, outfile = NULL, retimg = FALSE,
  reorient = FALSE, intern = TRUE, ...)
}
\arguments{
  \item{file}{(character) image to be smoothed}

  \item{sigma}{(numeric) sigma (in mm) of Gaussian kernel
  for smoothing}

  \item{mask}{(character) optional mask given for image}

  \item{outfile}{(character) resultant smoothed image name
  (optional) if not give, will be the stub of the filename
  then _sigma}

  \item{retimg}{(logical) return image of class nifti}

  \item{reorient}{(logical) If retimg, should file be
  reoriented when read in? Passed to
  \code{\link{readNIfTI}}.}

  \item{intern}{(logical) to be passed to
  \code{\link{system}}}

  \item{...}{additional arguments passed to
  \code{\link{readNIfTI}}.}
}
\value{
Result from system command, depends if intern is TRUE or
FALSE.
}
\description{
This function calls \code{fslmaths -s} to smooth an image
and either saves the image or returns an object of class
nifti
}
\examples{
if (have.fsl()){
system.time({
 img = system.file("MNI152_T1_4mm_brain.nii.gz", package="brainR")
 s.img = fslsmooth(img, retimg=TRUE)
})
}
}

