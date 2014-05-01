\name{fslsmooth}
\alias{fslsmooth}
\title{Gaussian smooth image using FSL}
\usage{
fslsmooth(file, sigma = 10, mask = NULL, outfile = NULL, retimg = FALSE,
  reorient = FALSE, intern = TRUE)
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
}
\value{
Result from system command, depends if intern is TRUE or
FALSE.
}
\description{
Gaussian smooth image using FSL
}

