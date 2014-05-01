\name{fslmask}
\alias{fslmask}
\title{Mask image using FSL}
\usage{
fslmask(file, mask = NULL, outfile = NULL, retimg = FALSE,
  reorient = FALSE, intern = TRUE, opts = "", ...)
}
\arguments{
  \item{file}{(character) image to be masked}

  \item{mask}{(character) mask given for image}

  \item{outfile}{(character) resultant masked image name}

  \item{retimg}{(logical) return image of class nifti}

  \item{reorient}{(logical) If retimg, should file be
  reoriented when read in? Passed to
  \code{\link{readNIfTI}}.}

  \item{intern}{(logical) to be passed to
  \code{\link{system}}}

  \item{opts}{(character) additional options to be passed
  to fslmask}

  \item{...}{additional arguments passed to
  \code{\link{readNIfTI}}.}
}
\value{
Result from system command, depends if intern is TRUE or
FALSE.
}
\description{
This function calls \code{fslmaths -mas} to mask an image
from an image mask and either saves the image or returns an
object of class nifti
}

