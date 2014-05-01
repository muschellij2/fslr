\name{fslerode}
\alias{fslerode}
\title{Erode image using FSL}
\usage{
fslerode(file, outfile = NULL, retimg = FALSE, reorient = FALSE,
  intern = TRUE, kopts = "", opts = "", ...)
}
\arguments{
  \item{file}{(character) image to be eroded}

  \item{outfile}{(character) resultant eroded image name}

  \item{retimg}{(logical) return image of class nifti}

  \item{reorient}{(logical) If retimg, should file be
  reoriented when read in? Passed to
  \code{\link{readNIfTI}}.}

  \item{intern}{(logical) to be passed to
  \code{\link{system}}}

  \item{kopts}{(character) options for kernel}

  \item{opts}{(character) additional options to be passed
  to fslmaths}

  \item{...}{additional arguments passed to
  \code{\link{readNIfTI}}.}
}
\value{
Result from system command, depends if intern is TRUE or
FALSE.  If retimg is TRUE, then the image will be returned.
}
\description{
This function calls \code{fslmaths -ero} to erode an image
with either the default FSL kernel or the kernel specified
in \code{kopts}.  The function either saves the image or
returns an object of class nifti.
}

