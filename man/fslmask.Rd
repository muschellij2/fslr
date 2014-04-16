\name{fslmask}
\alias{fslmask}
\title{Mask image using FSL}
\usage{
fslmask(file, mask = NULL, outfile = NULL, intern = TRUE, opts = "")
}
\arguments{
  \item{file}{(character) image to be masked}

  \item{mask}{(character) mask given for image}

  \item{outfile}{(character) resultant masked image name
  (optional)}

  \item{intern}{(logical) to be passed to
  \code{\link{system}}}

  \item{opts}{(character) additional options to be passed
  to fslmask}
}
\value{
Result from system command, depends if intern is TRUE or
FALSE.
}
\description{
Mask image using FSL
}

