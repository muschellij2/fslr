\name{fslerode}
\alias{fslerode}
\title{Erode image using FSL}
\usage{
fslerode(file, outfile = NULL, retimg = FALSE, intern = TRUE, opts = "",
  reorient = FALSE, ...)
}
\arguments{
  \item{file}{(character) image to be eroded}

  \item{outfile}{(character) resultant eroded image name}

  \item{retimg}{(logical) Should the result be the eroded
  image?}

  \item{intern}{(logical) to be passed to
  \code{\link{system}}}

  \item{opts}{(character) additional options to be passed
  to fslmaths}

  \item{reorient}{(logical) If retimg, should file be
  reoriented when read in? Passed to
  \code{\link{readNIfTI}}.}

  \item{...}{additional arguments passed to
  \code{\link{readNIfTI}}.}
}
\value{
Result from system command, depends if intern is TRUE or
FALSE.  If retimg is TRUE, then the image will be returned
}
\description{
Erode image using FSL
}

