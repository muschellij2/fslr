\name{fslfill}
\alias{fslfill}
\title{Fill image holes}
\usage{
fslfill(file, outfile = NULL, bin = TRUE, retimg = FALSE,
  reorient = FALSE, intern = TRUE)
}
\arguments{
  \item{file}{(character) filename of image to be filled}

  \item{outfile}{(character) name of resultant filled file}

  \item{bin}{(logical) binarize the image before filling}

  \item{retimg}{(logical) return image of class nifti}

  \item{reorient}{(logical) If retimg, should file be
  reoriented when read in? Passed to
  \code{\link{readNIfTI}}.}

  \item{intern}{(logical) pass to \code{\link{system}}}
}
\value{
character or logical depending on intern
}
\description{
This function calls \code{fslmaths -fillh} to fill in image
holes and either saves the image or returns an object of
class nifti
}

