\name{fslsub2}
\alias{fslsub2}
\title{Subsample image by factor of 2}
\usage{
fslsub2(file, outfile = NULL, retimg = FALSE, reorient = FALSE,
  intern = TRUE)
}
\arguments{
  \item{file}{(character) filename of image to be
  thresholded}

  \item{outfile}{(character) name of resultant subsampled
  file}

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
This function calls \code{fslmaths -subsamp2} to subsample
an image and either saves the image or returns an object of
class nifti
}

