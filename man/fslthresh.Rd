\name{fslthresh}
\alias{fslthresh}
\title{Threshold an image}
\usage{
fslthresh(file, outfile = NULL, thresh = 0, uthresh = NULL,
  retimg = FALSE, reorient = FALSE, intern = TRUE, opts = "")
}
\arguments{
  \item{file}{(character) filename of image to be
  thresholded}

  \item{outfile}{(character) name of resultant thresholded
  file}

  \item{thresh}{(numeric) threshold (anything below set to
  0)}

  \item{uthresh}{(numeric) upper threshold (anything above
  set to 0)}

  \item{retimg}{(logical) return image of class nifti}

  \item{reorient}{(logical) If retimg, should file be
  reoriented when read in? Passed to
  \code{\link{readNIfTI}}.}

  \item{intern}{(logical) pass to \code{\link{system}}}

  \item{opts}{(character) additional options to be passed
  to fslmaths}
}
\value{
character or logical depending on intern
}
\description{
Threshold an image
}

