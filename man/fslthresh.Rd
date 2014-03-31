\name{fslthresh}
\alias{fslthresh}
\title{Threshold an image}
\usage{
fslthresh(file, outfile = file, thresh = 0, intern = TRUE)
}
\arguments{
  \item{file}{(character) filename of image to be
  thresholded}

  \item{outfile}{(character) name of resultant thresholded
  file}

  \item{thresh}{(numeric) threshold (anything below set to
  0)}

  \item{intern}{(logical) pass to \code{\link{system}}}
}
\value{
character or logical depending on intern
}
\description{
Threshold an image
}

