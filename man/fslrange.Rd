\name{fslrange}
\alias{fslrange}
\title{Get range of an image}
\usage{
fslrange(file)
}
\arguments{
  \item{file}{(character) filename of image to be checked}
}
\value{
numeric vector of length 2
}
\description{
This function calls \code{fslstats -R} to get the range of
an image
}

