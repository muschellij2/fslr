\name{fslmerge}
\alias{fslmerge}
\title{Merge images using FSL}
\usage{
fslmerge(infiles, outfile, direction = c("x", "y", "z", "t", "a"),
  intern = TRUE)
}
\arguments{
  \item{infiles}{(character) input filenames}

  \item{outfile}{(character) output filename}

  \item{direction}{(character) direction to merge over, x,
  y, z, t (time), a (auto)}

  \item{intern}{(logical) pass to \code{\link{system}}}
}
\value{
character or logical depending on intern
}
\description{
Merge images using FSL
}

