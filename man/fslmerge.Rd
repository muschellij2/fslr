\name{fslmerge}
\alias{fslmerge}
\title{Merge images using FSL}
\usage{
fslmerge(infiles, direction = c("x", "y", "z", "t", "a"), outfile = NULL,
  retimg = FALSE, reorient = FALSE, intern = TRUE)
}
\arguments{
  \item{infiles}{(character) input filenames}

  \item{direction}{(character) direction to merge over, x,
  y, z, t (time), a (auto)}

  \item{outfile}{(character) output filename}

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
Merge images using FSL
}

