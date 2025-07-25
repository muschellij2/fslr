% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fsl_merge.R, R/fslhd.R
\name{fsl_merge}
\alias{fsl_merge}
\alias{fslmerge}
\title{Merge images using FSL}
\usage{
fsl_merge(..., outfile = tempfile(fileext = ".nii.gz"), retimg = FALSE)

fslmerge(
  infiles,
  direction = c("x", "y", "z", "t", "a"),
  outfile = NULL,
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{...}{additional arguments passed to \code{\link[neurobase]{readnii}}.}

\item{outfile}{(character) output filename}

\item{retimg}{(logical) return image of class nifti}

\item{infiles}{(character) input filenames}

\item{direction}{(character) direction to merge over, x, y, z, 
t (time), a (auto)}

\item{reorient}{(logical) If retimg, should file be reoriented when read in?
Passed to \code{\link[neurobase]{readnii}}.}

\item{intern}{(logical) pass to \code{\link{system}}}

\item{verbose}{(logical) print out command before running}
}
\value{
character or logical depending on intern
}
\description{
This function calls \code{fslmerge} to merge files on some dimension
and either saves the image or returns an object of class nifti
}
\note{
Functions with underscores have different defaults
and will return an output filename, so to be used for piping
}
