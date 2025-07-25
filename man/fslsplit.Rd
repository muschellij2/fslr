% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fslsplit.R
\name{fslsplit}
\alias{fslsplit}
\alias{fsl_split}
\title{Split images using FSL}
\usage{
fslsplit(
  infile,
  direction = c("t", "x", "y", "z"),
  output_basename = NULL,
  retimg = TRUE,
  reorient = FALSE,
  verbose = TRUE
)

fsl_split(..., retimg = FALSE)
}
\arguments{
\item{infile}{(character) input filename}

\item{direction}{(character) direction to split over: t (time), x, y, z}

\item{output_basename}{(character) prefix to have for output}

\item{retimg}{(logical) return image of class nifti}

\item{reorient}{(logical) If retimg, should file be reoriented when read in?
Passed to \code{\link[neurobase]{readnii}}.}

\item{verbose}{(logical) print out command before running}

\item{...}{not used}
}
\value{
List of output files
}
\description{
This function calls \code{fslsplit} to merge files on some dimension
and either saves the image or returns an object of class nifti
}
\note{
Functions with underscores have different defaults
and will return an output filename, so to be used for piping
}
