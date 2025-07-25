% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fsl_maths.R, R/fslhd.R
\name{fsl_maths}
\alias{fsl_maths}
\alias{fslmaths}
\title{FSL Maths}
\usage{
fsl_maths(..., outfile = tempfile(fileext = ".nii.gz"), retimg = FALSE)

fslmaths(
  file,
  outfile = NULL,
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE,
  opts = "",
  verbose = TRUE,
  ...
)
}
\arguments{
\item{...}{additional arguments passed to \code{\link[neurobase]{readnii}}.}

\item{outfile}{(character) resultant image name (optional)}

\item{retimg}{(logical) return image of class nifti}

\item{file}{(character) image to be manipulated}

\item{reorient}{(logical) If retimg, should file be reoriented when read in?
Passed to \code{\link[neurobase]{readnii}}.}

\item{intern}{(logical) to be passed to \code{\link{system}}}

\item{opts}{(character) operations to be passed to \code{fslmaths}}

\item{verbose}{(logical) print out command before running}
}
\value{
If \code{retimg} then object of class nifti.  Otherwise,
Result from system command, depends if intern is TRUE or FALSE.
}
\description{
This function calls \code{fslmaths}
}
\note{
Functions with underscores have different defaults
and will return an output filename, so to be used for piping
}
