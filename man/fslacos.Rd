% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fsl_acos.R, R/fslacos.R
\name{fsl_acos}
\alias{fsl_acos}
\alias{fslacos}
\title{Arc Cosine Transform Image using FSL}
\usage{
fsl_acos(..., outfile = tempfile(fileext = ".nii.gz"), retimg = FALSE)

fslacos(
  file,
  outfile = NULL,
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE,
  opts = "",
  ...
)
}
\arguments{
\item{...}{additional arguments passed to \code{\link[neurobase]{readnii}}.}

\item{outfile}{(character) resultant image name (optional)}

\item{retimg}{(logical) return image of class nifti}

\item{file}{(character) input image to arc cosine transform}

\item{reorient}{(logical) If retimg, should file be reoriented 
when read in?
Passed to \code{\link[neurobase]{readnii}}.}

\item{intern}{(logical) to be passed to \code{\link{system}}}

\item{opts}{(character) operations to be passed to \code{fslmaths}}
}
\value{
If \code{retimg} then object of class nifti.  Otherwise,
Result from system command, depends if intern is TRUE or FALSE.
}
\description{
This function calls \code{fslmaths -acos}.  
The R functions wraps \code{fslmaths}
}
\note{
Functions with underscores have different defaults
and will return an output filename, so to be used for piping
}
