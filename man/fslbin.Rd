% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fsl_bin.R, R/fslbin.R
\name{fsl_bin}
\alias{fsl_bin}
\alias{fslbin}
\title{Binarize Image using FSL}
\usage{
fsl_bin(..., outfile = tempfile(fileext = ".nii.gz"), retimg = FALSE)

fslbin(
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

\item{file}{(character) image to be binarized}

\item{reorient}{(logical) If retimg, should file be reoriented when read in?
Passed to \code{\link[neurobase]{readnii}}.}

\item{intern}{(logical) to be passed to \code{\link{system}}}

\item{opts}{(character) operations to be passed to \code{fslmaths}}
}
\value{
If \code{retimg} then object of class nifti.  Otherwise,
Result from system command, depends if intern is TRUE or FALSE.
}
\description{
This function calls \code{fslmaths -bin}.  The R functions wraps
\code{fslmaths}
}
\note{
Functions with underscores have different defaults
and will return an output filename, so to be used for piping
}
\examples{
  set.seed(5)
dims = rep(10, 3)
arr = array(rnorm(prod(dims)), dim = dims)
nim = oro.nifti::nifti(arr)
if (have.fsl()){
 fslbin(nim)
 fsl_bin(nim)
 }

}
