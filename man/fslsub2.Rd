% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fsl_sub2.R, R/fslhd.R
\name{fsl_sub2}
\alias{fsl_sub2}
\alias{fslsub2}
\title{Subsample image by factor of 2}
\usage{
fsl_sub2(..., outfile = tempfile(fileext = ".nii.gz"), retimg = FALSE)

fslsub2(
  file,
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

\item{outfile}{(character) name of resultant subsampled file}

\item{retimg}{(logical) return image of class nifti}

\item{file}{(character) filename of image to be subsampled}

\item{reorient}{(logical) If retimg, should file be reoriented when read in?
Passed to \code{\link[neurobase]{readnii}}.}

\item{intern}{(logical) pass to \code{\link{system}}}

\item{verbose}{(logical) print out command before running}
}
\value{
character or logical depending on intern
}
\description{
This function calls \code{fslmaths -subsamp2} to subsample an image
and either saves the image or returns an object of class nifti
}
\note{
Functions with underscores have different defaults
and will return an output filename, so to be used for piping
}
\examples{
if (have.fsl()){
system.time({
x = array(rnorm(1e6), dim = c(100, 100, 100))
img = nifti(x, dim= c(100, 100, 100), 
datatype = convert.datatype()$FLOAT32, cal.min = min(x), 
cal.max = max(x), pixdim = rep(1, 4))
subsamp = fslsub2(img, retimg=TRUE)
print(voxdim(subsamp))
})
} 
}
