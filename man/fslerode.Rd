% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fsl_erode.R, R/fslhd.R
\name{fsl_erode}
\alias{fsl_erode}
\alias{fslerode}
\title{Erode image using FSL}
\usage{
fsl_erode(..., outfile = tempfile(fileext = ".nii.gz"), retimg = FALSE)

fslerode(
  file,
  outfile = NULL,
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE,
  kopts = "",
  opts = "",
  verbose = TRUE,
  ...
)
}
\arguments{
\item{...}{additional arguments passed to \code{\link[neurobase]{readnii}}.}

\item{outfile}{(character) resultant eroded image name}

\item{retimg}{(logical) return image of class nifti}

\item{file}{(character) image to be eroded}

\item{reorient}{(logical) If retimg, should file be reoriented when read in?
Passed to \code{\link[neurobase]{readnii}}.}

\item{intern}{(logical) to be passed to \code{\link{system}}}

\item{kopts}{(character) options for kernel}

\item{opts}{(character) additional options to be passed to fslmaths}

\item{verbose}{(logical) print out command before running}
}
\value{
Result from system command, depends if intern is TRUE or FALSE.  If 
retimg is TRUE, then the image will be returned.
}
\description{
This function calls \code{fslmaths -ero} to erode an image with either 
the default FSL kernel or the kernel specified in \code{kopts}.  The function
either saves the image or returns an object of class nifti.
}
\note{
Functions with underscores have different defaults
and will return an output filename, so to be used for piping
}
\examples{
if (have.fsl()){
system.time({
dims = c(50, 50, 20)
x = array(rnorm(prod(dims)), dim = dims) 
img = nifti(x, dim= dims, 
datatype = convert.datatype()$FLOAT32, cal.min = min(x), 
cal.max = max(x), pixdim = rep(1, 4))
mask = img > .5
eroded = fslerode(mask, kopts = "-kernel boxv 5", retimg=TRUE)
})
}    
}
