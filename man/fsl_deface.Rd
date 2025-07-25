% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fsl_deface.R
\name{fsl_deface}
\alias{fsl_deface}
\title{Tool to deface a structural T1w image.}
\usage{
fsl_deface(
  file,
  outfile = NULL,
  retimg = TRUE,
  opts = "",
  deface_cropped = FALSE,
  bet_fractional_intensity = NULL,
  bias_correct = FALSE,
  shift_xyz = NULL,
  cog_xyz = NULL,
  reorient = FALSE,
  intern = FALSE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{file}{(character) input image to estimate edge strength}

\item{outfile}{(character) resultant image name (optional)}

\item{retimg}{(logical) return image of class nifti}

\item{opts}{(character) operations to be passed to \code{fsl_deface}}

\item{deface_cropped}{apply the defacing to the cropped image instead of the original image}

\item{bet_fractional_intensity}{fractional intensity for bet (0->1); default=0.5;}

\item{bias_correct}{Bias-correct the input image (with fast);}

\item{shift_xyz}{Shift, in mm, x-, y- and z-directions, to shift face mask by;}

\item{cog_xyz}{centre-of-gravity for bet (voxels, not mm);}

\item{reorient}{(logical) If retimg, should file be reoriented 
when read in?
Passed to \code{\link[neurobase]{readnii}}.}

\item{intern}{(logical) to be passed to \code{\link{system}}}

\item{verbose}{print diagnostic messages}

\item{...}{additional arguments passed to \code{\link{fslcmd}}.}
}
\description{
Tool to deface a structural T1w image.
}
\examples{
\donttest{
if (have_fsl()) {
  file = mni_fname(mm = 1, brain = FALSE)
  out = fsl_deface(file, retimg = FALSE)
}
}
}
