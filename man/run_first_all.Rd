% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_first_all.R
\name{run_first_all}
\alias{run_first_all}
\title{Run FIRST All}
\usage{
run_first_all(
  img,
  oprefix = tempfile(),
  brain_extracted = FALSE,
  structures = NULL,
  affine = NULL,
  opts = "",
  verbose = TRUE
)
}
\arguments{
\item{img}{specifies the input image (T1-weighted)}

\item{oprefix}{specifies the output image basename (extensions will be added to this)}

\item{brain_extracted}{specifies that the input image has been brain extracted}

\item{structures}{a restricted set of structures to be segmented}

\item{affine}{specifies the affine registration matrix to standard space (optional)}

\item{opts}{(character) operations to be passed to \code{run_first_all}}

\item{verbose}{(logical) print out command before running}
}
\value{
List of results, including result of \code{\link{system}}
and some output files
}
\description{
Wrapper for \code{run_first_all} from FSL for FIRST analysis 
segmentation of subcortical structures
}
