% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fslview.R
\name{fslview}
\alias{fslview}
\alias{fsleyes}
\title{Open image in FSLView}
\usage{
fslview(file, intern = TRUE, opts = "", verbose = TRUE, ...)

fsleyes(file, intern = TRUE, opts = "", verbose = TRUE, ...)
}
\arguments{
\item{file}{(character) filename of image to be thresholded}

\item{intern}{(logical) pass to \code{\link{system}}}

\item{opts}{(character) options for FSLView}

\item{verbose}{(logical) print out command before running}

\item{...}{options passed to \code{\link[neurobase]{checkimg}}}
}
\value{
character or logical depending on intern
}
\description{
This function calls \code{fslview} to view an image 
in the FSL viewer
}
\note{
As of FSL version 5.0.10, this is deprecated:
\url{https://fsl.fmrib.ox.ac.uk/fsl/docs/#/development/history/index}
}
