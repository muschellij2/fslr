\name{fslview}
\alias{fslview}
\title{Open image in FSLView}
\usage{
fslview(file, intern = TRUE, opts = "")
}
\arguments{
  \item{file}{(character) filename of image to be
  thresholded}

  \item{intern}{(logical) pass to \code{\link{system}}}

  \item{opts}{(character) options for FSLView}
}
\value{
character or logical depending on intern
}
\description{
This function calls \code{fslview} view an image in the FSL
viewer
}

