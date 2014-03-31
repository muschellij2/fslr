\name{fslfill}
\alias{fslfill}
\title{Fill image holes}
\usage{
fslfill(file, outfile = file, bin = TRUE, intern = TRUE)
}
\arguments{
  \item{file}{(character) filename of image to be filled}

  \item{outfile}{(character) name of resultant filled file}

  \item{bin}{(logical) binarize the image before filling}

  \item{intern}{(logical) pass to \code{\link{system}}}
}
\value{
character or logical depending on intern
}
\description{
Fill image holes
}

