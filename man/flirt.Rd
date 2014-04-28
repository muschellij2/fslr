\name{flirt}
\alias{flirt}
\title{Register using FLIRT}
\usage{
flirt(infile, outfile, reffile, omat, dof, opts = "", intern = TRUE)
}
\arguments{
  \item{infile}{(character) input filename}

  \item{outfile}{(character) output filename}

  \item{reffile}{(character) reference image to be
  registered to}

  \item{omat}{(character) Output matrix name}

  \item{dof}{(numeric) degrees of freedom}

  \item{opts}{(character) additional options to FLIRT}

  \item{intern}{(logical) pass to \code{\link{system}}}
}
\value{
character or logical depending on intern
}
\description{
Register using FLIRT
}

