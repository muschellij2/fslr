\name{get.fsl}
\alias{get.fsl}
\title{Get FSL's Location}
\usage{
get.fsl()
}
\value{
NULL if FSL in path, or bash code for setting up FSL DIR
}
\description{
Finds the FSLDIR from system environment or
\code{getOption("fsl.path")} for location of FSL fuctions
}

