% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Misc.R
\name{modOM}
\alias{modOM}
\alias{runMSEhist}
\title{Generate an operating model}
\usage{
modOM(OM, nsim, proyears)

runMSEhist(OM)
}
\arguments{
\item{OM}{An OM object.}

\item{nsim}{The number of simulations in the reduced OM.}

\item{proyears}{The number of projection years in the reduced OM.}
}
\description{
Returns a Hist object with shiny progress bars.
}
\section{Functions}{
\itemize{
\item \code{modOM()}: Modifies an OM with fewer simulations and projection years if desired.

}}
