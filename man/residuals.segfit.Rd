% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rsegfit.R
\name{residuals.segfit}
\alias{residuals.segfit}
\title{Extract the residuals from a segfit object}
\usage{
\method{residuals}{segfit}(sf, concat = TRUE)
}
\arguments{
\item{sf}{The segfit object}

\item{concat}{Whether the residuals of different segments will be
concatenated.}
}
\value{
If concat==TRUE, the residuals of different segments
              will be concatenated into one vector. Otherwise,
              the returned value will be a list of residuals
              of each segment.
}
\description{
Extract the residuals from a segfit object
}
\details{
Extract the residuals from a segfit object
}
