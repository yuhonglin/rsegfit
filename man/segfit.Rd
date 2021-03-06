% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rsegfit.R
\name{segfit}
\alias{segfit}
\title{segfit a sequence}
\usage{
segfit(data, smp = 2.3, lb = -6, ub = 6, maxiter = 1000, factr = 5000,
  pgtol = 1e-04)
}
\arguments{
\item{data}{The series to be segfitted}

\item{smp}{The smaller, the more segments will be found}

\item{lb}{lower bound of parameter $b$ of each segment}

\item{ub}{upper bound of parameter $b$ of each segment}

\item{maxiter}{maximum iteration in optimisation}

\item{factr}{maximum function evalution}

\item{pgtol}{tolerance used in optimisation}
}
\value{
A "segfit" object. It contains the params of all
        the segments. For each segment, it contains the
        head index (hi), tail index (ei), parameter $a$ (a)
        parameter $b$ (b), parameter $c$ (c), fitting order
        (order), fitted values (fit), and fitting residuals
        (residual). It also has a attribute "data" for the
        original data
}
\description{
Do segmentation on "data"
}
\details{
"Segment and fit" a sequence. Each sequence will be
described by y=a*x^b+c where x=1:seg.length (order==0)
or x = seg.length:1 (order==1). The segmentation is
to minimise the sum of mse of each segments plus
smp*numSegment.
}
