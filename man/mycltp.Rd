% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mycltp.R
\name{mycltp}
\alias{mycltp}
\title{A function for showing the central limit theorem}
\usage{
mycltp(n, iter, lambda = 10, ...)
}
\arguments{
\item{n}{the number of observations in a sample}

\item{iter}{the numeber of samples}

\item{lambda}{the parameter for the poisson distribution}

\item{...}{}
}
\value{
nothing, but creates a plot of the normal distribution of the mean of the samples.
}
\description{
A function for showing the central limit theorem
}
\examples{
mycltp(n=2, iter=10000, lambda=4)
}
