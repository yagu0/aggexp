\name{aggexp-package}
\alias{aggexp-package}
\alias{aggexp}
\docType{package}

\title{
	\packageTitle{aggexp}
}

\description{
	\packageDescription{aggexp}
}

\details{
	The package devtools should be useful in development stage, since we rely on testthat for
	unit tests, and roxygen2 for documentation. knitr is used to generate the package vignette.

	The main entry point is located in R/z_runAlgorithm.R, and take threee parameters:
	\itemize{
		\item{the algorithm (short) name,}
		\item{the list of stations dataframes,}
		\item{the vector of experts names.}
	}
}

\author{
	\packageAuthor{aggexp}

	Maintainer: \packageMaintainer{aggexp}
}

%\references{
%	TODO: Literature or other references for background information
%}

%\examples{
%	TODO: simple examples of the most important functions
%}
