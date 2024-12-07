% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dotNetAssembly.R
\name{.downloadNupkgs}
\alias{.downloadNupkgs}
\alias{Thermo}
\alias{ThermoFisher}
\alias{ThermoFisherScientific}
\title{Download and install the Thermo Fisher Scientific .NET 8.0 nupkgs}
\usage{
.downloadNupkgs(sourceUrl = .thermofisherlsmsUrl(), force = TRUE)
}
\arguments{
\item{sourceUrl}{url of nupkgs.}

\item{force}{if \code{TRUE} it will overwrite the pkgs.}
}
\value{
An (invisible) vector of integer code, 0 for success and non-zero for
failure. For the "wget" and "curl" methods this is the status code returned
by the external program.
}
\description{
Download and install the Thermo Fisher Scientific .NET 8.0 nupkgs
}
\author{
Christian Panse <cp@fgcz.ethz.ch>, 2021, 2024
}
