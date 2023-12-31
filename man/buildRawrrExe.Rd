% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dotNetAssembly.R
\name{buildRawrrExe}
\alias{buildRawrrExe}
\title{Build \code{rawrr.exe} console application.}
\usage{
buildRawrrExe()
}
\value{
the return value of the system2 command.
}
\description{
builds \code{rawrr.exe} file from C# source code requiring 
xbuild or msbuild tools. The console application \code{rawrr.exe}
is used by the package's reader functions through a \link{system2} call.
}
\details{
The rawrr package implementation consists of two language layers,
the top R layer and the hidden C# layer. Specifically, R functions requesting
access to data stored in binary raw files invoke compiled C# wrapper methods
using a \link{system2} call. Calling a wrapper method typically results in the
execution of methods defined in the RawFileReader dynamic link library
provided by Thermo Fisher Scientific. Our precompiled wrapper methods are
bundled in the \code{rawrr.exe} executable file (.NET assembly) and shipped
with the released R package. Running \code{rawrr.exe} requires the
\url{https://www.mono-project.com/} environment on non-Microsoft
operating systems. Mono is a cross platform, open source .NET framework.
On Microsoft Windows the Microsoft .NET framework is typically already
installed and sufficient. Our package also contains the C# source code
\code{rawrr.cs}.
In order to return extracted data back to the R layer we use file I/O.
More specifically, the extracted information is written to a temporary
location on the harddrive, read back into memory and parsed into R  objects.
}
\references{
\itemize{
  \item{\url{https://www.mono-project.com/docs/advanced/assemblies-and-the-gac/}}
  \item{\url{https://planetorbitrap.com/rawfilereader}}
  \item{\url{https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/compiler-options/advanced}}
  \item{\doi{10.1021/acs.jproteome.0c00866}}
}
}
\seealso{
\link{installRawrrExe} and \link{installRawFileReaderDLLs}
}
\author{
Tobias Kockmann, Christian Panse <cp@fgcz.ethz.ch>, 2021
}
