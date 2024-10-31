#R

# Test if \code{rawrr.exe} .NET assembly is working
.isAssemblyWorking <-
  function(FUN = stop, exe = .rawrrAssembly()){
    
    if (isFALSE(file.exists(exe))){
      msg <- c("'rawrr.exe' not found.\n",
               "Run 'rawrr::installRawrrExe()'.",
               " For more information, type '?rawrr.exe'.")
      FUN(msg)
    }
    
    
    # execute rawrr.exe assembly and keep output string
    rvs <-  "?"
    if (file.exists(exe)){
      rvs <- system2(exe, stdout = TRUE)
    }
    
    # expect that output string
    if (rvs != "No RAW file specified!"){
      msg <- ("The 'rawrr.exe' dot Net assembly is not working!")
      FUN(msg)
    }
    
    if(interactive()){ stopifnot(.isRawFileReaderLicenseAccepted()) }
    TRUE
  }


## TODO: refactor
.rawfileReaderDLLs <- function(){
  # 'ThermoFisher.CommonCore.BackgroundSubtraction.dll',
  c(
    'ThermoFisher.CommonCore.BackgroundSubtraction.dll',
    'ThermoFisher.CommonCore.Data.dll',
    'ThermoFisher.CommonCore.MassPrecisionEstimator.dll',
    'ThermoFisher.CommonCore.RawFileReader.dll')
}

#' Derives the path where all .NET assemblies are stored.
#'
#' @return path
#' @export rawrrAssemblyPath
#' @seealso \code{installRawFileReaderDLLs} and \code{installRawrrExe}
#'
#' @examples
#' rawrrAssemblyPath()
rawrrAssemblyPath <- function(){
  libdir <- tools::R_user_dir("rawrr", which='cache')
  d <- file.path(libdir, 'rawrrassembly')
  
  if (interactive()){
    if (isFALSE(dir.exists(d))){
      #msg <- sprintf("rawrr .NET assemply path '%s' is not existing!", d)
      #warning(msg)
    }
  }
  return(d)
}


## TODO: refactor
.checkRawFileReaderDLLs <- function(FUN=stop){
  rv <- vapply(.rawfileReaderDLLs(), function(dll){
    userFileDllPath <- file.path(rawrrAssemblyPath(), dll)
    dllExists <- file.exists(userFileDllPath)
    if (isFALSE(dllExists)){
      message(sprintf("'%s' is missing.", dll))
    }
    return(dllExists)
  }, FALSE)
  
  if (isFALSE(all(rv)) && TRUE){
    FUN("'ThermoFisher.CommonCore.*.dll' files are not available on the system.\n",
         "Run 'rawrr::installRawFileReaderDLLs()' or setenv MONO_PATH to ",
         "the location where the assemblies are located.\n",
         "For more information, type '?ThermoFisher'.")
  }
  all(rv)
}


.rawrrAssembly <- function(){
    if (Sys.info()['sysname'] == "Darwin"){
       file.path(rawrr::rawrrAssemblyPath(), 'osx-x64', 'rawrr') -> f
    } else if (Sys.info()['sysname'] == "Linux"){
       file.path(rawrr::rawrrAssemblyPath(), 'linux-x64', 'rawrr') -> f
    } else {
       file.path(rawrr::rawrrAssemblyPath(), 'win-x64', 'rawrr.exe') -> f
    }
    return(f)
}


#' URL for Thermo Fisher .NET assemblies
#'
#' @return an URL
.thermofisherlsmsUrl <- function(){
 #  "https://github.com/thermofisherlsms/RawFileReader/tree/main/Libs/NetCore/Net8/"
    "https://github.com/thermofisherlsms/RawFileReader/raw/refs/heads/main/Libs/NetCore/Net8/"
}


#' Download and install the Thermo Fisher Scientific .NET 8.0 nupkgs
#' 
#' @param sourceUrl url of nupkgs.
#' @param force if \code{TRUE} it will overwrite the pkgs.
#'
#' @aliases Thermo
#' @aliases ThermoFisher
#' @aliases ThermoFisherScientific
#' 
#' @details 
#' The console application assembly \code{rawrr.exe} requires:
#' \itemize{
#' \item {\code{ThermoFisher.CommonCore.Data.dll}, }
#' \item{\code{ThermoFisher.CommonCore.MassPrecisionEstimator.dll}, and}
#' \item{ThermoFisher.CommonCore.RawFileReader.dll}
#' }.
#' 
#' @references \itemize{
#'   \item{\url{https://www.mono-project.com/docs/advanced/assemblies-and-the-gac/}}
#'   \item{\url{https://planetorbitrap.com/rawfilereader}}
#'   \item{\doi{10.1021/acs.jproteome.0c00866}}
#' }
#' 
#' @author Christian Panse <cp@fgcz.ethz.ch>, 2021, 2024
#' 
#' @return An (invisible) vector of integer code, 0 for success and non-zero for
#' failure. For the "wget" and "curl" methods this is the status code returned
#' by the external program.
#'
#' @importFrom utils download.file
.downloadNupkgs <- function(sourceUrl = .thermofisherlsmsUrl(), force = TRUE){
    
    rawfileReaderDLLsPath <- rawrrAssemblyPath()

    if (isFALSE(dir.exists(rawfileReaderDLLsPath))){
      dir.create(rawfileReaderDLLsPath, recursive = TRUE)
    }
    
    if (isTRUE(dir.exists(rawfileReaderDLLsPath))){
      msg <- sprintf("removing nupkgs files in directory '%s'", rawfileReaderDLLsPath)
      message(msg)
      
      file.remove(file.path(rawrrAssemblyPath(),
                            list.files(rawrrAssemblyPath(), pattern="\\.nupkg$")))
    }
    
    c('ThermoFisher.CommonCore.BackgroundSubtraction.8.0.6.nupkg',
      'ThermoFisher.CommonCore.RandomAccessReaderPlugin.8.0.6.nupkg', 
      'ThermoFisher.CommonCore.Data.8.0.6.nupkg',
      'ThermoFisher.CommonCore.RawfileReader.8.0.6.nupkg',
      'ThermoFisher.CommonCore.MassPrecisionEstimator.8.0.6.nupkg') |> vapply(FUN = function(nupkg){
      destfile <- file.path(rawfileReaderDLLsPath, nupkg)
      download.file(file.path(sourceUrl, nupkg),
                    destfile = destfile, mode='wb')
    }, 0) -> rv

    rv
  }

#' dotnet nuget add source /Users/cp/Library/Caches/org.R-project.R/R/rawrr/rawrrassembly/
#' dotnet nuget remove source "Package source 1"
#' dotnet nuget list source   
#' dotnet add package ThermoFisher.CommonCore.MassPrecisionEstimator
.addNupkgSource <- function(){
}

.addPackages <- function(){
}

#' Download and install the \code{rawrr.exe} console application
#' 
#' @description downloads and installs the \code{rawrr.exe} .NET assembly in 
#' the directory provided by \code{rawrrAssemblyPath()}.
#' 
#' @details The console application \code{rawrr.exe} is used by the package's
#' reader functions through a \link{system2} call.
#' 
#' @param sourceUrl url of \code{rawrr.exe} assembly.
#' @param force if \code{TRUE} it will overwrite the assembly
#' @param ... other parameter for \code{download.file}.
#'
#' @return An integer code, 0 for success and non-zero for
#' failure. For the "wget" and "curl" methods this is the status code returned
#' by the external program.
#' @seealso \link{buildRawrrExe}
#' @references \doi{10.1021/acs.jproteome.0c00866}
#' @aliases rawrr.exe
#' @export installRawrrExe
installRawrrExe <-
  function (sourceUrl = "https://fgcz-ms.uzh.ch/~cpanse/rawrr/dotnet/",
            force = FALSE,
            ...) {
  rawrrAssembly <- .rawrrAssembly()
  if (file.exists(rawrrAssembly) && isFALSE(force)){
	## TODO: if interactive ask to override
        if (interactive()){
            response <- readline(prompt = sprintf("Assembly exists. Do you want to overwrite it? [Y/n]: "))
            if (tolower(response) == "y"){
                 
            }else{
                return()
            }
	}
  }

  if (isFALSE(dir.exists(rawrrAssemblyPath()))) {
        dir.create(rawrrAssemblyPath(), recursive = TRUE)
  }

  if (Sys.info()["sysname"] == "Darwin") {
            sourceUrl <- file.path(sourceUrl, "osx-x64", "rawrr")
  }
  else if (Sys.info()["sysname"] == "Linux") {
            sourceUrl <- file.path(sourceUrl, "linux-x64", "rawrr")
  }
  else {
            sourceUrl <- file.path(sourceUrl, "win-x64", "rawrr.exe")
  }
  message("Overwrite sourceUrl to ", sourceUrl)


  dir.create(dirname(rawrrAssembly), recursive = TRUE, showWarnings = FALSE)
  rv = download.file(sourceUrl, destfile = rawrrAssembly, mode = "wb", 
        ...)
  Sys.chmod(rawrrAssembly, mode = "0777", use_umask = TRUE)

  message(sprintf("MD5 %s %s", tools::md5sum(rawrrAssembly), rawrrAssembly))

  rv
}

.buildOnLoad <- function(){
  return()

  ## TODO:
  
  # nothing to do
  if (file.exists(.rawrrAssembly())){
    return()
  }
  
  # check Thermo DLLs
  if(isFALSE(.checkRawFileReaderDLLs(message))){
    return()
  }

  if (Sys.which("msbuild") == "" && Sys.which("xbuild") == "")
  {
    msg <- c("Could not find 'msbuild' or 'xbuild' in the path. Therefore, ",
         "it is not possible to build the 'rawrr.exe' assembly from",
         " source code.\nTry to run rawrr::installRawrrExe().")
    message(msg)
    return()
  }

  buildRawrrExe()
}


## TODO: make it work for dotnet
#' Build \code{rawrr.exe} console application.
#' 
#' @description builds \code{rawrr.exe} file from C# source code requiring 
#' xbuild or msbuild tools. The console application \code{rawrr.exe}
#' is used by the package's reader functions through a \link{system2} call.
#' 
#' @details The rawrr package implementation consists of two language layers,
#' the top R layer and the hidden C# layer. Specifically, R functions requesting
#' access to data stored in binary raw files invoke compiled C# wrapper methods
#' using a \link{system2} call. Calling a wrapper method typically results in the
#' execution of methods defined in the RawFileReader dynamic link library
#' provided by Thermo Fisher Scientific. Our precompiled wrapper methods are
#' bundled in the \code{rawrr.exe} executable file (.NET assembly) and shipped
#' with the released R package. Running \code{rawrr.exe} requires the
#' \url{https://www.mono-project.com/} environment on non-Microsoft
#' operating systems. Mono is a cross platform, open source .NET framework.
#' On Microsoft Windows the Microsoft .NET framework is typically already
#' installed and sufficient. Our package also contains the C# source code
#' \code{rawrr.cs}.
#' In order to return extracted data back to the R layer we use file I/O.
#' More specifically, the extracted information is written to a temporary
#' location on the harddrive, read back into memory and parsed into R  objects.
#' 
#' @author Tobias Kockmann, Christian Panse <cp@fgcz.ethz.ch>, 2021
#' 
#' @seealso \link{installRawrrExe} and \link{installRawFileReaderDLLs}
#' 
#' @references \itemize{
#'   \item{\url{https://www.mono-project.com/docs/advanced/assemblies-and-the-gac/}}
#'   \item{\url{https://planetorbitrap.com/rawfilereader}}
#'   \item{\url{https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/compiler-options/advanced}}
#'   \item{\doi{10.1021/acs.jproteome.0c00866}}
#' }
#' 
#' @return the return value of the system2 command.
#' @export buildRawrrExe
buildRawrrExe <- function(){
  packagedir <- system.file(package = 'rawrr')
 
  if (isFALSE(dir.exists(rawrrAssemblyPath()))){
    dir.create(rawrrAssemblyPath(), recursive = TRUE)
  }
  
  if (isFALSE(.checkRawFileReaderDLLs())){
    return()
  }
  

  
  if (Sys.which("msbuild") == "" && Sys.which("xbuild") == "")
  {
    msg <- c("Could not find 'msbuild' or 'xbuild' in the path. Therefore, ",
         "it is not possible to build the 'rawrr.exe' assembly from",
         " source code.\nTry to run rawrr::installRawrrExe().")
    stop(msg)
  }
  
  cwd <- getwd()
  setwd(file.path(packagedir, 'rawrrassembly'))
  
  cmd <- ifelse(Sys.which("msbuild") != "", "msbuild", "xbuild")

  # https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/compiler-options/advanced#additionallibpaths
  additionalLibPath <- .determineAdditionalLibPath()
  
  buildLog <- tempfile("rawrr_build.log.",
                       tmpdir = rawrrAssemblyPath())
  
  cmdArgs <- sprintf("/p:OutputPath=%s/ /p:AdditionalLibPaths=%s /v:diagnostic /flp:LogFile=%s rawrr.csproj",
                     shQuote(rawrrAssemblyPath()),
                     shQuote(additionalLibPath),
                     shQuote(buildLog))

  message("Attempting to build 'rawrr.exe', one time setup ...")
  rv <- system2 (cmd, cmdArgs, wait=TRUE, stderr=TRUE, stdout=TRUE)
  
  if (rv <- any(grepl("Build succeeded.", rv))
      && file.exists(.rawrrAssembly())){
    msg <- sprintf("'rawrr.exe' successfully built in \n'%s'.
The build report should have been saved in\n'%s'.", .rawrrAssembly(), buildLog)
    message(msg)
  }else{
    err <- sprintf("Building 'rawrr.exe' failed. For details see the build report, supposed to be saved in:
'%s'
Call 'rawrr::installRawrrExe()' to download and install a precompiled version
from a remote location. Note this requires internet connection.",
                   buildLog)
    setwd(cwd)
    stop(err)
  }
  setwd(cwd)
  rv
}

.eulaPath <- function(){
  file.path(rawrrAssemblyPath(), "eula.txt")
}

.isRawFileReaderLicenseAccepted <- function(){
  licenseFile <- file.path(system.file(package = 'rawrr'), 'rawrrassembly',
                           'RawFileReaderLicense.txt')
  stopifnot(file.exists(licenseFile))
  
  eulaFile <- .eulaPath()
  
  msg <- c("# By changing the setting below to TRUE you are accepting ",
           "the Thermo License agreement.")
  
  if (!file.exists(eulaFile)){
    file.show(licenseFile)
    fmt <- "Do you accept the Thermo License agreement '%s'? [Y/n]: "
    prompt <- sprintf(fmt, licenseFile)
    response <- readline(prompt = prompt)
    if (tolower(response) == "y"){
      if (isFALSE(dir.exists(dirname(eulaFile)))) {
        dir.create(dirname(eulaFile), recursive = TRUE)
      }
      fileConn <- file(eulaFile)
      writeLines(paste(msg, paste0("# ", date()), "eula=true", sep="\n"),
                 fileConn)
      close(fileConn)
      
      return(TRUE %in% grepl("eula=true", tolower(readLines(eulaFile))))
    }
  }else{
    return(TRUE %in% grepl("eula=true", tolower(readLines(eulaFile))))
  }
  
  msg <- ("You have to accept the Thermo Fisher Scientific License agreement!")
  stop(msg)
}

