![rawRHexSticker](https://github.com/cpanse/rawR/blob/master/rawRcolor10%25.png)

# rawR
R interface for Thermo Fisher Scientifc raw files branched from [rawDiag](https://github.com/fgcz/rawDiag). This package wraps the functionality of the [RawFileReader](https://planetorbitrap.com/rawfilereader) .NET assembly. Within the R environment raw files, spectra and chromatograms are represented by S3 objects. All objects are kept in memory. 

## Install

```{r}
install.packages('http://fgcz-ms.uzh.ch/~cpanse/rawR_0.0.1.tar.gz')
```

## System requirements

### MS Windows

### MacOS

You need to have [Mono](https://www.mono-project.com) installed. Mono is an open source implementation of Microsoft's .NET Framework.

## .Net related R packages

- https://CRAN.R-project.org/package=rDotNet 
- https://github.com/rdotnet/rClr
