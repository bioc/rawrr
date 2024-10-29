# rawrr.exe commandline options


## Index

```
rawrr.exe <rawfile> index
```

## Chromatograms

```
rawrr.exe <rawfile> chromatogram <scan filter> <output file>
```

Example:

```
rawrr.exe <rawfile> chromatogram 
```


## Howto build

```
dotnet publish rawrr-dotnet.csproj --os osx -a x64 --output /Users/cp/Library/Caches/org.R-project.R/R/rawrr/rawrrassembly/osx-x64
dotnet publish rawrr-dotnet.csproj --os win -a x64 --output /Users/cp/Library/Caches/org.R-project.R/R/rawrr/rawrrassembly/win-x64
dotnet publish rawrr-dotnet.csproj --os linux -a x64 --output /Users/cp/Library/Caches/org.R-project.R/R/rawrr/rawrrassembly/linux-x64
```
