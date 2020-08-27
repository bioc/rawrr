#R

context("spectrum")

library(rawR)

.is.peaklist <- function(x){
	sum(c("scan", "scanType", "rtinseconds", "pepmass", "charge", "mZ", "intensity") %in% names(x))>6
}

test_that("check readSpectrum function names.", {

  rawfile <- file.path(path.package(package = 'rawR'), 'extdata', 'sample.raw')

  S <- readSpectrum(rawfile, 1:22)

  lapply(S, function(x){expect_true(.is.peaklist(x))})

})

test_that("check readSpectrum 23.", {

  rawfile <- file.path(path.package(package = 'rawR'), 'extdata', 'sample.raw')
  S <- readSpectrum(rawfile, 23)[[1]]

  DF <- read.table(file.path(path.package(package = 'rawR'), 'extdata', 'scan23_peakList.txt'), sep="\t", header=TRUE)

  expect_true(sum(S$mZ %in% DF$m.z) >= 720)
  expect_true(sum(S$intensity %in% DF$Intensity) >= 720)
})
