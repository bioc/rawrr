#R

#' Benchmark spectra per second
#' @importFrom utils object.size
#' @examples
#' sampleFilePath() |> rawrr:::.benchmark() -> S
#' plot (count / S$runTimeInSec ~ count, log='xy', data=S,
#'   sub = paste0("Overall runtime took ", round(sum(S$runTimeInSec), 3), " seconds."),
#'   xlab = 'number of random generated scan ids')
#'   main = "benchmark spectra per second")
#' @author Christian Panse 2024-11-05
.benchmark <- function(f){
   stopifnot(file.exists(f))

   rawrr::readFileHeader(f)$`Number of scans` -> n 

   2**(seq(0, floor(log(n, 2)))) |>
            lapply(FUN = function(i){
                sample(n, size = i) |> sort() -> idx
                message("Reading ", i, " random scans from ", f, " ...")
		start.time <- Sys.time()
		rawrr::readSpectrum(f, scan = idx) -> S
		end.time <- Sys.time()
                message("in ", end.time - start.time)
		data.frame(count = i,
                    size = object.size(S) |> as.integer(),
                    runTimeInSec = as.double(difftime(end.time, start.time, units='secs')))
	}) |> Reduce(f = rbind)
}
