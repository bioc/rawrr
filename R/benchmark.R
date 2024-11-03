#R

#' @importFrom utils object.size
# f <- "/Users/cp/Library/Caches/org.R-project.R/R/ExperimentHub/46314c3933e2_4590.raw"
.benchmark <- function(f){
   stopifnot(file.exists(f))

   rawrr::readFileHeader(f)$`Number of scans` -> n 

   2**(seq(0, floor(log(n, 2)))) |>
            lapply(FUN = function(i){
                sample(n, size = i) |> sort() -> idx
                message("Reading ", i, "random scans from ", f)
		start.time <- Sys.time()
		rawrr::readSpectrum(f, scan = idx) -> S
		end.time <- Sys.time()
                message("in ", end.time - start.time)
		data.frame(count = i,
                    size = object.size(S) |> as.integer(),
                    runTimeInSec = as.double(difftime(end.time, start.time, units='secs')))
	}) |> Reduce(f = rbind)
}
