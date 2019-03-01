library(stringr)

target <- "output/20190301"

# Helper function for repeated operation over files.
load.results <- function(target="output", verbose=FALSE)
{
  files <- dir(target)
  files <- paste0(target, "/", files[grepl("^run", files)])
  
  do.call(rbind, lapply(files, function(f) {
    if(verbose) cat("Loading ", f, "\n")
    load(f)
    
    results
  }))
}
results <- data.frame(load.results(verbose=TRUE, target=target))
results$run <- as.numeric(gsub(".*run-?([0-9]+)\\.Rdata", "\\1", dir(target), perl=TRUE))
save(results, file=paste0("results-",substr(target, 8, 15),".Rdata"))
rm(target)
