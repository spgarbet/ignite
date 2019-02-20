library(stringr)

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
results <- data.frame(load.results(verbose=TRUE, target="output/20190219"))
results$run <- as.numeric(gsub(".*run-?([0-9]+)\\.Rdata", "\\1", dir("output/20190219"), perl=TRUE))

