source("run.R")

  #############################################################################
 ##
## ACCRE batch run
args    <- commandArgs(trailingOnly=TRUE)
x       <- as.numeric(args[1])
results <- ignite(inputs, x)
save(results, file=paste0("output/run-",x,".Rdata"))
