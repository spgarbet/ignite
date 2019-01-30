source("run.R")

  #############################################################################
 ##
## For exection on local desktop
library(parallel)
 
# mclapply(1:24, mc.cores=8, function(x)
# {
#   set.seed(x)
#   sapply(1:n, function(y) simulation(x, y))
# })


x <- 1
results <- ignite(inputs, x)
results <- matrix(results, nrow=1, dimnames=list(x,names(results)))
save(results, file="output/run-1.Rdata")
