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

ignite(inputs, 1)