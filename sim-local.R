source("run.R")

  #############################################################################
 ##
## For exection on local desktop
library(parallel)
 
#for(x in 1:100)
#{
x <- 8184
  results <- ignite(inputs, x)
  #results <- matrix(results, nrow=1, dimnames=list(x,names(results)))
  save(results, file=paste0("output/run-",x,".Rdata"))
#}

