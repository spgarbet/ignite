setwd("./IGNITE")
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

fct_range <- seq(0.75,2,by=0.25)
args <- commandArgs(trailing = TRUE)
num_seed <- as.integer(args[1])
fct <- fct_range[num_seed+1]

options("scipen"=100, "digits"=12)
print("costs")
source("./costs_ICER.R")
inputs$vN <- 200000

out <- NULL
for(i in 0:9) {
  out1 <- NULL
  load(file=paste0("/gpfs23/data/h_imph/gravesj/right/ignite/raw_ignite_SA_",i,"_r.rda"))
    for(I in 0:4) {
      cat("Running ", i, fct, I, "\n")
      s <- cost.qaly(subset(results,strategy==I,fct==fct),inputs) %>% mutate(strategy=I,fct=fct) 
      if(is.null(out1)) { out1 <- s } else  {out1 <- rbind(out1,s)}
      rm(s)
    }
  out1 <- out1 %>%  mutate(name = paste0(name,"_b",i))
  if(is.null(out)) { out <- out1 } else  {out <- rbind(out1,out)}
  rm(out1)
  rm(results)
}
save(out,file=paste0("/gpfs23/data/h_imph/gravesj/right/ignite/cost_ignite_SA",num_seed,"_r.rda"))





