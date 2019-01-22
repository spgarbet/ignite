### Single Drug - low Weibull
setwd("./IGNITE")
pkg = list("simmer",
           "data.table",
           "plyr",
           "dplyr",
           "tidyr",
           "reshape2",
           "ggplot2",
           "downloader",
           "msm",
           "quantmod")
invisible(lapply(pkg, require, character.only = TRUE))
rm(list=ls())

args <- commandArgs(trailing = TRUE)
num_seed <- as.integer(args[1])

###Costs
source("./costs_ICER.R")
inputs$vN <- 200000


load(file=paste0("/gpfs23/data/h_imph/gravesj/right/raw_ignite_SA_",num_seed,"_p.rda"))
out <- NULL
for(cc in seq(50, 250, by=25)) {
        inputs$costs$single_test <- cc
        
        for(i in 0:4) {
                s <- cost.qaly(subset(results,strategy==i),inputs) %>% mutate(strategy=i)
                s$tcost <- cc
                if(is.null(out)) { out <- s } else  {out <- rbind(out,s)}
        }
}
out <- out %>%  mutate(name = paste0(name,"_b",num_seed))


save(out,file=paste0("/gpfs23/data/h_imph/gravesj/right/cost_ignite10_SA_test",num_seed,"_p.rda"))

