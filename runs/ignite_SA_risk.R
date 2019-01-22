###full run
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

#seed_01
sdt <- c(12345,
         54321,
         1501,
         268,
         34,
         3905,
         476,
         325,
         5,
         10)

args <- commandArgs(trailing = TRUE)
num_seed <- as.integer(args[1])

env  <- simmer("RIGHT-v1.1")

exec.simulation <- function(inputs)
{
        set.seed(sdt[num_seed+1])
        env  <<- simmer("RIGHT-v1.1")
        traj <- simulation(env, inputs)
        env %>% create_counters(counters)
        
        env %>%
                add_generator("patient", traj, at(rep(0, inputs$vN)), mon=2) %>%
                run(365*inputs$vHorizon+1) %>% # Simulate just past horizon
                wrap()
        
        get_mon_arrivals(env, per_resource = T)
}



source("./run_IGNITE.r")
inputs$vN <- 200000
inputs$vHorizon <- 1

###Single Drug 
inputs$vDrugs = list(vSimvastatin = F, 
                     vWarfarin = F,
                     vClopidogrel = T)
#inputs$warfarin$vscale_timetowarfarin <- epsilon
inputs$clopidogrel$vDAPTScale <- epsilon
#inputs$simvastatin$vScale <- epsilon
inputs$clopidogrel$vRRRepeat.DAPT <- 0 #only for low-weibull runs, to fix retrigger clopidogrel prescription


results <- NULL
for(fct in seq(0.75,2,by=0.25)) {
        inputs$clopidogrel$vRR.ExtBleed.Ticagrelor <- 1.3*fct
        inputs$clopidogrel$vRR.IntBleed.Ticagrelor <- 1.15*fct
        
        {
                for(Istrategy in 0:4) {
                        if(Istrategy==0) 
                        {
                                inputs$vPreemptive = "None"
                                inputs$vReactive = "None"
                                inputs$vSwitch = "None"
                                inputs$clopidogrel$vDAPT.Start = "Clopidogrel"
                        } else if(Istrategy==1) {
                                inputs$vPreemptive = "None"
                                inputs$vReactive = "None"
                                inputs$vSwitch = "None"
                                inputs$clopidogrel$vDAPT.Start = "Ticagrelor"
                        } else if(Istrategy==2){
                                inputs$vPreemptive = "None"
                                inputs$vReactive = "None"
                                inputs$vSwitch = "All"
                                inputs$clopidogrel$vDAPT.Start = "Ticagrelor"
                        } else if(Istrategy==3){
                                inputs$vPreemptive = "None"
                                inputs$vReactive = "Single"
                                inputs$vSwitch = "None"
                                inputs$clopidogrel$vDAPT.Start = "Clopidogrel"
                                inputs$clopidogrel$vProbabilityDAPTSwitch <- 1
                        } else {    
                                inputs$vPreemptive = "None"
                                inputs$vReactive = "None"
                                inputs$vSwitch = "Genotype"
                                inputs$clopidogrel$vDAPT.Start = "Ticagrelor"
                                inputs$clopidogrel$vProbabilityDAPTSwitch <- 1
                        }
                        
                        cat("Running ", Istrategy, fct, "\n")
                        run <- exec.simulation(inputs)
                        run$strategy <- Istrategy
                        run$fct <- fct
                        
                        if(is.null(results)) { results <- run } else  {results <- rbind(results, run)}
                        rm(run)
                }
        }
}

###events summary
DT <- data.table(results)
DT[, .N, by = list(resource, strategy, fct)]
summ <- DT[, .N, by = list(resource, strategy, fct)]
save(summ,file=paste0("/gpfs23/data/h_imph/gravesj/right/ignite/sum_ignite_SA_",num_seed,"_r.rda"))
save(results,file=paste0("/gpfs23/data/h_imph/gravesj/right/ignite/raw_ignite_SA_",num_seed,"_r.rda"))

