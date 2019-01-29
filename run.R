pkg <- c("simmer",
         "data.table",
         "plyr",
         "dplyr",
         "tidyr",
         "reshape2",
         "ggplot2",
         "msm",
         "quantmod")
invisible(sapply(pkg, require, character.only=TRUE))

source("model.R")
source("costs.R")

env  <- simmer("IGNITE-v1.2")

exec.simulation <- function(inputs)
{
  env  <<- simmer("IGNITE-v1.2")
  traj <- simulation(env, inputs)
  env %>% create_counters(counters)
  
  env %>%
    add_generator("patient", traj, at(rep(0, inputs$vN)), mon=2) %>%
    run(365*inputs$vHorizon+1) %>% # Simulate just past horizon
    wrap()

  get_mon_arrivals(env, per_resource = T)
}

set.strategy <- function(inputs, strategy)
{
  inputs$vPreemptive <- "None"
  inputs$vDrugs      <- list(vClopidogrel = T)
  inputs$clopidogrel$vRRRepeat.DAPT <- 0
  
  if(strategy==0) 
  {
    inputs$vReactive                          <- "None"
    inputs$vSwitch                            <- "None"
    inputs$clopidogrel$vDAPT.Start            <- "Clopidogrel"
    inputs$clopidogrel$vProbabilityDAPTSwitch <- 0
  } else if(strategy==1)
  {
    inputs$vPreemptive                        <- "None"
    inputs$vReactive                          <- "None"
    inputs$vSwitch                            <- "None"
    inputs$clopidogrel$vDAPT.Start            <- "Ticagrelor"
    inputs$clopidogrel$vProbabilityDAPTSwitch <- 0
  } else if(strategy==2)
  {
    inputs$vPreemptive                        <- "None"
    inputs$vReactive                          <- "None"
    inputs$vSwitch                            <- "All"
    inputs$clopidogrel$vDAPT.Start            <- "Ticagrelor"
    inputs$clopidogrel$vProbabilityDAPTSwitch <- 0
  } else if(Istrategy==3)
  {
    inputs$vPreemptive                        <- "None"
    inputs$vReactive                          <- "Single"
    inputs$vSwitch                            <- "None"
    inputs$clopidogrel$vDAPT.Start            <- "Clopidogrel"
    inputs$clopidogrel$vProbabilityDAPTSwitch <- 1
  } else
  {    
    inputs$vPreemptive                        <- "None"
    inputs$vReactive                          <- "None"
    inputs$vSwitch                            <- "Genotype"
    inputs$clopidogrel$vDAPT.Start            <- "Ticagrelor"
    inputs$clopidogrel$vProbabilityDAPTSwitch <- 1
  }
  
  inputs
}



inputs$vN <- 20 #0000
###events summary

run.model <- function(inputs, strategy, seed)
{
  inputs <- set.strategy(inputs, strategy)
  set.seed(seed)
  cost.qaly(data.table(exec.simulation(inputs)), inputs)
}

ignite <- function(inputs, seed, strategies=0:4)
{
  result <- unlist(lapply(strategies, function(x) run.model(inputs, x, seed)))
  names(result) <- paste0(c("dQALY", "dCOST"), rep(strategies, each=2))
  result 
}

