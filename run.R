pkg <- c("simmer",
         "data.table",
         "plyr",
         "dplyr",
         "tidyr",
         "reshape2",
         "ggplot2",
         "msm")
invisible(sapply(pkg, require, character.only=TRUE))

source("model.R")
source("costs.R")

progress   <- function(...) cat(date(), ' ', paste0(...), '\n')

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
  } else if(strategy==3)
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

chunksize <- 100 #000

run.model <- function(inputs, strategy, seed)
{
  inputs    <- set.strategy(inputs, strategy)
  chunksize <- if(chunksize > inputs$vN) inputs$vN else chunksize
  
  # Run in chucks as needed
  runs      <- ceiling(inputs$vN / chunksize)
  inputs$vN <- chunksize
  result <- sapply(1:runs, function(n) {
    progress("Strategy ", strategy, ", chunk ", n)
    set.seed(seed+runs*20000)
    cost.qaly(data.table(exec.simulation(inputs)), inputs)
  })
  rowSums(result)/runs
}

ignite <- function(inputs, seed, strategies=c(0, 2, 3))
{
  result <- unlist(lapply(strategies, function(x) run.model(inputs, x, seed)))
  names(result) <- paste0(c("dQALY", "dCOST"), rep(strategies, each=2))
  result 
}

