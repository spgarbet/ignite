pkg <- c("simmer",
         "data.table",
         "plyr",
         "dplyr",
         "tidyr",
         "reshape2",
         "parallel",
         "msm",
         "quantmod")
invisible(sapply(pkg, require, character.only=TRUE))

source("model.R")
source("costs.R")

progress   <- function(...) cat(date(), ' ', paste0(...), '\n')

env  <- simmer("IGNITE-v1.2") # Global simmer envirnoment

exec.simulation <- function(inputs)
{
  env  <<- NULL
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
  inputs$clopidogrel$vProbabilityDAPTSwitch <- 1
  
  if(strategy==0) 
  {
    inputs$vReactive                          <- "None"
    inputs$vSwitch                            <- "None"
    inputs$clopidogrel$vDAPT.Start            <- "Clopidogrel"
  } else if(strategy==1)
  {
    inputs$vReactive                          <- "None"
    inputs$vSwitch                            <- "None"
    inputs$clopidogrel$vDAPT.Start            <- "Ticagrelor"
  } else if(strategy==2)
  {
    inputs$vReactive                          <- "None"
    inputs$vSwitch                            <- "All"
    inputs$clopidogrel$vDAPT.Start            <- "Ticagrelor"
  } else if(strategy==3)
  {
    inputs$vReactive                          <- "Single"
    inputs$vSwitch                            <- "None"
    inputs$clopidogrel$vDAPT.Start            <- "Clopidogrel"
  } else
  {    
    inputs$vReactive                          <- "None"
    inputs$vSwitch                            <- "Genotype"
    inputs$clopidogrel$vDAPT.Start            <- "Ticagrelor"
  }
  
  inputs
}


run.model <- function(inputs, strategy, seed)
{
  inputs    <- set.strategy(inputs, strategy)
  chunksize <- if(inputs$chunksize > inputs$vN) inputs$vN else inputs$chunksize
  
  # Run in chucks as needed
  runs      <- ceiling(inputs$vN / chunksize)
  inputs$vN <- chunksize
  result <- sapply(1:runs, function(n) {
    progress("Strategy ", strategy, ", chunk ", n)
    set.seed(seed+n*100000)
    cost.qaly(exec.simulation(inputs), inputs)
  })
  rowSums(result)/runs
}

ignite <- function(inputs, seed, strategies=c(0, 2, 3))
{
  #result <- unlist(lapply(strategies, function(x) run.model(inputs, x, seed)))
  result <- unlist(mclapply(strategies, function(x) run.model(inputs, x, seed), mc.cores=4))
  names(result) <- paste0(c("dQALY", "dCOST"), rep(strategies, each=2))
  result 
}

