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
  result <- mclapply(1:runs, mc.cores=3, function(n) {
    progress("Strategy ", strategy, ", chunk ", n)
    set.seed(seed+n*100000)
    cost.qaly(exec.simulation(inputs), inputs)
  })
  result <- matrix(unlist(result), ncol=2, byrow=TRUE)
  colSums(result)/runs
}

sample.point <- function(inputs, row)
{
  sample <- as.numeric(read.csv("sample.txt", header=FALSE)[row,])

  inputs$clopidogrel$vRiskST30.Non                    <- sample[ 1]
  inputs$clopidogrel$vRiskST365.Non                   <- sample[ 2]
  inputs$clopidogrel$vRiskST30.Alt.Non                <- sample[ 3]
  inputs$clopidogrel$vRiskST365.Alt.Non               <- sample[ 4]
  inputs$clopidogrel$vRiskST30.LOF                    <- sample[ 5]
  inputs$clopidogrel$vRiskST365.LOF                   <- sample[ 6]
  inputs$clopidogrel$vRiskST30.Alt.LOF                <- sample[ 7]
  inputs$clopidogrel$vRiskST365.Alt.LOF               <- sample[ 8]
  inputs$clopidogrel$vSt.Case.Fatality.Non            <- sample[ 9]
  inputs$clopidogrel$vSt.Case.Fatality.Alt.Non        <- sample[10]
  inputs$clopidogrel$vSt.Case.Fatality.LOF            <- sample[11]
  inputs$clopidogrel$vSt.Case.Fatality.Alt.LOF        <- sample[12]
  inputs$clopidogrel$vPrCABG.ST                       <- sample[13]
  inputs$clopidogrel$vRiskMI30.Non                    <- sample[14]
  inputs$clopidogrel$vRiskMI365.Non                   <- sample[15]
  inputs$clopidogrel$vRiskMI30.Alt.Non                <- sample[16]
  inputs$clopidogrel$vRiskMI365.Alt.Non               <- sample[17]
  inputs$clopidogrel$vRiskMI30.LOF                    <- sample[18]
  inputs$clopidogrel$vRiskMI365.LOF                   <- sample[19]
  inputs$clopidogrel$vRiskMI365.Alt.LOF               <- sample[20]
  inputs$clopidogrel$vRiskMI365.Alt.LOF               <- sample[21]
  inputs$clopidogrel$vPrCABG.MI                       <- sample[22]
  inputs$clopidogrel$vPrPCI.MI                        <- sample[23]
  inputs$clopidogrel$vRiskStroke30.Non                <- sample[24]
  inputs$clopidogrel$vRiskStroke362.Non               <- sample[25]
  inputs$clopidogrel$vRiskStroke30.Alt.Non            <- sample[26]
  inputs$clopidogrel$vRiskStroke365.Alt.Non           <- sample[27]
  inputs$clopidogrel$vRiskStroke30.LOF                <- sample[28]
  inputs$clopidogrel$vRiskStrok365.LOF                <- sample[29]
  inputs$clopidogrel$vRiskStroke30.Alt.LOF            <- sample[30]
  inputs$clopidogrel$vRiskStroke365.Alt.LOF           <- sample[31]
  inputs$clopidogrel$vRiskDeath                       <- sample[32]
  inputs$clopidogrel$vRiskRV365                       <- sample[33]
  inputs$clopidogrel$vPrCABG.RV                       <- sample[34]
  inputs$clopidogrel$vRiskExtBleed                    <- sample[35]
  inputs$clopidogrel$vRiskIntBleed                    <- sample[36]
  inputs$clopidogrel$vRiskTIMIMinor                   <- sample[37]
  inputs$clopidogrel$vRiskFatalBleed                  <- sample[38]
  inputs$clopidogrel$vRR.ExtBleed.Ticagrelor          <- sample[39]
  inputs$clopidogrel$vRR.TintBleed.Ticagrelor         <- sample[40]
  inputs$clopidogrel$vRR.TIMIMinor.Ticagrelor         <- sample[41]
  inputs$clopidogrel$vRR.FatalBleed.Ticagrelor        <- sample[42]
  inputs$clopidogrel$vRiskCABGTIMImajor               <- sample[43]
  inputs$clopidogrel$vRR.RiskCABGTIMImajor.Ticagrelor <- sample[44]
  inputs$clopidogrel$vRR.Bleed.LOF                    <- sample[45]
  inputs$costs$single_test                            <- sample[46]
  inputs$costs$clopidogrel                            <- sample[47] / 30
  inputs$costs$ticagrelor                             <- sample[48] / 30
  
  inputs
}

ignite <- function(inputs, seed, strategies=c(0, 2, 3))
{
  inputs <- sample.point(inputs, seed)
  
  result <- unlist(lapply(strategies, function(x) run.model(inputs, x, seed)))
  #result <- unlist(mclapply(strategies, function(x) run.model(inputs, x, seed), mc.cores=4))
  names(result) <- paste0(c("dQALY", "dCOST"), rep(strategies, each=2))
  result 
}

