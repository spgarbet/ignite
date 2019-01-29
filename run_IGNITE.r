##################################################################################################################
# RIGHT - Simulation Model
# Note: Attributes Start With Prefix "a"
# To Do:
#     Annals paper has a cardiovascular mortality outcome for 1 year post PCI.  Should we add in?  Or just 
#       rely on the MACE outcome constructed elsewhere?
#     New Outcome: Periprocedural death due to PCI ? 0.12% (0.10-1.00%)
#     New Outcome: Periprocedural death due to CABG? 2.10% (1.0-10.0%)
##################################################################################################################

####
## 
# Define Simulation Scenario
##
####
source("./inputs_IGNITE.R")

####
## Secular Death
source('./main/event_secular_death.R')

# Define Panel Test attributes, functions
all_genotyped <- function()
{
  get_attribute(env, 'aGenotyped_CYP2C19')  == 1
}

any_genotyped <- function()
{
  get_attribute(env, 'aGenotyped_CYP2C19') == 1 
}

panel_test <- function(traj, inputs)
{
  traj %>% 
    set_attribute('aGenotyped_CYP2C19', 1)
    mark("panel_test")
}

#####
## Clopidogrel
source("./clopidogrel/counters.R")
source("./clopidogrel/initial-patient-attributes.R")
source("./clopidogrel/cleanup.R")
source("./clopidogrel/PGx-attributes.r")
source("./clopidogrel/dapt-events.r")

#load('./main/NHANES_pop_11_14.rda')
study_pop <- read.csv("./main/Age_Sex.csv")
initialize_patient <- function(traj, inputs)
{
  traj %>%
    seize("time_in_model")       %>%
    
    # Empirical
    set_attribute("aNo",     function() sample(1:nrow(study_pop), 1, prob=rep(1/nrow(study_pop),nrow(study_pop)))) %>%
    set_attribute("aGender", function() study_pop$gender[get_attribute(env, "aNo")]) %>% 
    set_attribute("aAge",    function() study_pop$age.mh[get_attribute(env, "aNo")]) %>% 
    
    set_attribute("aAgeInitial",function() get_attribute(env, 'aAge'))  %>%
    assign_clopidogrel_attributes(inputs)
}

# Must Be Run After The Initial Event Times Have Been Determined 

# No modification required for adding more drug models
preemptive_strategy <- function(traj, inputs)
{
  # Note this doesn't have to use branch, because it's a global that every trajectory gets
  if        (inputs$vPreemptive == "None"     )
  {
    traj # Do nothing
  } else if (inputs$vPreemptive == "Panel"    )
  {
    traj %>% panel_test(inputs)
  } else if (inputs$vPreemptive == "Age >= 50")
  {
    traj %>%
    branch(
      function() if(get_attribute(env,'aAge') >= 50) 1 else 2,
      continue = c(TRUE, TRUE),
      trajectory() %>% panel_test(inputs), 
      trajectory() %>% timeout(0)  # Do nothing
    )
  } else stop("Unhandled Preemptive Strategy")
}

####
## Cleanup 
cleanup_on_termination <- function(traj)
{
  traj %>% 
    release("time_in_model") %>%
    cleanup_clopidogrel() %>%
    cleanup_aspirin()
}

terminate_simulation <- function(traj, inputs)
{
  traj %>%
  branch(
    function() 1, 
    continue=FALSE,
    trajectory() %>% cleanup_on_termination()
  )
}

####
## Event Registry
event_registry <- list(
  
  #### Global Events
  list(name          = "Secular Death",
       attr          = "aSecularDeathTime",
       time_to_event = days_till_death,
       func          = secular_death,
       reactive      = FALSE),
  list(name          = "Terminate at 10 years",
       attr          = "aTerminate",
       time_to_event = function(inputs) 365.0*inputs$vHorizon,
       func          = terminate_simulation,
       reactive      = FALSE),
  

  #### Clopidogrel Events
  list(name          = "DAPT Initialized",
       attr          = "aTimeDAPTInitialized",
       time_to_event = days_till_dapt,
       func          = dapt,
       reactive      = FALSE) ,
  list(name          = "DAPT Ended",
       attr          = "aDAPTEnded",
       time_to_event = dapt_end_time,
       func          = dapt_end,
       reactive      = FALSE),
  list(name          = "Stent Thromb",
       attr          = "aST",
       time_to_event = time_to_ST,
       func          = ST_event,
       reactive      = FALSE),
  list(name          = "Myocardial Infarction",
       attr          = "aMI",
       time_to_event = time_to_MI,
       func          = MI_event,
       reactive      = FALSE) , 
  list(name          = "Revascularization",
       attr          = "aRV",
       time_to_event = time_to_RV,
       func          = RV_event,
       reactive      = FALSE) ,
  list(name          = "Extracranial TIMI Non-Fatal",
       attr          = "aExtBleed",
       time_to_event = time_to_ExtBleed,
       func          = ExtBleed_event,
       reactive      = FALSE) ,
  list(name          = "Intracranial TIMI Major Nonfatal",
       attr          = "aIntBleed",
       time_to_event = time_to_IntBleed,
       func          = IntBleed_event,
       reactive      = FALSE),
  list(name          = "TIMI Minor",
       attr          = "aTIMIMinor",
       time_to_event = time_to_TIMIMinor,
       func          = TIMIMinor_event,
       reactive      = FALSE) ,
  list(name          = "Fatal Bleed",
       attr          = "aFatalBleed",
       time_to_event = time_to_FatalBleed,
       func          = FatalBleed_event,
       reactive      = FALSE),
  list(name          = "CABG-related bleed",
       attr          = "aCABGBleed",
       time_to_event = time_to_CABGBleed,
       func          = CABGBleed_event,
       reactive      = FALSE),
  list(name          = "DAPT 30d",
       attr          = "aDAPT30d",
       time_to_event = dapt_30d,
       func          = dapt_30d_strategy,
       reactive      = FALSE),
  list(name          = "DAPT Stroke",
       attr          = "aDAPTStroke",
       time_to_event = days_to_stroke,
       func          = dapt_stroke_event,
       reactive      = FALSE)

)

#####
## Counters
source("./main/counters.R")
counters <- c(counters.gen, counters.dapt)


#####################################################################
####
##
# Setup and Run the Simulation.
##
####
source('./main/event_main_loop.R')


