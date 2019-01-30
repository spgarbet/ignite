####
## 
# Define Simulation Inputs
##
####

# Function to Convert Nominal to Real Dollars
# Get CPI-U from FRED
#library(quantmod)
#getSymbols("CPIAUCSL", src='FRED')
#avg.cpi <- apply.yearly(CPIAUCSL, mean) 
#save(avg.cpi, file="main/cpi.Rdata")
#library(quantmod)
#load("CPIAUCSL.Rdata")
#avg.cpi <- apply.yearly(CPIAUCSL, mean) 
#save(avg.cpi, file="main/cpi.Rdata")
load("main/cpi.Rdata")

realdol = function(nominal, year, base=2017)
{
  cf <- avg.cpi/as.numeric(avg.cpi[paste(base)])
  return(as.numeric(nominal*cf[paste(year)]))
}

epsilon <- 1e-12

clopidogrel = list(
  vProbabilityRead          = 1.00, # probability of physician using test results
  vProbabilityReactive      = 1.00, # Under reactive, probability of ordering test
  
  vDAPT.Start               = "Clopidogrel",
  vDAPT.SecondLine          = "Ticagrelor",
  
  # Population-Level Allele Frequency Distribution
  vCYP2C19.Poor             = 0.320, #IGNITE input - Jan
  vCYP2C19.Rapid            = 0.260, #IGNITE input - Jan
  vCYP2C19.Unknown          = 0.038, #IGNITE input - Jan
  
  # This parameter governs whether repeat DAPT therapy is more or less likely after having one.
  vRRRepeat.DAPT            = epsilon,
  
  # This paramter governs the maximum number of DAPT therapies an individual can have.  The relative risk of DAPT is 
  # set to epsilon (i.e., never re-occurs) once the patient has hit this maximum.
  vMaxDAPT                  = 4,
   
  vDAPT.Tx.Duration         = 365, # (12mo-48mo)
  
  vProbabilityDAPTSwitch    = 0.62, #IGNITE input - Jan
  
  # Stent Thrombosis: Event Rates and Relative Risks
  # The Stent Thrombosis Risks are drawn from a piecewise exponential with the following
  # durations and rates. 
  #Clopidogrel,Non-LOF
  vRiskST30.Non             = 0.0074, #IGNITE input
  vRiskST365.Non            = 0.0135, #IGNITE input

  #Alt, Non-LOF
  vRiskST30.Alt.Non         = 0.0067, #IGNITE input
  vRiskST365.Alt.Non        = epsilon, #IGNITE input

  #Clopidogrel, LOF
  vRiskST30.LOF             = 0.021, #IGNITE input 0.0150
  vRiskST365.LOF            = epsilon, #IGNITE input 0.0060

  #Alt, LOF
  vRiskST30.Alt.LOF         = epsilon, #IGNITE input  
  vRiskST365.Alt.LOF        = 0.0244, #IGNITE input  

  #Aspirin
  vRiskST30.Aspirin         = 0.0150, 
  vRiskST365.Aspirin        = 0.0060, 
  vRR.ST.Aspirin            = 1.29,

  vSt.Case.Fatality.Non     = 0.15, #IGNITE input inferred
  vSt.Case.Fatality.Alt.Non = epsilon, #IGNITE input inferred  
  vSt.Case.Fatality.LOF     = 0.18, #IGNITE input inferred  
  vSt.Case.Fatality.Alt.LOF = epsilon, #IGNITE input inferred  
  
  vPrCABG.ST                = 0.08,
  
  # Relative Risk of ST for patients with loss of function allele who are treated with 
  # Clopidogrel.

  # Myocardial Infarction: Event Rates and Relative Risks
  #subtract ST from MI in IGNITE spreadsheet
  #Clopidogrel,Non-LOF
  vRiskMI30.Non             = 0.0225, #IGNITE input  
  vRiskMI365.Non            = 0.0379, #IGNITE input  

  # Alt,Non-LOF
  vRiskMI30.Alt.Non         = 0.0133, #IGNITE input  
  vRiskMI365.Alt.Non        = 0.0484, #IGNITE input  

  # Clopidogrel, LOF
  vRiskMI30.LOF             = 0.0427, #IGNITE input  
  vRiskMI365.LOF            = 0.1262, #IGNITE input  

  # Alt, LOF
  vRiskMI30.Alt.LOF         = 0.0085, #IGNITE input  
  vRiskMI365.Alt.LOF        = 0.0327, #IGNITE input  

  #Aspirin
  vRiskMI.Aspirin           = 0.035, #(0.013-0.097)
  vRR.MI.Aspirin            = 1.29, # (1.12-1.48)
  vPrCABG.MI                = 0.08, # (4-12)
  vPrPCI.MI                 = 0.55, # (45-65)

  # Stroke - IGNITE added event
  #Clopidogrel,Non-LOF
  vRiskStroke30.Non         = 0.009, #IGNITE input  
  vRiskStroke365.Non        = 0.0068, #IGNITE input  

  #Alt, Non-LOF
  vRiskStroke30.Alt.Non     = epsilon, #IGNITE input  
  vRiskStroke365.Alt.Non    = epsilon, #IGNITE input  

  #Clopidogrel, LOF
  vRiskStroke30.LOF         = 0.0067, #IGNITE input  
  vRiskStroke365.LOF        = epsilon, #IGNITE input  

  #Alt, LOF
  vRiskStroke30.Alt.LOF     = epsilon, #IGNITE input  
  vRiskStroke365.Alt.LOF    = epsilon, #IGNITE input  

 # Non-cardio death - IGNITE defined event
  vRiskDeath=0.0175,
  
  # Revascularization
  vRiskRV365                = 0.10,   # (0.05-0.15)
  vRiskRVgt365              = 0.03,   # (0.02-0.04)
  vPrCABG.RV                = 0.25,   # (15-35)
  
  # Bleeding
  vRiskExtBleed             = 0.0230, # (0.015-0.070)
  vRiskIntBleed             = 0.0015, # (0.001-0.002)
  vRiskTIMIMinor            = 0.0200, # (0.010-0.060)
  vRiskFatalBleed           = 0.0015, # (0.001-0.003)
  vRR.ExtBleed.Ticagrelor   = 1.30,   # (1.05-1.61)
  vRR.ExtBleed.Prasugrel    = 1.22,   # (0.93-1.6)
  vRR.ExtBleed.Aspirin      = 0.72,   # (0.60-1.00)
  
  vRR.IntBleed.Ticagrelor   = 1.15,   # (.55-2.41)
  vRR.IntBleed.Prasugrel    = 0.83,   # (0.36-1.92)
  vRR.IntBleed.Aspirin      = 0.71,   # (0.23-2.23)
  
  vRR.TIMIMinor.Ticagrelor  = 1.07,   # ( .91 - 1.26)
  vRR.TIMIMinor.Prasugrel   = 1.16,   # (.91-1.49)
  vRR.TIMIMinor.Aspirin     = 0.47,   # (.39-.57)
  
  vRR.FatalBleed.Ticagrelor = 0.87,   # (0.48-1.59)
  vRR.FatalBleed.Prasugrel  = 4.19,   # (1.58-11.11)
  vRR.FatalBleed.Aspirin    = 1.35,   # (0.62-0.95)
  
  vRiskCABGTIMImajor        = 0.022,  # (0.013-0.031) 
  vRR.RiskCABGTIMImajor.Ticagrelor = 1.08, # (0.85-1.36)
  vRR.RiskCABGTIMImajor.Prasugrel  = 4.73, # (1.90-11.82)
  vRR.RiskCABGTIMImajor.Aspirin    = 1.08, # (0.61-1.91)
  
  vRR.Bleed.LOF                    = 0.84 # (0.75-1.00)
)



inputs <- list(
  # Population Parameters
  vN           = 1000,   # Patients to simulate
  vHorizon     = 1,      # Length of simulation upon a patient entering (1 year)

  # Strategies
  vPreemptive  = "None", # Can be one of following: "None", "Panel", or "Age >= 50"
  vReactive    = "None", # Can be one of following: "None", "Single", "Panel"
  vSwitch      = "None", # Can be one of following: "None", "All","Genotype"; trigger 30-day drug switch event

  # Drug specific model parameters
  clopidogrel = clopidogrel,
  
  # If these names match the event names from the simmer model, then computation can be generalized!
  # These must be DAILY costs
  costs = list(
    panel_test             = realdol(  250,    2012),
    single_test            = realdol(  100,    2012),
    
    aspirin                = realdol(    2/30, 2017),
    clopidogrel            = realdol(   10/30, 2017),
    ticagrelor             = realdol(  360/30, 2017),
    prasugrel              = realdol(  261/30, 2011),
    bleed_ext_maj_nonfatal = realdol(10120/14, 2011),
    bleed_int_maj_nonfatal = realdol(20740,    2011),
    bleed_min_nonfatal     = realdol(   79/ 2, 2011),
    bleed_fatal            = realdol(17920,    2011),
    cardio_death           = realdol(24540,    2011),
    st_cabg                = realdol(67720,    2011),
    st_pci                 = realdol(27840,    2011), 
    mi_cabg                = realdol(67720,    2011),
    mi_med_manage          = realdol(17200,    2011),
    mi_pci                 = realdol(27840,    2011), 
    revasc_cabg            = realdol(50560/14, 2011),
    revasc_pci             = realdol(20670/ 7, 2011),
    cabg_bleed             = realdol(35570/ 7, 2011),
    dapt_stroke            = realdol(21537,    2007)
  ),
  # Each listed duration will be corrected in the final data frame (temp disutility)
  durations = list(
    bleed_ext_maj_nonfatal = 14,
    bleed_min_nonfatal     =  2,
    revasc_cabg            = 14,
    revasc_pci             =  7,
    cabg_bleed             =  7,
    MajorBleed_GI	         = 14,
    MajorBleed_Other       = 14,
    MinorBleed             = 2
  ),
  disutilities = list(
    bleed_ext_maj_nonfatal = 0.2,
    bleed_int_maj_nonfatal = 0.61,
    bleed_min_nonfatal     = 0.2,
    bleed_fatal            = 1,
    cardio_death           = 1,
    st_cabg                = 0.12,
    st_pci                 = 0.12,    
    mi_cabg                = 0.12,
    mi_med_manage          = 0.12,
    mi_pci                 = 0.12,
    revasc_cabg            = 0.5,
    revasc_pci             = 0.5,
    cabg_bleed             = 0.5,
    dapt_stroke            = 0.64,
    secular_death          = 1
  ),
  # Each shows whether the event permanently decreases utility (type==0) vs temporarily(type==1)
  type= list(
    bleed_ext_maj_nonfatal = 1,
    bleed_int_maj_nonfatal = 0,
    bleed_min_nonfatal     = 1,
    bleed_fatal            = 0,
    cardio_death           = 0,
    st_cabg                = 0,
    st_pci                 = 0,    
    mi_cabg                = 0,
    mi_med_manage          = 0,
    mi_pci                 = 0,
    revasc_cabg            = 1,
    revasc_pci             = 1,
    cabg_bleed             = 1,
    dapt_stroke            = 0,
    secular_death          = 0
  )   
)
