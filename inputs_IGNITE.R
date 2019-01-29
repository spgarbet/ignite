####
## 
# Define Simluation Inputs
##
####

# Function to Convert Nominal to Real Dollars
# Get CPI-U from FRED
 library(quantmod)
 getSymbols("CPIAUCSL", src='FRED')
 avg.cpi <- apply.yearly(CPIAUCSL, mean) 
 #save(avg.cpi,file="./main/cpi.Rdata")

realdol = function(nominal,year,base=2017)
{
  cf <- avg.cpi/as.numeric(avg.cpi[paste(base)])
  return(as.numeric(nominal*cf[paste(year)]))
}

epsilon <- 0.000000000001

clopidogrel = list(
  vPREDICTsens = 0.23,
  vPREDICTspec = 0.93,
  vProbabilityRead = 1.00, # probability of physician using test results
  vProbabilityReactive = 1.00, # Under reactive, probability of ordering test
  
  vDAPT.Start = "Clopidogrel",
  vDAPT.SecondLine = "Ticagrelor",
  
  # Population-Level Allele Frequency Distribution
  vCYP2C19.Poor    = 0.32, #IGNITE input - Jan
  vCYP2C19.Rapid   = 0.26, #IGNITE input - Jan
  vCYP2C19.Unknown = 0.038, #IGNITE input - Jan
  
  # Indication Paramters (Weibull) source: VUMC data -- files is ./reference/WCS_KM_Distribution_Generation.pdf
  vDAPTShape = 0.59,
  vDAPTScale = 60475.53,
  
  # This parameter governs whether repeat DAPT therapy is more or less likely after having one.
  vRRRepeat.DAPT = epsilon,
  
  # This paramter governs the maximum number of DAPT therapies an individual can have.  The relative risk of DAPT is 
  # set to epsilon (i.e., never re-occurs) once the patient has hit this maximum.
  vMaxDAPT = 4,
  
  vDAPT.Tx.Duration = 365, # (12mo-48mo)
  
  vProbabilityDAPTSwitch = 0.62, #IGNITE input - Jan
  
  # Stent Thrombosis: Event Rates and Relative Risks
  # The Stent Thrombosis Risks are drawn from a piecewise exponential with the following
  # durations and rates. 
  #Clopidogrel,Non-LOF
  vRiskST30.Non    = 0.0074, #IGNITE input
  vRiskST365.Non   = 0.0135, #IGNITE input
  vRR.ST.Non = c(1,1),
  
  #Alt, Non-LOF
  vRiskST30.Alt.Non    = 0.0067, #IGNITE input
  vRiskST365.Alt.Non   = epsilon, #IGNITE input
  vRR.ST.Alt.Non = c(1,1), 
  
  #Clopidogrel, LOF
  vRiskST30.LOF    = 0.021, #IGNITE input 0.0150, # (0.010-0.020)
  vRiskST365.LOF   = epsilon, #IGNITE input 0.0060, # (0.003-0.009)
  vRR.ST.LOF = c(1,1), 
  
  #Alt, LOF
  vRiskST30.Alt.LOF    = epsilon, #IGNITE input  
  vRiskST365.Alt.LOF   = 0.0244, #IGNITE input  
  vRR.ST.Alt.LOF = c(1,1), 
  
  #vRR.ST.Ticagrelor = 0.75, # (0.59-0.95) 
  #vRR.ST.Prasugrel  = 0.48, # (0.36-0.64)
  
  #Aspirin
  vRiskST30.Aspirin    = 0.0150, # (0.010-0.020)
  vRiskST365.Aspirin   = 0.0060, # (0.003-0.009)
  vRR.ST.Aspirin = c(1.29,1.29),
  #vRR.ST.Aspirin    = 1.29, # (1.12-1.48)
  
  vSt.Case.Fatality.Non = 0.15, #IGNITE input inferred
  vSt.Case.Fatality.Alt.Non = epsilon, #IGNITE input inferred  
  vSt.Case.Fatality.LOF = 0.18, #IGNITE input inferred  
  vSt.Case.Fatality.Alt.LOF = epsilon, #IGNITE input inferred  
  
  vPrCABG.ST = 0.08,  # WHAT IS SOURCE?  CAN'T FIND IN ANNALS PAPER...
  
  # Relative Risk of ST for patients with loss of function allele who are treated with 
  # Clopidogrel.
  #vRR.ST.LOF = 1.72, #IGNITE input - Jan 
  
  # Myocardial Infarction: Event Rates and Relative Risks
  #subtract ST from MI in IGNITE spreadsheet
  #Clopidogrel,Non-LOF
  vRiskMI30.Non = 0.0225, #IGNITE input  
  vRiskMI365.Non = 0.0379, #IGNITE input  
  vRR.MI.Non = c(1,1),
  
  #Alt,Non-LOF
  vRiskMI30.Alt.Non = 0.0133, #IGNITE input  
  vRiskMI365.Alt.Non = 0.0484, #IGNITE input  
  vRR.MI.Alt.Non = c(1,1),
  
 #Clopidogrel, LOF
  vRiskMI30.LOF = 0.0427, #IGNITE input  
  vRiskMI365.LOF = 0.1262, #IGNITE input  
  vRR.MI.LOF = c(1,1), #4.34 is vRR.MI.LOF
  
  #Alt, LOF
  vRiskMI30.Alt.LOF = 0.0085, #IGNITE input  
  vRiskMI365.Alt.LOF = 0.0327, #IGNITE input  
  vRR.MI.Alt.LOF = c(1,1), #0.84 is vRR.MI.Ticagrelor
  
  #vRiskMI = 0.035, #(0.013-0.097)
  #vRR.MI.Ticagrelor =0.84, # (0.75-0.95)
  #vRR.MI.Prasugrel = 0.76, # (0.67-0.85)
  #Aspirin
  vRiskMI.Aspirin = 0.035, #(0.013-0.097)
  vRR.MI.Aspirin = 1.29, # (1.12-1.48)
  vPrCABG.MI = 0.08, # (4-12)
  vPrPCI.MI = 0.55, # (45-65)
  #vRR.MI.LOF = 4.34, #IGNITE input-Jan
  
  # Stroke - IGNITE added event
  #Clopidogrel,Non-LOF
  vRiskStroke30.Non = 0.009, #IGNITE input  
  vRiskStroke365.Non = 0.0068, #IGNITE input  
  vRR.Stroke.Non = c(1,1),
  
  #Alt, Non-LOF
  vRiskStroke30.Alt.Non = epsilon, #IGNITE input  
  vRiskStroke365.Alt.Non = epsilon, #IGNITE input  
  vRR.Stroke.Alt.Non = c(1,1),
  
  #Clopidogrel, LOF
  vRiskStroke30.LOF = 0.0067, #IGNITE input  
  vRiskStroke365.LOF = epsilon, #IGNITE input  
  vRR.Stroke.LOF = c(1,1),
  
  #Alt, LOF
  vRiskStroke30.Alt.LOF = epsilon, #IGNITE input  
  vRiskStroke365.Alt.LOF = epsilon, #IGNITE input  
  vRR.Stroke.Alt.LOF = c(1,1),
  
 # Non-cardio death - IGNITE defined event
  vRiskDeath=0.0175,
  
  # Revascularization
  vRiskRV365 = 0.10, # (0.05-0.15)
  vRiskRVgt365 = 0.03, # (0.02-0.04)
  vPrCABG.RV = .25, # (15-35)
  
  # Bleeding
  vRiskExtBleed = 0.0230, # (0.015-0.070)
  vRiskIntBleed = 0.0015, # (0.001-0.002)
  vRiskTIMIMinor = 0.0200, # (0.010-0.060)
  vRiskFatalBleed = 0.0015, # (0.001-0.003)
  vRR.ExtBleed.Ticagrelor = 1.30, # (1.05-1.61)
  vRR.ExtBleed.Prasugrel = 1.22, #(0.93-1.6)
  vRR.ExtBleed.Aspirin =  0.72, #(0.60-1.00)
  
  vRR.IntBleed.Ticagrelor = 1.15, # (.55-2.41)
  vRR.IntBleed.Prasugrel =  0.83, # (0.36-1.92)
  vRR.IntBleed.Aspirin =  0.71, # (0.23-2.23)
  
  vRR.TIMIMinor.Ticagrelor = 1.07, # ( .91 - 1.26)
  vRR.TIMIMinor.Prasugrel =  1.16, # (.91-1.49)
  vRR.TIMIMinor.Aspirin =  0.47, #(.39-.57)
  
  vRR.FatalBleed.Ticagrelor = 0.87, # (0.48-1.59)
  vRR.FatalBleed.Prasugrel =  4.19, # (1.58-11.11)
  vRR.FatalBleed.Aspirin =  1.35, #(0.62-0.95)
  
  vRiskCABGTIMImajor = 0.022, # (0.013-0.031) 
  vRR.RiskCABGTIMImajor.Ticagrelor = 1.08, # (0.85-1.36)
  vRR.RiskCABGTIMImajor.Prasugrel =  4.73,# (1.90-11.82)
  vRR.RiskCABGTIMImajor.Aspirin =  1.08, # (0.61-1.91)
  
  vRR.Mort.LOF = 1.65, #IGNITE input  1.28, #(0.95-1.73) Unused parameter
  vRR.Bleed.LOF = 0.84 # (0.75-1.00)
  #vRR.Thrombotic.GOF = 0.75, # (0.66-1.00)
  #vRR.Bleed.GOF = 1.26  # (1.00-1.50)
)



inputs <- list(
  # Population Parameters
  vN           = 1000,   # Patients to simulate
  vNIter       = 4,      # Number of Iterations (parallel processing)
  #vLowerAge    = 40,      # Lower age to simulate coming in (uniform distribution)
  #vUpperAge    = 85,      # Upper age to simulate
  vHorizon     = 1,      # Length of simulation upon a patient entering
  #vPctFemale   = 0.5,     # Percent Female
  
  # Strategies
  vPreemptive  = "None",  # Can be one of following: "None", "Panel", "PREDICT", or "Age >= 50"
  vReactive    = "None", # Can be one of following: "None", "Single", "Panel"
  vSwitch = "None", # Can be one of following: "None", "All","Genotype"; trigger 30-day drug switch event

# Control Which Drugs Are Run in the Model 
  vDrugs       = list(vSimvastatin = FALSE),

# CURRENTLY PANEL IS FOR ALL DRUGS ???
  vPanel       = list(vSimvastatin = TRUE),

  # Drug specific model parameters
  clopidogrel = clopidogrel,
  
  # If these names match the event names from the simmer model, then computation can be generalized!
  # These must be DAILY costs
costs = list(
  panel_test      =   realdol(250,year=2012),
  single_test     =   realdol(100,year=2012),
  
  aspirin         = realdol(2/30,year=2017),
  clopidogrel     = realdol(10/30,year=2017),
  ticagrelor      = realdol(360/30,year=2017),
  prasugrel       = realdol(261/30,year=2011),
  bleed_ext_maj_nonfatal = realdol(10120/14,2011),
  bleed_int_maj_nonfatal = realdol(20740,2011),
  bleed_min_nonfatal = realdol(79/2,2011),
  bleed_fatal     = realdol(17920,2011),
  cardio_death        = realdol(24540,2011),
  st_cabg         = realdol(67720,2011),
  st_pci     = realdol(27840,2011), 
  mi_cabg           = realdol(67720,2011),
  mi_med_manage   = realdol(17200,2011),
  mi_pci     = realdol(27840,2011), 
  revasc_cabg     = realdol(50560/14,2011),
  revasc_pci      = realdol(20670/7,2011),
  cabg_bleed      = realdol(35570/7,2011),
  dapt_stroke  = realdol(21537,year=2007)
  
),
# Each listed duration will be corrected in the final data frame (temp disutility)
durations = list(
  
  bleed_ext_maj_nonfatal = 14,
  bleed_min_nonfatal = 2,
  revasc_cabg     = 14,
  revasc_pci      = 7,
  cabg_bleed      = 7,
  
  MajorBleed_GI	= 14,
  MajorBleed_Other =	14,
  MinorBleed = 2
  
),
disutilities = list(
  
  
  bleed_ext_maj_nonfatal = .2,
  bleed_int_maj_nonfatal = .61,
  bleed_min_nonfatal = .2,
  bleed_fatal     = 1,
  cardio_death        = 1,
  st_cabg         = .12,
  st_pci     = .12,    
  mi_cabg         = .12,
  mi_med_manage   = .12,
  mi_pci     = .12,
  revasc_cabg     = .5,
  revasc_pci      = .5,
  cabg_bleed      = .5,
  dapt_stroke  = 0.64,
  
  secular_death = 1
  
),
# Each shows whether the event permanently decreases utility (type==0) vs temporarily(type==1)
type= list(
  bleed_ext_maj_nonfatal = 1,
  bleed_int_maj_nonfatal = 0,
  bleed_min_nonfatal = 1,
  bleed_fatal     = 0,
  cardio_death        = 0,
  st_cabg         = 0,
  st_pci     = 0,    
  mi_cabg         = 0,
  mi_med_manage   = 0,
  mi_pci     = 0,
  revasc_cabg     = 1,
  revasc_pci      = 1,
  cabg_bleed      = 1,
  dapt_stroke     = 0,
  
  secular_death = 0
  
)   
)



