# running the pharmaverse end-to-end ADSL example in blocks

options(repos = c(
  pharmaverse = 'https://pharmaverse.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

library(metacore)
library(metatools)
library(admiral.test)
library(admiral)
library(xportr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(haven)
library(readxl)


# Read in input SDTM data 
data("admiral_dm")
data("admiral_ex")
dm <- read_xpt("sdtm/dm.xpt")
ds <- read_xpt("sdtm/ds.xpt")
ex <- read_xpt("sdtm/ex.xpt")
vs <- read_xpt("sdtm/vs.xpt")
qs <- read_xpt("sdtm/qs.xpt")
mh <- read_xpt("sdtm/mh.xpt")
sv <- read_xpt("sdtm/sv.xpt")
my_spec <- read_xlsx("metadata/specs.xlsx", sheet = "Codelists")


# Read in metacore object 
load(metacore_example("pilot_ADaM.rda"))
metacore <- metacore %>% 
  select_dataset("ADSL")

metacore$ds_vars



#auxiliar functions
format_eoxxstt_nodef <- function(x) {
  case_when(
    x == "COMPLETED" ~ "COMPLETED",
    TRUE ~ "DISCONTINUED"
  )
}

format_agegr1n <- function(x) {
  case_when(
    x < 65 ~ 1,
    x <= 80 ~ 2,
    x > 80 ~ 3
  )
}


format_agegr1 <- function(x) {
  case_when(
    x == 1 ~ "<65",
    x == 2 ~ "65-80",
    x == 3 ~ ">80"
  )
}

format_visnumen <- function(x) {
  case_when(
    x ==13 ~ 12,
    TRUE ~ x
  )
}


format_racen <- function(x) {
  case_when(
    x == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
    x == "ASIAN" ~ 2,
    x == "BLACK OR AFRICAN AMERICAN" ~ 3,
    x == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 5,
    x == "WHITE" ~ 6,
    TRUE ~ 99
  )
}


format_armn <- function(x) {
  case_when(
    x=="Placebo" ~ 0,
    x=="Xanomeline Low Dose" ~54,
    x=="Xanomeline High Dose" ~81,
    TRUE ~ 99
  )
}


##auxiliar DS

#auxiliar dates

#MH

mh_disons <- mh %>%
  
  filter(MHCAT=="PRIMARY DIAGNOSIS") %>%
  
  derive_vars_dt(
    dtc = MHSTDTC,
    new_vars_prefix = "DISONS"
  )

#SV

sv_trtsdt <- sv %>%
  
  filter(VISITNUM==3) %>%
  
  derive_vars_dt(
    dtc = SVSTDTC,
    new_vars_prefix = "TRTS"
  )


#qsorres auxiliar (char to int)

qsorres <- qs %>%
  
  filter(QSCAT == "MINI-MENTAL STATE") %>%
  
  mutate_all(type.convert, as.is=TRUE)


adsl <- dm %>%
  
  #Predecesor DM variables (no computation)
  
  select (AGE,
          AGEU,
          ARM,
          DTHFL,
          ETHNIC,
          RACE,
          RFENDTC,
          RFSTDTC,
          SEX,
          SITEID,
          STUDYID,
          SUBJID,
          ARMCD,
          USUBJID)

adsla <- adsl %>%
  
  derive_vars_merged(
    dataset_add = ds,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(VISNUM = VISITNUM),
    filter_add = DSTERM =="PROTCOL COMPLETED"
  ) %>%
  
  derive_vars_merged(
    dataset_add = ds,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(DCDECOD = DSDECOD),
    filter_add = DSCAT == "DISPOSITION EVENT"
  ) %>%
  
  derive_vars_merged(
    dataset_add = vs,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(WEIGHTBL = VSSTRESN),
    filter_add = (VSTESTCD=="WEIGHT" & VISITNUM==3 )
  ) %>%
  
  derive_vars_merged(
    dataset_add = vs,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(HEIGHTBL = VSSTRESN),
    filter_add = (VSTESTCD=="HEIGHT" & VISITNUM==1 )
  ) %>%
  
  derive_vars_merged(
    dataset_add = mh_disons,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(DISONSDT )
  ) %>%
  
  derive_vars_merged(
    dataset_add = sv_trtsdt,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(TRTSDT )
  ) %>%
  
  mutate(
    
    EOSSTT = format_eoxxstt_nodef(DCDECOD),
    
    RACEN = format_racen(RACE),
    
    ARMN = format_armn(ARM),
    
    AGEGR1N = format_agegr1n(AGE),
    
    AGEGR1 = format_agegr1(AGEGR1N),
    
    BMIBL = WEIGHTBL / ((HEIGHTBL/100)**2),
    
    VISNUMEN = format_visnumen (VISNUM),
    
    RFENDT = convert_dtc_to_dt(RFENDTC),
    
    TRT01P = ARM,
    
    TRT01A = TRT01P,
    
    TRT01PN = ARMN,
    
    TRT01AN = TRT01PN,
    
    ITTFL = if_else(ARMCD!="", "Y", "N"),
    
    SAFFL = if_else(ITTFL=="Y" & !is.na(TRTSDT) , "Y", "N")
    
  )  %>%
  
  derive_var_merged_summary(
    dataset_add = qsorres,
    by_vars = vars(STUDYID, USUBJID),
    filter_add = (QSCAT == "MINI-MENTAL STATE" & !is.na(QSORRES) ),
    new_var = MMSETOT,
    analysis_var = QSORRES,
    summary_fun =function(x) sum(x, na.rm = TRUE)
  )





