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

# Read in input SDTM data 
data("admiral_dm")
data("admiral_ex")

# Read in metacore object 
load(metacore_example("pilot_ADaM.rda"))
metacore <- metacore %>% 
  select_dataset("ADSL")

metacore$ds_vars




#qsorres auxiliar (char to int)

qsorres<-qs %>%
  filter(QSCAT == "MINI-MENTAL STATE") %>%
  mutate_all(type.convert, as.is=TRUE)


#auxiliar functions
format_eoxxstt_nodef <- function(x) {
  case_when(
    x == "COMPLETED" ~ "COMPLETED",
    TRUE ~ "DISCONTINUED"
  )
}

format_agegr1 <- function(x) {
  case_when(
    x < 65 ~ 1,
    x <= 80 ~ 2,
    x > 80 ~ 3
  )
}

format_visnumen <- function(x) {
  case_when(
    x ==13 ~ 12,
    TRUE ~ x
  )
}


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
          ARM,
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
  mutate(
    EOSSTT = format_eoxxstt_nodef(DCDECOD),
    AGEGR1N = format_agegr1(AGE),
    AGEGR1 = as.character(AGEGR1N),
    BMIBL = WEIGHTBL / ((HEIGHTBL/100)**2),
    VISNUMEN = format_visnumen (VISNUM),
    RFENDT=convert_dtc_to_dt(RFENDTC)
  ) %>%
  derive_var_merged_summary(
    dataset_add = qs,
    by_vars = vars(USUBJID),
    filter_add = QSCAT = "MINI-MENTAL STATE",
    new_var = MMSETOT,
    analysis_var = QSORRES,
    summary_fun = function(x) sum(x, collapse = ", ")
  )  %>%
  derive_var_merged_summary(
    dataset_add = qsorres,
    by_vars = vars(STUDYID, USUBJID),
    filter_add = (QSCAT == "MINI-MENTAL STATE" & !is.na(QSORRES) ),
    new_var = MMSETOT,
    analysis_var = QSORRES,
    summary_fun =function(x) sum(x, na.rm = TRUE)
  ) %>%
  derive_var_merged_exist_flag(
    dataset_add = vs,
    by_vars = vars(STUDYID, USUBJID),
    new_var = COMP8FL,
    condition = (VISITNUM==8 | RFENDT>= convert_dtc_to_dt(VSDTC))
  ) %>%
  derive_var_merged_exist_flag(
    dataset_add = vs,
    by_vars = vars(STUDYID, USUBJID),
    new_var = COMP8FL,
    condition = (VISITNUM==8 & ENDDT >= convert_dtc_to_dt(VSDTC))
  )


