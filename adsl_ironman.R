# ADSL program for Ironman team
# Programmers : Naveen

#install.packages("admiral")
#install.packages("admiral.test")
#install.packages("xportr")
#install.packages("readxl")
#install.packages("haven")


library(admiral)
library(admiral.test)
library(dplyr)
library(lubridate)
library(stringr)
library(xportr)
library(readxl)
library(haven)

# Load source xpt datasets ----

dm <- haven::read_xpt("/cloud/project/sdtm/dm.xpt")
ds <- haven::read_xpt("/cloud/project/sdtm/ds.xpt")
ex <- haven::read_xpt("/cloud/project/sdtm/ex.xpt")


# Reading ADSL specifications from specs sheet

adsl_spec <- readxl::read_xlsx("/cloud/project/metadata/specs.xlsx", sheet = "Variables") %>% 
      rlang::set_names(tolower) %>%
      filter(dataset == "ADSL") %>%
      mutate(format = str_to_lower(format))

# Replacing spece with NA in sas datasets ----

dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)

# Gets a list of DM vars required for derivations 
dm_vars <- vars(STUDYID, USUBJID, SUBJID, SITEID, ARM, AGE, AGEU, RACE, SEX, ETHNIC, RFSTDTC, RFENDTC, DTHFL)
dm_vars_v <- c("STUDYID", "USUBJID", "SUBJID", "SITEID", "ARM", "AGE", "AGEU", "RACE", "SEX", "ETHNIC", "RFSTDTC", "RFENDTC", "DTHFL")

# Gets a list of EX vars required for derivations 
ex_vars <- vars(STUDYID, USUBJID, EXSTDTC, EXENDTC, EXSEQ)
ex_vars_v <- c("STUDYID", "USUBJID", "EXSTDTC", "EXENDTC", "EXSEQ")


# Gets a list of DS vars required for derivations 
ds_vars <- vars(STUDYID, USUBJID, DSTERM, DSDECOD, DSCAT, VISITNUM, VISIT, DSSTDTC)
ds_vars_v <- c("STUDYID", "USUBJID", "DSTERM", "DSDECOD", "DSCAT", "VISITNUM", "VISIT", "DSSTDTC")


# User defined functions -----

format_agegr1 <- function(var) {
   case_when(
     var < 65 ~ "<65",
     between(var, 65, 80) ~ "65-80",
     var >80 ~ ">80"
   )  
}

format_agegr1n <- function(var) {
  case_when(
    var < 65 ~ 1,
    between(var, 65, 80) ~ 2,
    var >80 ~ 3
  )  
}

format_trtn <- function(var) {
  case_when(
    var == "Placebo" ~ 0,
    var == "Xanomeline Low Dose" ~ 54,
    var == "Xanomeline High Dose" ~ 81
  )  
}

format_race <- function(var) {
  case_when(
    var == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
    var == "ASIAN" ~ 2,
    var == "BLACK OR AFRICAN AMERICAN" ~ 3,
    var == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 4,
    var == "WHITE" ~ 5
    
  )  
}


# deriving AGEGR1, AGEGR1N -----

adsl_agegr <- dm %>% 
  filter(ARM != "Screen Failure") %>%
  select(all_of(dm_vars_v)) %>%
  mutate(AGEGR1 = format_agegr1(AGE),
         AGEGR1N = format_agegr1n(AGE),
         .after = AGE)


# deriving TRT01P, TRT01A, TRT01PN, TRT01AN  ----


adsl_trt <- adsl_agegr %>%
    mutate(TRT01P = ARM,
           TRT01PN = format_trtn(TRT01P),
           TRT01A = TRT01P,
           TRT01AN = format_trtn(TRT01A),
           .after = ARM,
           )

# Deriving RFENDT ------

adsl_rfendt <- derive_vars_dt(
  adsl_trt, 
  dtc = RFENDTC,
  new_vars_prefix = "RFEN"
)


# Deriving RACEN  ------

adsl_racen <- adsl_rfendt %>%
  mutate(RACEN = format_race(RACE),
         .after = RACE)


# Deriving ITTFL   ------

adsl_ittfl <- adsl_racen %>%
  mutate(ITTFL = case_when(
    ARM != "" ~ 'Y',
    .default = "N"))


# Process DS domain -------------


ds_dscat <- ds %>%
  select(all_of(ds_vars_v)) %>%
  filter(DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE") %>%
  mutate(
    DCDECOD = DSDECOD,
    DCSREAS = case_when( DSTERM == "PROTOCOL ENTRY CRITERIA NOT MET" & DCDECOD =="PROTOCOL VIOLATION" ~ "I/E Not Met",
                         DCDECOD == "ADVERSE EVENT"  ~ "Adverse Event",
                         DCDECOD == "STUDY TERMINATED BY SPONSOR"  ~ "Sponsor Decision",
                         DCDECOD == "DEATH"  ~ "Death",
                         DCDECOD == "WITHDRAWAL BY SUBJECT"  ~ "Withdrew Consent",
                         DCDECOD == "PHYSICIAN DECISION"  ~ "Physician Decision",
                         DCDECOD == "PROTOCOL VIOLATION"  ~ "Protocol Violation",
                         DCDECOD == "LOST TO FOLLOW-UP"  ~ "Lost to Follow-up",
                         DCDECOD == "LACK OF EFFICACY"  ~ "Lack of Efficacy"
    ),
    EOSSTT = case_when(
      DCDECOD == "COMPLETED" ~ "COMPLETED",
      DCDECOD != "COMPLETED" ~ "DISCONTINUED"
    ),
    VISNUMEN = case_when(
      VISITNUM == "13" & DSTERM == "PROTOCOL COMPLETED" ~ 12,
      VISITNUM != "13" & DSTERM == "PROTOCOl COMPLETED" ~ VISITNUM
    ),
    DISCONFL = case_when(
      DCSREAS != "COMPLETED" & !is.na(DCSREAS) ~ "Y"
    ),
    DSRAEFL = case_when(
      DCSREAS != "Adverse Event" ~ "Y"
    )) 


  
  
# test count to see if we have subjects with more than one disposition event

# ds_sub_count <- ds_dscat %>%
#   select(USUBJID) %>%
#   group_by(USUBJID)
# 
# ds_cnt <- ds_sub_count %>%
#   summarise(n = n()) %>%
#   filter(n > 1)

# Derive EXENDTM and EXSTDTM ------------

ex_ext <- ex %>%
  select(all_of(ex_vars_v)) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  ) %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  )

# Merge DM and EX and add extra columns  ------------

dm_ex <- adsl_ittfl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    new_vars = vars(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = vars(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = vars(STUDYID,USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    new_vars = vars(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = vars(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = vars(STUDYID,USUBJID)
  )

# Derive DT using DTM for EX variables ------------

dm_ex_tm <- dm_ex %>%
  derive_vars_dtm_to_dt(source_vars = vars(TRTSDTM, TRTEDTM))


# Derive TRTDUR -----------

dm_ex_trtdur <- dm_ex_tm %>%
  derive_var_trtdurd()

aest_enddt_na <- ex_ext %>%
  filter(is.na(EXENDTC))



