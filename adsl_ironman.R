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

# Gets a list of AM vars required for derivations 
# NOTE: Look at the difference between the objects
dm_vars <- vars(STUDYID, USUBJID, SUBJID, SITEID, ARM, AGE, AGEU, RACE, SEX, ETHNIC, RFSTDTC, RFENDTC, DTHFL)
dm_vars_v <- c("STUDYID", "USUBJID", "SUBJID", "SITEID", "ARM", "AGE", "AGEU", "RACE", "SEX", "ETHNIC", "RFSTDTC", "RFENDTC", "DTHFL")


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
