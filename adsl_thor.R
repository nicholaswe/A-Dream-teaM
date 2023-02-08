# Name: ADSL
#
library(admiral)
library(metatools)
library(metacore)
library(haven)
library(dplyr)
library(lubridate)
library(stringr)
library(xportr)


# Use haven to read sdtm domains

dm <- haven::read_xpt("sdtm/dm.xpt")
sv <- haven::read_xpt("sdtm/sv.xpt")
ds <- haven::read_xpt("sdtm/ds.xpt")
sc <- haven::read_xpt("sdtm/sc.xpt")
qs <- haven::read_xpt("sdtm/qs.xpt")
ex <- haven::read_xpt("sdtm/ex.xpt")


# Clean missing values after read xpt file

dm <- convert_blanks_to_na(dm)
sv <- convert_blanks_to_na(sv)
ds <- convert_blanks_to_na(ds)
sc <- convert_blanks_to_na(sc)
qs <- convert_blanks_to_na(qs)
ex <- convert_blanks_to_na(ex)


# Format functions

format_trt01xn <- function(x) {
  case_when(
    x == "Placebo" ~ 0,
    x == "Xanomeline Low Dose" ~ 54,
    x == "Xanomeline High Dose" ~ 81
  )
}

format_agegr1 <- function(x) {
  case_when(
    x < 65 ~ "<65",
    between(x, 65, 80) ~ "65-80",
    x > 80 ~ ">80",
    TRUE ~ "Missing"
  )
}

format_agegr1n <- function(x) {
  case_when(
    x == "<65" ~ 1,
    x == "65-80" ~ 2,
    x == ">80" ~ 3
  )
}

format_racen <- function(x) {
  case_when(
    x == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
    x == "ASIAN" ~ 2,
    x == "BLACK OR AFRICAN AMERICAN" ~ 3,
    x == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 5,
    x == "WHITE" ~ 6
  )
}

format_discreas <- function(x) {
  case_when(
    x == "COMPLETED" ~ "Completed",
    x == "ADVERSE EVENT" ~ "Adverse Event",
    x == "DEATH" ~ "Death",
    x == "I/E NOT MET" ~ "I/E Not Met",
    x == "LACK OF EFFICACY" ~ "Lack of Efficacy",
    x == "LOST TO FOLLOW-UP" ~ "Lost to Follow-up",
    x == "PHYSICIAN DECISION" ~ "Physician Decision",
    x == "PROTOCOL VIOLATION" ~ "Protocol Violation",
    x == "STUDY TERMINATED BY SPONSOR" ~ "Sponsor Decision",
    x == "WITHDRAWAL BY SUBJECT" ~ "Withdrew Consent",
    TRUE ~ NA_character_
  )
}


# Select dm variables

adsl <- dm %>%
  select(STUDYID, USUBJID, SUBJID, SITEID, ARM, AGE, AGEU, RACE, SEX, ETHNIC, DTHFL, RFSTDTC, RFENDTC) %>%
  filter(ARM != "Screen Failure") %>%
  mutate(SITEGR1 = SITEID,
         TRT01P = ARM,
         TRT01PN = format_trt01xn(TRT01P),
         TRT01A = TRT01P,
         TRT01AN = format_trt01xn(TRT01A),
         AGEGR1 = format_agegr1(AGE),
         AGEGR1N = format_agegr1n(AGEGR1),
         AGEU = "YEARS",
         RACEN = format_racen(RACE)
  ) %>%
  derive_vars_dt(
    new_vars_prefix = "RFEN",
    dtc = RFENDTC,
    highest_imputation = "n",
    date_imputation = "first"
  )

# Merge ADSL with SV, DS and  EX dataframes

sv_df <- sv %>%
  select(STUDYID, USUBJID, VISITNUM, SVSTDTC) %>%
  derive_vars_dt(
    new_vars_prefix = "SVST",
    dtc = SVSTDTC,
    highest_imputation = "n",
    date_imputation = "first"
  )

# Creating variables VISNUMEN, DCDECOD, EOSSTT, DCSREAS, DCREASCD to use in derive_vars_merged() with ADSL
# SCREEN FAILURE are excluded
ds_df <- ds %>%
  mutate(
    DSVISNUMEN = ifelse(DSTERM == "PROTCOL COMPLETED" & VISITNUM == 13, 12,
                        ifelse(DSTERM == "PROTCOL COMPLETED", VISITNUM, NA_character_)),
    DSDCDECOD = if_else(DSCAT == "DISPOSITION EVENT", DSDECOD, NA_character_),
    DSEOSSTT = ifelse(DSDCDECOD == "COMPLETED", "COMPLETED",
                      ifelse(!is.na(DSDCDECOD), "DISCONTINUED", NA_character_)),
    DSDCSREAS = format_discreas(DSDCDECOD),
    DSDCREASCD =format_discreas(DSDCDECOD),
  ) %>%
  filter(DSDECOD != "SCREEN FAILURE" & !is.na(DSDCDECOD))

# Convert EXENDTC from char to date type
ex_df <- ex %>%
  select(STUDYID, USUBJID, EXENDTC) %>%
  derive_vars_dt(
    new_vars_prefix = "EXEN",
    dtc = EXENDTC,
    highest_imputation = "n",
    date_imputation = "last"
  )

# Merging ADSL with SV, DS and  EX dataframes

adsl_svex <- adsl %>%
  derive_vars_merged(
    dataset_add = sv_df,
    filter_add = VISITNUM == 3,
    new_vars = vars(TRTSDT = SVSTDT),
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = sv_df,
    filter_add = VISITNUM == 1,
    new_vars = vars(VISIT1DT = SVSTDTC),
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = sv_df,
    filter_add = VISITNUM == 8,
    new_vars = vars(COMP8DT = SVSTDTC),
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = sv_df,
    filter_add = VISITNUM == 10,
    new_vars = vars(COMP16DT = SVSTDTC),
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = sv_df,
    filter_add = VISITNUM == 12,
    new_vars = vars(COMP24DT = SVSTDTC),
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = ds_df,
    new_vars = vars(VISNUMEN = DSVISNUMEN, DCDECOD = DSDCDECOD, EOSSTT = DSEOSSTT,
                    DCSREAS = DSDCSREAS, DCREASCD = DSDCREASCD),
    by_vars = vars(STUDYID, USUBJID)
  )
