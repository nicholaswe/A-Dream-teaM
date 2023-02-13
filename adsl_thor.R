# Name: ADSL
# Thor team
#
library(admiral)
library(metatools)
library(metacore)
library(haven)
library(dplyr)
library(lubridate)
library(stringr)
library(xportr)
library(magrittr)


# Use haven to read sdtm domains and clean missing values after read xpt file

dm <- convert_blanks_to_na(haven::read_xpt("sdtm/dm.xpt"))
sv <- convert_blanks_to_na(haven::read_xpt("sdtm/sv.xpt"))
ds <- convert_blanks_to_na(haven::read_xpt("sdtm/ds.xpt"))
sc <- convert_blanks_to_na(haven::read_xpt("sdtm/sc.xpt"))
qs <- convert_blanks_to_na(haven::read_xpt("sdtm/qs.xpt"))
ex <- convert_blanks_to_na(haven::read_xpt("sdtm/ex.xpt"))
vs <- convert_blanks_to_na(haven::read_xpt("sdtm/vs.xpt"))
mh <- convert_blanks_to_na(haven::read_xpt("sdtm/mh.xpt"))


# Format functions

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

# Using metacore for Predecessors variables
load(metacore_example("pilot_ADaM.rda"))

adsl_spec <- metacore %>%
  select_dataset("ADSL")

ds_list <- list("DM" = dm)

adsl <- build_from_derived(adsl_spec,
                           ds_list,
                           predecessor_only = FALSE,
                           keep = TRUE)
metacore <- spec_to_metacore("metadata/specs.xlsx", where_sep_sheet = FALSE, quiet = TRUE)


# Identifying unique patients with ARM = "Screen Failure" to exclude them
dm_exc <- dm %>%
  filter(ARM == "Screen Failure") %>%
  select(USUBJID) %>%
  unique()


# Function to derive variable SVSTDT to use it to derive COMPxxFL variables
# and filter by required VISITNUM
derive_compxxfl <- function(x) {
  sv_mod <- sv %>%
    filter(VISITNUM == x) %>%
    derive_vars_dt(
      new_vars_prefix = "SVST",
      dtc = SVSTDTC,
      highest_imputation = "n",
      date_imputation = "first"
    ) %>%
    derive_vars_merged_lookup(
      dataset_add = adsl_ct,
      by_vars = vars(STUDYID, USUBJID),
      new_vars = vars(RFENDT),
      print_not_mapped = TRUE
    )
  return(sv_mod)
}


# Function to be used in EOSSTT Derivarion in parameter: format_new_var
format_eoxxstt1 <- function(x) {
  case_when(
    x == "SCREEN FAILURE" ~ NA_character_,
    x == "COMPLETED" ~ "COMPLETED",
    TRUE ~ "DISCONTINUED"
  )
}


# Derive VISNUMEN in DS to merge it into "adsl_dt"
ds_mod <- ds %>%
  mutate(DSVISNUMEN = ifelse(DSTERM == "PROTOCOL COMPLETED" & VISITNUM == 13, 12,
                             ifelse(DSTERM == "PROTOCOL COMPLETED", VISITNUM, NA_character_))
  ) %>%
  create_var_from_codelist(
    metacore = metacore,
    input_var = DSDECOD,
    out_var = DCDECOD
  ) %>%
  create_var_from_codelist(
    metacore = metacore,
    input_var = DCDECOD,
    out_var = DCSREAS
  )



# Keep predeccesors variables from DM

adsl_ct <- adsl %>%
  create_cat_var(metacore, ref_var = AGE,
                 grp_var = AGEGR1, num_grp_var = AGEGR1N) %>%
  create_var_from_codelist(metacore = metacore,
                           input_var = RACE,
                           out_var = RACEN) %>%
  # Removing screen failures from ARM and TRT01P to match the define and FDA guidence
  mutate(ARM = if_else(ARM != "Screen Failure", ARM, NA_character_),
         TRT01P = if_else(TRT01P != "Screen Failure", TRT01P, NA_character_),
         TRT01A = TRT01P,
         SUBJID = SUBJID,
         SITEGR1 = SITEID,
         ARMN = case_when(ARM == "Placebo" ~ 0,
                          ARM == "Xanomeline Low Dose" ~ 54,
                          ARM == "Xanomeline High Dose" ~ 81)
  ) %>%
  # Creating TRT01PN and TRT01AN from specs codelist
  create_var_from_codelist(metacore = metacore,
                           input_var = TRT01P,
                           out_var = TRT01PN) %>%
  create_var_from_codelist(metacore = metacore,
                           input_var = TRT01A,
                           out_var = TRT01AN) %>%
  # Creating RFENDT variable
  derive_vars_dt(
    new_vars_prefix = "RFEN",
    dtc = RFENDTC,
    highest_imputation = "n",
    date_imputation = "first"
  ) %>%
  # Lookup for ARMCD variable from DM, it's not considered in the predeccesors variables list
  derive_vars_merged_lookup(
    dataset_add = dm,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(ARMCD),
    print_not_mapped = TRUE
  ) %>%
  # Removing screen failures from ARMCD
  mutate(
    ARMCD = if_else(ARMCD != "Scrnfail", ARMCD, NA_character_)
  ) %>%
  # Derive ITTFL variable
  derive_var_merged_exist_flag(
    dataset_add = dm, # Call the function to generate the sv_mod dataset, sending the visitnum num to filter it according specs
    by_vars = vars(STUDYID, USUBJID),
    new_var = IITFL,
    condition = !is.na(if_else(ARMCD != "Scrnfail", ARMCD, NA_character_)), #alternative: !is.na(if_else(!USUBJID %in% dm_exc$USUBJID, ARMCD, NA_character_)),
    #true_value = "Y",
    false_value = "N",
    missing_value = NA_character_
  ) %>%
  # Derive TRTSDT variable
  derive_vars_merged_dt(
    dataset_add = sv,
    filter_add = VISITNUM == 3,
    new_vars_prefix = "TRTS",
    dtc = SVSTDTC,
    order = vars(TRTSDT),
    mode = "first",
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  # Derive VISIT1DT variable
  derive_vars_merged_dt(
    dataset_add = sv,
    filter_add = VISITNUM == 1 & !USUBJID %in% dm_exc$USUBJID,
    new_vars_prefix = "VISIT1",
    dtc = SVSTDTC,
    date_imputation = "first",
    by_vars = vars(STUDYID, USUBJID),
    flag_imputation = "none"
  ) %>%
  # Using derive_var_merged_exist_flag admiral function to derived COMP8FL, COMP16FL and COMP24FL variables
  derive_var_merged_exist_flag(
    dataset_add = derive_compxxfl(8), # Call the function to generate the sv_mod dataset, sending the visitnum num to filter it according specs
    by_vars = vars(STUDYID, USUBJID),
    new_var = COMP8FL,
    condition = RFENDT >= SVSTDT,
    false_value = "N",
    missing_value = NA_character_
  ) %>%
  derive_var_merged_exist_flag(
    dataset_add = derive_compxxfl(10), # Call the function to generate the sv_mod dataset, sending the visitnum num to filter it according specs
    by_vars = vars(STUDYID, USUBJID),
    new_var = COMP16FL,
    condition = RFENDT >= SVSTDT,
    false_value = "N",
    missing_value = NA_character_
  ) %>%
  derive_var_merged_exist_flag(
    dataset_add = derive_compxxfl(12), # Call the function to generate the sv_mod dataset, sending the visitnum num to filter it according specs
    by_vars = vars(STUDYID, USUBJID),
    new_var = COMP24FL,
    condition = RFENDT >= SVSTDT,
    false_value = "N",
    missing_value = NA_character_
  ) %>%
  # Derive DCDECOD variable
  derive_vars_merged(
    dataset_add = ds,
    filter_add = DSCAT == 'DISPOSITION EVENT' & !USUBJID %in% dm_exc$USUBJID, # aternative: DSDECOD != "SCREEN FAILURE",
    new_vars = vars(DCDECOD = DSDECOD),
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  # Derive VISNUMEN variable
  derive_vars_merged(
    dataset_add = ds_mod,
    filter_add = DSTERM == "PROTOCOL COMPLETED",
    new_vars = vars(VISNUMEN = DSVISNUMEN),
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  # Derive DCSREAS, DCREASCD and DISCONFL variables. Alternative for DCREAS: derive_vars_disposition_reason()
  # Unfortunately the codelist in specs have different values
  mutate(
    DCSREAS = format_discreas(DCDECOD),
    DCREASCD = format_discreas(DCDECOD),
    DISCONFL = if_else(DCREASCD != "Completed", "Y", NA_character_),
    DSRAEFL = if_else(DCREASCD == "Adverse Event", "Y", NA_character_)
  ) %>%
  # Derive EOSSTT variable. Alternative: # mutate(EOSSTT = if_else(DCDECOD == "COMPLETED", "COMPLETED", "DISCONTINUED"))
  derive_var_disposition_status(
    dataset_ds = ds,
    new_var = EOSSTT,
    status_var = DSDECOD,
    format_new_var = format_eoxxstt1,
    filter_ds = DSCAT == "DISPOSITION EVENT"
  ) %>%
  # Derive DCSREAS variable
  derive_vars_disposition_reason(
    dataset_ds = ds,
    new_var = DCSREAS1,
    reason_var = DSDECOD,
    # new_var_spe = DISCREAS,
    reason_var_spe = DSTERM,
    filter_ds = DSCAT == "DISPOSITION EVENT"
  ) %>%
  # Derive HEIGHTBL variable
  derive_vars_merged(
    dataset_add = vs, # Call the function to generate the sv_mod dataset, sending the visitnum num to filter it according specs
    filter_add = VSTESTCD == "HEIGHT" & VISITNUM == 1,
    new_vars = vars(HEIGHTBL = VSSTRESN),
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  # Derive WEIGHTBL variable
  derive_vars_merged(
    dataset_add = vs, # Call the function to generate the sv_mod dataset, sending the visitnum num to filter it according specs
    filter_add = VSTESTCD == "WEIGHT" & VISITNUM == 3,
    new_vars = vars(WEIGHTBL = VSSTRESN),
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  # Derive BMIBL variable
  mutate(
    BMIBL = compute_bmi(HEIGHTBL, WEIGHTBL),
  ) %>%
  #Derive BMIBL variable
  create_cat_var(metacore = metacore,
                 ref_var = BMIBL,
                 grp_var = BMIBLGR1) %>%
  # Derive EDUCLVL variable
  derive_vars_merged(
    dataset_add = sc,
    filter_add = SCTESTCD == "EDLEVEL",
    new_vars = vars(EDUCLVL = SCSTRESN),
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  # Derive DISONSDT variable
  derive_vars_merged_dt(
    dataset_add = mh,
    filter_add = MHCAT == "PRIMARY DIAGNOSIS",
    new_vars_prefix = "DISONS",
    dtc = MHSTDTC,
    date_imputation = "first",
    flag_imputation = "none",
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  # Derive DURDIS variable #  alternative: mutate(DURDIS = as.numeric(VISIT1DT - DISONSDT)/30.4375)
  derive_vars_duration(
    new_var = DURDIS,
    start_date = DISONSDT,
    end_date = VISIT1DT,
    out_unit = "months",
    floor_in = FALSE,
    add_one = FALSE,
  ) %>%
  #Derive DURDSGR1 variable
  create_cat_var(metacore = metacore,
                 ref_var = DURDIS,
                 grp_var = DURDSGR1) %>%
  #Derive MMSETOT variable
  derive_var_merged_summary(
    dataset_add = qs,
    by_vars = vars(STUDYID, USUBJID),
    filter_add = QSCAT == "MINI-MENTAL STATE",
    new_var = MMSETOT,
    analysis_var = QSSTRESN,
    summary_fun = function(x) sum(x, na.rm = TRUE)
  )