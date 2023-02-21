
# Libraries ---------------------------------------------------------------

library(metacore)
library(metatools)
library(admiral.test)
library(admiral)
library(xportr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(magrittr)


# Reading domains ---------------------------------------------------------

dm <- haven::read_xpt("sdtm/dm.xpt") %>%
      convert_blanks_to_na()

ds <- haven::read_xpt("sdtm/ds.xpt") %>%
      convert_blanks_to_na()

sc <- haven::read_xpt("sdtm/sc.xpt") %>%
      convert_blanks_to_na()

vs <- haven::read_xpt("sdtm/vs.xpt") %>%
      convert_blanks_to_na()

qs <- haven::read_xpt("sdtm/qs.xpt") %>%
      convert_blanks_to_na()

mh <- haven::read_xpt("sdtm/mh.xpt") %>%
      convert_blanks_to_na()

ex <- haven::read_xpt("sdtm/ex.xpt") %>%
      convert_blanks_to_na()

sv <- haven::read_xpt("sdtm/sv.xpt") %>%
  convert_blanks_to_na()

##################
### Derivation ###
##################

###############

# The first (and easiest) way to start building the derivation is by pulling through 
# all the columns that come directly from the SDTM datasets. 
# To do that we can use the "build_from_derived" function, but first we need to read the specs.xls

###############

# Reading specs -----------------------------------------------------------

# Reading specs from all datasets

metacore <- spec_to_metacore('metadata/specs.xlsx',
                             where_sep_sheet = F,
                             quiet = T) 

# Subsetting ADSL metacore

specs <- metacore %>%
         select_dataset("ADSL")

# Checking which datasets we'll pull from directly

build_from_derived(specs,
                   list(), 
                   predecessor_only = FALSE)

###############

# We can see that we need DM and ADSL datasets, but turns out that we don't have ADSL.
# To solve that, we must use the "pilot_ADaM.rda" (which is our data, once we are usig the official documentation data)
# as the predecessor variables.


# load(metacore_example("pilot_ADaM.rda")) # This will mask our "metacore" object

# metacore_aux <- metacore # Assigning metacore to metacore_aux

# rm(metacore) # Removing "metacore" object, we'll use this name

# specs_aux <- metacore_aux %>%       # Subsetting ADSL auxiliar metacore
#              select_dataset("ADSL")

# adsl_aux <- build_from_derived(specs_aux,
#                                list("DM" = dm),
#                                predecessor_only = FALSE,
#                                keep = TRUE)


# Now our base dataset is in adsl_aux and  we can start to create some variables.
# If our original dataset was working, we could run

# metacore <- spec_to_metacore('metadata/specs.xlsx',
#                              where_sep_sheet = F,
#                              quiet = T) 
 
# # Subsetting ADSL metacore
 
# specs <- metacore %>%
#          select_dataset("ADSL")

# # Checking which datasets we'll pull from directly
 
# build_from_derived(specs,
#                    list(), 
#                    predecessor_only = FALSE)

# adsl_preds <- build_from_derived(specs, 
#                                  ds_list = list("dm" = dm), 
#                                  predecessor_only = FALSE, 
#                                  keep = TRUE)


# Another option is to associate the variables manually

###############

# Base dataset

adsl_preds <- dm %>%
              transmute(AGE = AGE, AGEU = AGEU, ARM = ARM, DTHFL = DTHFL, ETHNIC = ETHNIC,
                        RACE = RACE, RFENDTC = RFENDTC, RFSTDTC = RFSTDTC, SEX = SEX,
                        SITEID = SITEID, STUDYID = STUDYID, SUBJID = SUBJID, USUBJID = USUBJID,
                        TRT01P = ARM, TRT01A = ACTARM, ARMCD = ARMCD)

# AGEGR1, AGEGR1N and RACEN

adsl_ct <- adsl_preds %>% 
           create_cat_var(specs, 
                          ref_var = AGE, 
                          grp_var = AGEGR1, 
                          num_grp_var = AGEGR1N) %>% 
           create_var_from_codelist(specs, 
                                    input_var = RACE, 
                                    out_var = RACEN) %>% 
           # Removing screen failures from ARM and TRT01P to match the define and FDA guidence
           mutate(ARM = if_else(ARM == "Screen Failure", NA_character_, ARM),
                  TRT01P = case_when(
                           TRT01P == "Screen Failure" ~ NA_character_,
                           TRUE ~ TRT01P)) %>% 
           mutate(ITTFL = if_else(ARMCD =="","N","Y"))
          

### TRTSDTM, TRTEDTM and TRTDURD




# To derive these domains we'll need to use the "ex" domains with some pre-processing step
# Basically, we'll convert EX.EXSTDTC and EX.EXSTDTC to datetime variables and input missing components.
  
ex_aux <- ex %>%
          derive_vars_dtm(                               # Derive/Impute a Datetime from a Date Character Vector
                          dtc = EXSTDTC,                 # The "--DTC" date to input
                          new_vars_prefix = "EXST") %>%  # Prefix used for the output variable(s)
          derive_vars_dtm(
                          dtc = EXENDTC,
                          new_vars_prefix = "EXEN",
                          time_imputation = "last")      # The value to impute when a time is missing ("last" is the end of the day)


adsl_trt <- adsl_ct %>%
            derive_vars_merged(                       # Add New Variable(s) to the Input Dataset Based on Variables from Another Datase
                               dataset_add = ex_aux,  # Additional dataset, adding "ex_aux" to "adsl"
                               filter_add = (EXDOSE > 0 |(EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM), # Filter for additional dataset	
                               new_vars = vars(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF), # Variables to add
                               order = vars(EXSTDTM, EXSEQ), # Sort order
                               mode = "first", # Selection mode, determines if the first or last observation is selected
                               by_vars = vars(STUDYID, USUBJID)) %>% # Grouping variables
            derive_vars_merged(
                               dataset_add = ex_aux,
                               filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
                               new_vars = vars(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
                               order = vars(EXENDTM, EXSEQ),
                               mode = "last",
                               by_vars = vars(STUDYID, USUBJID)) %>% 
            derive_vars_dtm_to_dt( # Derive Date Variables from Datetime Variables
                                  source_vars = vars(TRTSDTM, TRTEDTM)) %>%  # A list of datetime variables from which dates are to be extracted
            derive_var_trtdurd()



### DCDECOD 

# PRE_DCDECOD <- ds %>% select(USUBJID, DSDECOD, DSCAT)
# 
# DCDECOD <- PRE_DCDECOD %>%  filter (DSCAT == "DISPOSITION EVENT") %>% 
#                             mutate(DCDECOD = DSDECOD) %>% 
#                             select(USUBJID, DCDECOD) %>% 
#                             create_var_from_codelist(specs, 
#                                                      input_var = DCDECOD, 
#                                                      out_var = DCREASCD)

DCDECOD <- ds %>% select(USUBJID, DSDECOD, DSCAT) %>% 
                  filter (DSCAT == "DISPOSITION EVENT") %>% 
                  mutate(DCDECOD = DSDECOD ) %>% 
                  select(USUBJID, DCDECOD)

### DCREASCD

DCREASCD <- DCDECOD %>% mutate(DCREASCD = DCDECOD) %>% 
            select(USUBJID, DCREASCD)

### DISCONFL

DISCONFL <-  DCREASCD %>% 
             mutate( DISCONFL = if_else(toupper(DCREASCD) != "COMPLETED","Y","")) %>% 
             select(USUBJID, DISCONFL)
             


### VISNUMEN 

ds_aux <- ds %>% select(USUBJID, DSTERM, VISITNUM)

ds_aux %<>% filter (DSTERM == "PROTOCOL COMPLETED") %>% 
            mutate(VISNUMEN = case_when(
                              VISITNUM == 13 & DSTERM == "PROTOCOL COMPLETED" ~ 12,
                              TRUE ~ VISITNUM))%>% 
            select(USUBJID, VISNUMEN)
            
            
            
### WEIGHTBL

PRE_WEIGHTBL <- vs %>% select(USUBJID, VISITNUM, VSTESTCD, VSSTRESN)
            
WEIGHTBL <- PRE_WEIGHTBL %>%  filter (VSTESTCD == "WEIGHT" & VISITNUM == 3) %>% 
              mutate(WEIGHTBL = VSSTRESN %>% round(.,1)) %>% 
              select(USUBJID, WEIGHTBL )



### HEIGHTBL

PRE_HEIGHTBL <- vs %>% select(USUBJID, VISITNUM, VSTESTCD, VSSTRESN)

HEIGHTBL <- PRE_HEIGHTBL %>%  filter (VSTESTCD == "HEIGHT" & VISITNUM == 1) %>% 
  mutate(HEIGHTBL = VSSTRESN  %>% round(.,1)) %>% 
  select(USUBJID, HEIGHTBL)


### EDUCLVL

PRE_EDUCLVL <- sc %>% select(USUBJID, SCSTRESN, SCTESTCD)

EDUCLVL <- PRE_EDUCLVL %>%  filter (SCTESTCD == "EDLEVEL") %>% 
  mutate(EDUCLVL = SCSTRESN) %>% 
  select(USUBJID, EDUCLVL)

### BMIBL

BMIBL <- left_join(HEIGHTBL, WEIGHTBL,  by = "USUBJID") %>% 
         mutate(BMIBL = WEIGHTBL/((HEIGHTBL/100)^2)) %>% 
         create_cat_var(specs, 
                        ref_var = BMIBL, 
                        grp_var = BMIBLGR1)

### MMSETOT

MMSETOT <-  qs %>% select(USUBJID, QSORRES, QSCAT) %>% 
        filter(QSCAT == "MINI-MENTAL STATE") %>% 
        mutate(QSORRES = QSORRES %>% as.numeric) %>% 
        group_by(USUBJID) %>% 
        summarize (MMSETOT = sum(QSORRES))


###   DISONSDT

DISONSDT <- mh %>% select(USUBJID, MHSTDTC, MHCAT) %>% 
            filter(MHCAT == "PRIMARY DIAGNOSIS")   %>% 
            mutate(DISONSDT = MHSTDTC %>% ymd)  %>% 
            select(USUBJID, DISONSDT)

###   TRTSDT

TRTSDT <-  sv %>% select(USUBJID, SVSTDTC, VISITNUM) %>% 
                  filter(VISITNUM == 3) %>% 
                  mutate(TRTSDT = SVSTDTC %>% ymd)  %>% 
                  select(USUBJID, TRTSDT)

###  TRTEDT


TRTEDT <- ex %>% select(USUBJID, EXENDTC, VISITNUM, VISIT, EXDOSE) %>%
  arrange(USUBJID, desc(EXENDTC)) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>% 
  mutate(TRTEDT = EXENDTC%>% ymd) %>% 
  select(USUBJID, TRTEDT)

###   COMPxxFL

COMPxxFL <- function(sv, adsl, visitnum, xx){

vis_usub <- sv %>% filter(VISITNUM==visitnum) %>% 
                   select(USUBJID, SVSTDTC, VISITNUM) 

out <- full_join(vis_usub, adsl, by ="USUBJID") %>% mutate(SVSTDTC = SVSTDTC %>% ymd, RFENDTC = RFENDTC %>% ymd) %>% 
                                                    mutate(aux = if_else(VISITNUM == visitnum & RFENDTC>=SVSTDTC,"Y","N")) %>%
                                                    mutate(!!paste0("COMP", xx, "FL") := ifelse(is.na(aux), "N", "Y")) %>% 
                                                    select(USUBJID, !!paste0("COMP", xx, "FL")) %>% 
                                                    arrange(., USUBJID)  

        return(out)

}

COMP16FL <- COMPxxFL(sv, adsl_preds, 10, 16)
COMP24FL <- COMPxxFL(sv, adsl_preds, 12, 24)
COMP8FL <- COMPxxFL(sv, adsl_preds, 8, 8)



### VISIT1DT

VISIT1DT <-  sv %>% select(USUBJID, SVSTDTC, VISITNUM) %>% 
  filter(VISITNUM == 1) %>% 
  mutate(VISIT1DT = SVSTDTC %>% ymd) %>% 
  select(USUBJID, VISIT1DT) 

### SAFFL (CHECK THIS ONE WITH NICHOLAS)

SAFFL <- left_join(adsl_ct %>% select(USUBJID, ITTFL), TRTSDT, by ="USUBJID") %>% 
         mutate( SAFFL = if_else(ITTFL == "Y" & is.na(TRTSDT) ,"N","Y")) %>% 
         select(USUBJID, SAFFL)

### EFFFL

qs %>%filter((QSTEST=="ADAS-COG(11) Subscore" & VISITNUM > 3) &
               (QSTEST=="EXTENT OF CHANGE, IF ANY, SINCE BASELINE" & VISITNUM > 3)
             ) %>%  count(QSTEST, VISITNUM) 


EFFFL <- qs %>%  select(USUBJID, QSTEST, VISITNUM) %>% 
                 filter((QSTEST=="ADAS-COG(11) Subscore" & VISITNUM > 3) & 
                          (QSTEST=="EXTENT OF CHANGE, IF ANY, SINCE BASELINE" & VISITNUM > 3)
                          )

#table(ex$EXDOSE, ex$EXTRT)
# Next (last) step: merge with ADSL


