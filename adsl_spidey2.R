
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
                     TRT01P = ARM, TRT01A = ACTARM)
