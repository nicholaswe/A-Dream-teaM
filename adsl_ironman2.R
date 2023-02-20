# ADSL program for Ironman team
# Programmers : Naveen
#               Nelson
# Notes: Naveen from 192 - 271 and Nelson 218 - 240


install.packages("admiral")
install.packages("admiral.test")
install.packages("dplyr")
install.packages("lubridate")
install.packages("stringr")
install.packages("xportr")
install.packages("haven")
install.packages("tidyverse")
install.packages("readxl")
install.packages("plyr")

library(admiral)
library(admiral.test) 
library(dplyr)
library(lubridate)
library(stringr)
library(xportr)
library(haven)
library(tidyverse)
library(readxl)
library(plyr)
library (data.table)
# Load source xpt datasets ----
ae <- haven::read_xpt("/cloud/project/sdtm/ae.xpt")
cm <- haven::read_xpt("/cloud/project/sdtm/cm.xpt")
dm <- haven::read_xpt("/cloud/project/sdtm/dm.xpt")
ds <- haven::read_xpt("/cloud/project/sdtm/ds.xpt")
ex <- haven::read_xpt("/cloud/project/sdtm/ex.xpt")
qs <- haven::read_xpt("/cloud/project/sdtm/qs.xpt")
sv <- haven::read_xpt("/cloud/project/sdtm/sv.xpt")
suppdm <- haven::read_xpt("/cloud/project/sdtm/suppdm.xpt")

# Reading ADSL specifications from specs sheet

adsl_spec <- readxl::read_xlsx("/cloud/project/metadata/specs.xlsx", sheet = "Variables") %>% 
  dplyr::rename(type = "Data Type") %>% 
  rlang::set_names(tolower) %>%
  filter(dataset == "ADSL") %>%
  mutate(format = str_to_lower(format))


# Reading CODE list specifications from codelist
codelist <- readxl::read_xlsx("/cloud/project/metadata/specs.xlsx", sheet = "Codelists") %>% 
  dplyr::rename(type = "Data Type",decodValue="Decoded Value") %>%
  rlang::set_names(tolower) 
  


# Replacing spece with NA in sas datasets ----

ae <- convert_blanks_to_na(ae)
cm <- convert_blanks_to_na(cm)
dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
qs <- convert_blanks_to_na(qs)
suppdm <- convert_blanks_to_na(suppdm)


# 218 MMSETOT as Sum of QS.QSORRES values for the subject when QSCAT = MINI-MENTAL STATE ----

 #Sum of QS.QSORRES values for the subject when QSCAT = MINI-MENTAL STATE
 MMSETOF <- qs %>% 
   filter (QSCAT=="MINI-MENTAL STATE") %>% 
     select(USUBJID,QSORRES) %>%
   group_by (USUBJID) %>% 
   mutate(QSORRES=as.numeric(QSORRES),
          MMSETOT=sum(QSORRES,na.rm=TRUE)) %>% 
   arrange(USUBJID) %>% 
   distinct(USUBJID)
   
# 219 RACE (compute DM.RACE)---- 222 Add RFENDTC , 222 RFSTDTC
 dm_race <- dm %>% 
    select(USUBJID, RACE,RFENDTC,RFSTDTC) %>%  
 mutate(term=RACE) # create a new variable named term equal to RACE variable
 
# 220 RACEN ----
 # Create a lookup table filtering with RACE numeric values
 codelistRace <- codelist %>% 
dplyr::rename(RACEN = "decodvalue") %>% 
   select(name,term,RACEN) %>% 
   filter(name=="RACE")
 
 # Add RACE numeric values to dm_race  
 dm_racen <- dm_race %>%
   # Join DMRACE with Codelist to get RACEN 
   derive_vars_merged_lookup(
     dataset_add = codelistRace,
     new_vars = vars(RACEN),
     by_vars = vars(term),  
     print_not_mapped = TRUE,
     check_type = "warning"
   ) %>% 
   # 221 RFENDT = RFENDTC converted to SAS date
   derive_vars_dt(
     new_vars_prefix = "RFEN",
     dtc = RFENDTC
   ) 

dm_racen <- dm_racen %>% 
     select(-term) 
     

# 236 TRTSDT (SV.SVSTDTC when SV.VISITNUM=3, converted to SAS date)


sv


# 224 Calculate SAFFL ('Y' if ITTFL='Y' and TRTSDT ne missing. 'N' otherwise) ----

mutate(
  
  SAFFL=if_else(ITTFL = "Y","elderly","adult") %>% 
    select(USBJID, AGE,AGEGR)
)


 
 
 
 
   usethis::use_git_config(user.name = "nelsonamurciab@gmail.com",
                           user.email = "murciabn")
 
   
   
 
   gitcreds::gitcreds_set()

