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
lb <- haven::read_xpt("/cloud/project/sdtm/lb.xpt")
qs <- haven::read_xpt("/cloud/project/sdtm/qs.xpt")
vs <- haven::read_xpt("/cloud/project/sdtm/vs.xpt")
mh <- haven::read_xpt("/cloud/project/sdtm/mh.xpt")
suppae <- haven::read_xpt("/cloud/project/sdtm/suppae.xpt")
suppdm <- haven::read_xpt("/cloud/project/sdtm/suppdm.xpt")
se <- haven::read_xpt("/cloud/project/sdtm/se.xpt")
relrec <- haven::read_xpt("/cloud/project/sdtm/relrec.xpt")
sc <- haven::read_xpt("/cloud/project/sdtm/sc.xpt")
ts <- haven::read_xpt("/cloud/project/sdtm/ts.xpt")
tv <- haven::read_xpt("/cloud/project/sdtm/tv.xpt")
ta <- haven::read_xpt("/cloud/project/sdtm/ta.xpt")
ti <- haven::read_xpt("/cloud/project/sdtm/ti.xpt")
te <- haven::read_xpt("/cloud/project/sdtm/te.xpt")
suppds <- haven::read_xpt("/cloud/project/sdtm/suppds.xpt")
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
# 229 DM.SUBJID
# 232 DM.ARM
# 228 TRT01A=TRT01P, i.e., no difference between actual and randomized treatment in this study.
dm_vars <- dm %>% 
  select(USUBJID, RACE,RFENDTC,RFSTDTC,SEX,SITEID,STUDYID,ARM) %>%  
  mutate(term=RACE,# create a new variable named term equal to RACE variable
         SUBJID=USUBJID, # 237  DM.SUBJID
         TRT01P=ARM, 
         TRT01A=TRT01P)

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
  ) %>% 
  select(-term)



# 236 TRTSDT (SV.SVSTDTC when SV.VISITNUM=3, converted to SAS date)

sv_visit_3 <- sv %>% 
  select(USUBJID,VISITNUM,SVSTDTC) %>% 
  filter(VISITNUM==3) %>% 
  derive_vars_dt( # Create a date variable named RFENDT from character variable SVSTDTC
    new_vars_prefix = "TRTS",
    dtc = SVSTDTC
  )

# Left join
dm_TRTSDT <- dm_vars_ %>% 
  derive_vars_merged(
    dataset_add = sv_visit_3,
    new_vars = vars(TRTSDT),
    by_vars = vars(USUBJID)  
  ) 


# 224 Calculate SAFFL ('Y' if ITTFL='Y' and TRTSDT ne missing. 'N' otherwise) ----
#Note: make a left join to add SAFFL from Naveen Derivations.

dm_TRTSDT <-dm_TRTSDT
mutate(
  SAFFL=case_when(
    ITTFL = "Y" & ! is.na(SVSTDTC)~ "Y"  
    TRUE ~ "N"))



# 233 Numeric code for TRT01A which corresponds to the randomized dose ----
# Create a lookup table filtering with ARMN for get numeric values
codelistArmN <- codelist %>% 
  dplyr::rename(ARM = "decodvalue",
                ARMN = "term") %>% 
  select(name,ARMN,ARM) %>% 
  filter(name=="ARMN")

# Add ARMN numeric values by left join  
dm_vars_plus_ArmN <- dm_TRTSDT %>%
  derive_vars_merged_lookup(
    dataset_add = codelistArmN,
    new_vars = vars(ARMN),
    by_vars = vars(ARM),  
    print_not_mapped = TRUE,
    check_type = "warning"
  ) 


# 238 VISIT1DT (SV.SVSTDTC when SV.VISITNUM=1, converted to SAS date)

sv_visit_1 <- sv %>% 
  select(USUBJID,VISITNUM,SVSTDTC) %>% 
  filter(VISITNUM==1) %>% 
  derive_vars_dt( # Create a date variable named RFENDT from character variable SVSTDTC
    new_vars_prefix = "VISIT1",
    dtc = SVSTDTC
  )

# Left join
dm_vars_plus_visit1dt <- dm_vars_plus_ArmN %>% 
  derive_vars_merged(
    dataset_add = sv_visit_1,
    new_vars = vars(VISIT1DT),
    by_vars = vars(USUBJID)  
  ) 


# 240 WEIGHTBL (VSSTRESN when VS.VSTESTCD='WEIGHT' and VS.VISITNUM=3)----
  #Select records properly to be merged
vs_vars <- vs %>% 
  filter(VISITNUM==3 & VSTESTCD=="WEIGHT") %>%   
  mutate(WEIGHTBL=VSSTRESN) %>% 
  select(USUBJID,WEIGHTBL) 
  
    # left join
dm_weightbl <- dm_vars_plus_visit1dt %>% 
  derive_vars_merged(
    dataset_add = vs_vars,
    new_vars = vars(WEIGHTBL),
    by_vars = vars(USUBJID)  
  ) 






sv










usethis::use_git_config(user.name = "nelsonamurciab@gmail.com",
                        user.email = "murciabn")




gitcreds::gitcreds_set()

