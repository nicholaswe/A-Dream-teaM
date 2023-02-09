# Test file
install.packages(c("admiral", "metatools", "logrx"))
install.packages("admiral.test")
install.packages("xportr")
install.packages("metacore")

library(admiral)
library(admiral.test)
library(dplyr)
library(lubridate)
library(stringr)
library(xportr)
library(metatools)
library(logrx)

# Use haven to read sdtm domains / Clean missing values after read xpt file
dm <- haven::read_xpt("sdtm/dm.xpt") %>% convert_blanks_to_na()
ds <- haven::read_xpt("sdtm/ds.xpt") %>% convert_blanks_to_na()
sc <- haven::read_xpt("sdtm/sc.xpt") %>% convert_blanks_to_na()
vs <- haven::read_xpt("sdtm/vs.xpt") %>% convert_blanks_to_na()
qs <- haven::read_xpt("sdtm/qs.xpt") %>% convert_blanks_to_na()
mh <- haven::read_xpt("sdtm/mh.xpt") %>% convert_blanks_to_na()

metacore <- metacore::spec_to_metacore('metadata/specs.xlsx', where_sep_sheet = F, quiet = T)
adsl_spec <- metacore %>%  select_dataset('ADSL')

# adsl 

adsl <- dm %>%
  mutate(AGE=AGE, AGEU=AGEU, ARM=ARM, DTHFL=DTHFL, ETHNIC=ETHNIC,
         RACE=RACE, RFENDTC=RFENDTC, RFSTDTC=RFSTDTC, SEX=SEX,
         SITEID=SITEID, STUDYID=STUDYID, SUBJID=SUBJID, USUBJID=USUBJID,
          TRT01P = ARM, TRT01A = ACTARM)

