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

adsl.aux <- dm %>%
            mutate(AGE=AGE, AGEU=AGEU, ARM=ARM, DTHFL=DTHFL, ETHNIC=ETHNIC,
                  RACE=RACE, RFENDTC=RFENDTC, RFSTDTC=RFSTDTC, SEX=SEX,
                  SITEID=SITEID, STUDYID=STUDYID, SUBJID=SUBJID, USUBJID=USUBJID,
                  TRT01P = ARM, TRT01A = ACTARM) %>% 
            mutate(AGEGR1N = case_when(
              AGE < 65 ~  1,
              65 <= AGE & AGE < 80 ~ 2,
              TRUE ~  3
            )) %>% 
            mutate(AGEGR1 = AGEGR1N %>% as.character()) %>% 
            mutate(ITTFL = case_when(
              ARMCD != "" ~ "Y",
              TRUE ~ "N"
            )) %>% 
            mutate(RACEN = case_when(
              RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
              RACE == "ASIAN" ~ 2,
              RACE == "BLACK OR AFRICAN AMERICAN" ~ 3,
              RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 5,
              RACE == "WHITE" ~ 6,
              TRUE ~ NA
            )) %>% 
            
  
  
### DRAFT ###

derive_vars_dt(adsl.aux, "RFEN", RFENDTC)

## Difference of dates ##

example <- tribble(
  ~Dates,
  "2013-03-18",
  "2013-03-25",
  "2013-02-28")


example2 <- tribble(
  ~ Dates,
  "2013-05-18",
  "2013-03-29",
  "2014-02-28")

example$Dates <- example$Dates %>% ymd()

example2$Dates <- example2$Dates %>% ymd()

example-example2

