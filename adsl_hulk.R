# running the pharmaverse end-to-end ADSL example in blocks


# install.packages("admiral")

# kyle token
# ghp_EE6POQTZoT70TJ045SXStkr5xayoV13shCGd


# Kyle sign-in process...

#  usethis::use_git_config(user.name = "kyleireton",
#                          user.email = "kyle.ireton@gmail.com")

#  gitcreds::gitcreds_set()

#  Enter token: ghp_ #
#  hXHuKYbIXE1eBQOaItxsA6sibH4Iqo1l2rQy


# call our libraries ------------------------------------------------------


options(repos = c(
  pharmaverse = 'https://pharmaverse.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))



library(metacore)
library(metatools)
library(admiral.test)
library(admiral)
library(xportr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(haven)
library(readxl)

# Read in input SDTM data 
data("admiral_dm")
data("admiral_ex")

dm <- read_xpt("sdtm/dm.xpt")
ds <- read_xpt("sdtm/ds.xpt")
ex <- read_xpt("sdtm/ex.xpt")
vs <- read_xpt("sdtm/vs.xpt")
qs <- read_xpt("sdtm/qs.xpt")
mh <- read_xpt("sdtm/mh.xpt")
sv <- read_xpt("sdtm/sv.xpt")

my_spec <- read_xlsx("metadata/specs.xlsx", sheet = "Codelists")

# auxiliary functions -----------------------------------------------------


format_eoxxstt_nodef <- function(x) {
  case_when(
    x == "COMPLETED" ~ "COMPLETED",
    TRUE ~ "DISCONTINUED"
  )
}

format_agegr1n <- function(x) {
  case_when(
    x < 65 ~ 1,
    x <= 80 ~ 2,
    x > 80 ~ 3
  )
}

format_agegr1 <- function(x) {
  case_when(
    x == 1 ~ "<65",
    x == 2 ~ "65-80",
    x == 3 ~ ">80"
  )
}

format_visnumen <- function(x) {
  case_when(
    x ==13 ~ 12,
    TRUE ~ x
  )
}


format_racen <- function(x) {
  case_when(
    x == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
    x == "ASIAN" ~ 2,
    x == "BLACK OR AFRICAN AMERICAN" ~ 3,
    x == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 5,
    x == "WHITE" ~ 6,
    TRUE ~ 99
  )
}


format_armn <- function(x) {
  case_when(
    x=="Placebo" ~ 0,
    x=="Xanomeline Low Dose" ~54,
    x=="Xanomeline High Dose" ~81,
    TRUE ~ 99
  )
}

# Read in input SDTM data 
data("admiral_dm")
data("admiral_ex")
dm <- read_xpt("sdtm/dm.xpt")
ds <- read_xpt("sdtm/ds.xpt")
ex <- read_xpt("sdtm/ex.xpt")
vs <- read_xpt("sdtm/vs.xpt")
qs <- read_xpt("sdtm/qs.xpt")
mh <- read_xpt("sdtm/mh.xpt")
sv <- read_xpt("sdtm/sv.xpt")
my_spec <- read_xlsx("metadata/specs.xlsx", sheet = "Codelists")


# Read in metacore object 
load(metacore_example("pilot_ADaM.rda"))
metacore <- metacore %>% 
  select_dataset("ADSL")

metacore$ds_vars



# auxiliar functions



# kyle playtime -----------------------------------------------------------

# ==============================

format_armn <- function(x) {
  case_when(
    x=="Placebo" ~ 0,
    x=="Xanomeline Low Dose" ~ 1,
    x=="Xanomeline High Dose" ~ 2,
    TRUE ~ 99
  )}
  
format_trt01pn <- function(x) {
  case_when(
    x =="Placebo" ~ 0,
    x =="Xanomeline Low Dose" ~ 54,
    x =="Xanomeline High Dose" ~ 81,
    TRUE ~ 99
  )}

# calculate treatment duration --------------------------------------------

# get start date

sv_3 = sv %>% 
  
  filter(VISITNUM == 3)

# get end date

ex_last = ex %>% 
  
  group_by(USUBJID) %>% 
  
  arrange(as.Date(EXENDTC)) %>%
  
  slice(n())

# check if last date worked

max((ex_last %>% count(USUBJID))$n)

max(is.na((ex_last %>% count(USUBJID))$n))

# try to merge the df 's

# dur_df = merge(x = (sv_3 %>% select(USUBJID, SVSTDTC)),
#                
#                y = (ex_last %>% select(USUBJID, EXENDTC)),
#                
#                by.x = sv_3$USUBJID)
          

# let's hack it instead

sv_3_id = sv_3 %>% 
  
  arrange(USUBJID) %>% 
  
  select(USUBJID,
         
         SVSTDTC)

ex_last_id = ex_last %>% 
  
  arrange(USUBJID) %>% 
  
  select(USUBJID,
         
         EXENDTC)

# merge and get trtdur per ID:

dur_df = merge(sv_3_id,
               
               ex_last_id) %>% 
  
  mutate(
    
    TRTDUR = as.Date(EXENDTC) - as.Date(SVSTDTC) + 1
    
    )


# build adsl from scratch -------------------------------------------------


adsl_dm <- dm %>%
  
  select(AGE,
         AGEU,
         ARM,
         DTHFL,
         ETHNIC,
         RACE,
         RFENDTC,
         RFSTDTC,
         SEX,
         SITEID,
         STUDYID,
         SUBJID,
         USUBJID) %>%
  
  mutate(ARMN = format_armn(ARM),
         
         TRT01PN = format_trt01pn(ARM))

# combine adsl with treatment duration

adsl_dur = merge(adsl_dm,
                 
                 dur_df)

# output didn't give same number of IDs as adsl, which don't match?

difference = setdiff(adsl_dm$USUBJID,
                     
                     dur_df$USUBJID)


adsl_match = adsl_dm %>% 
  
  filter(ARM != "Screen Failure")


# follow up with Martha, Monday -------------------------------------------
# merge the 254 that match, then tack back on the remainder

adsl_dur = merge(adsl_match,
                 
                 dur_df)

# the remainder are all screen failures
# let's just add empty variables for them, and then rbind


adsl_SF = adsl_dm %>% 
  
  filter(ARM == "Screen Failure") %>% 
  
  mutate(SVSTDTC = NA,
         EXENDTC = NA,
         TRTDUR = NA
         )


# rbind it all together

adsl_trtdur = rbind(adsl_dur,
                    
                    adsl_SF) 


# pick back up on monday: add more variables in order? ------------------

adsl_order = adsl_trtdur %>% 
  
  select(STUDYID,
         USUBJID,
         SUBJID,
         SITEID,
         # SITEGR1,
         ARM,
         # TRT01P,
         TRT01PN,
         # TRT01A,
         # TRT01AN,
         TRTSDT = SVSTDTC,
         TRTEDT = EXENDTC,
         TRTDURD = TRTDUR,
         # AVGDD
         # CUMDOSE
         AGE,
         # AGEGR1
         # AGEGR1N
         AGEU,
         RACE,
         # RACEN
         SEX,
         ETHNIC,
         #...
         DTHFL,
         #...
         RFSTDTC,
         RFENDTC
         )

# SITEGR1
# Sites are pooled if there are fewer than 3 patients per site

# check num patients per site

site_pool_n = adsl_order %>% 
  
  group_by(SITEID) %>% 
  
  count() %>% 
  
  arrange(n) %>% 
  
  mutate(
    
    SITEGR1 = case_when (
      
      n < 3 ~ as.character(paste(900)),
      
      TRUE ~ SITEID
      
      )) %>% 
  
  ungroup()


# we need to add the next lowest site to the pool to increase n to >= 3
# but we will not deal with programming it right now
# let's combine SITEGR1 with the pooled group code

adsl_all = merge(adsl_order,
                 
                 site_pool_n)

# let's try the same thing but easier with admiral? 

adsl_alt = adsl_order %>% 
  
  derive_vars_merged(
    
    dataset_add = site_pool_n,
    new_vars = vars(SITEGR1 = SITEGR1),
    by_vars = vars(SITEID)
    
    )



# TRT01P
# apparently this is just DM.ARM ? 

adsl_all = adsl_all %>% 
  
  mutate(TRT01P = ARM,
         
         .after = ARM)

# TRT01A
# this is the actual treatment, which is the same as planned in this study

adsl_all = adsl_all %>% 
  
  mutate(TRT01A = TRT01P,
         
         .after = TRT01PN)

# TRT01AN
# this is the actual treatment, which is the same as planned in this study
# codelist for ARMN specifies this means "0, 54, 81"

adsl_all = adsl_all %>% 
  
  mutate(TRT01AN = TRT01PN,
         
         .after = TRT01A)


# AVGDD

# need CUMDOSE, and for that we need ARMN

# for dose 81, we need something tricky:

# 54 for int 1, then 81 for int 2, then 54 for int 3


# AGEGR1
# AGEGR1 = 1 if AGE <65. AGEGR1 = 2 if AGE 65-80. AGEGR1 = 3 if AGE >80

adsl_all = adsl_all %>% 
  
  mutate(AGEGR1 = case_when(
    
    AGE < 65 ~ "AGE <65",
    AGE >= 65 & AGE <= 80 ~ "AGE 65-80",
    AGE > 80 ~ "AGE >80"
    
  ),
  
  .after = AGE)


# AGEGR1N
# AGEGR1 = 1 if AGE <65. AGEGR1 = 2 if AGE 65-80. AGEGR1 = 3 if AGE >80

adsl_all = adsl_all %>% 
  
  mutate(AGEGR1N = case_when(
    
    AGEGR1 == "AGE <65" ~ 1,
    AGEGR1 == "AGE 65-80" ~ 2,
    AGEGR1 == "AGE >80" ~ 3
    
  ),
  
  .after = AGEGR1)


# RACEN 

adsl_all = adsl_all %>% 
  
  mutate(RACEN = format_racen(RACE),

        .after = RACE )





















# more auxiliary ----------------------------------------------------------



##auxiliar DS

#auxiliar dates

#MH

mh_disons <- mh %>%
  
  filter(MHCAT=="PRIMARY DIAGNOSIS") %>%
  
  derive_vars_dt(
    dtc = MHSTDTC,
    new_vars_prefix = "DISONS"
  )

#SV

sv_trtsdt <- sv %>%
  
  filter(VISITNUM==3) %>%
  
  derive_vars_dt(
    dtc = SVSTDTC,
    new_vars_prefix = "TRTS"
  )


#qsorres auxiliar (char to int)

qsorres <- qs %>%
  
  filter(QSCAT == "MINI-MENTAL STATE") %>%
  
  mutate_all(type.convert, as.is=TRUE)


adsl <- dm %>%
  
  #Predecesor DM variables (no computation)
  
  select (AGE,
          AGEU,
          ARM,
          DTHFL,
          ETHNIC,
          RACE,
          RFENDTC,
          RFSTDTC,
          SEX,
          SITEID,
          STUDYID,
          SUBJID,
          ARMCD,
          USUBJID)

adsla <- adsl %>%
  
  derive_vars_merged(
    dataset_add = ds,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(VISNUM = VISITNUM),
    filter_add = DSTERM =="PROTCOL COMPLETED"
  ) %>%
  
  derive_vars_merged(
    dataset_add = ds,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(DCDECOD = DSDECOD),
    filter_add = DSCAT == "DISPOSITION EVENT"
  ) %>%
  
  derive_vars_merged(
    dataset_add = vs,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(WEIGHTBL = VSSTRESN),
    filter_add = (VSTESTCD=="WEIGHT" & VISITNUM==3 )
  ) %>%
  
  derive_vars_merged(
    dataset_add = vs,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(HEIGHTBL = VSSTRESN),
    filter_add = (VSTESTCD=="HEIGHT" & VISITNUM==1 )
  ) %>%
  
  derive_vars_merged(
    dataset_add = mh_disons,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(DISONSDT )
  ) %>%
  
  derive_vars_merged(
    dataset_add = sv_trtsdt,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(TRTSDT )
  ) %>%
  
  mutate(
    
    EOSSTT = format_eoxxstt_nodef(DCDECOD),
    
    RACEN = format_racen(RACE),
    
    ARMN = format_armn(ARM),
    
    AGEGR1N = format_agegr1n(AGE),
    
    AGEGR1 = format_agegr1(AGEGR1N),
    
    BMIBL = WEIGHTBL / ((HEIGHTBL/100)**2),
    
    VISNUMEN = format_visnumen (VISNUM),
    
    RFENDT = convert_dtc_to_dt(RFENDTC),
    
    TRT01P = ARM,
    
    TRT01A = TRT01P,
    
    TRT01PN = ARMN,
    
    TRT01AN = TRT01PN,
    
    ITTFL = if_else(ARMCD!="", "Y", "N"),
    
    SAFFL = if_else(ITTFL=="Y" & !is.na(TRTSDT) , "Y", "N")
    
  )  %>%
  
  derive_var_merged_summary(
    dataset_add = qsorres,
    by_vars = vars(STUDYID, USUBJID),
    filter_add = (QSCAT == "MINI-MENTAL STATE" & !is.na(QSORRES) ),
    new_var = MMSETOT,
    analysis_var = QSORRES,
    summary_fun =function(x) sum(x, na.rm = TRUE)
  )



# ==============================

format_armn <- function(x) {
  case_when(
    x=="Placebo" ~ 0,
    x=="Xanomeline Low Dose" ~1,
    x=="Xanomeline High Dose" ~2,
    TRUE ~ 99
  )
}

format_trt01pn <- function(x) {
  case_when(
    x=="Placebo" ~ 0,
    x=="Xanomeline Low Dose" ~54,
    x=="Xanomeline High Dose" ~81,
    TRUE ~ 99
  )
}

adsl<-dm %>% select(AGE,
                    AGEU,
                    ARM,
                    DTHFL,
                    ETHNIC,
                    RACE,
                    RFENDTC,
                    RFSTDTC,
                    SEX,
                    SITEID,
                    STUDYID,
                    SUBJID,
                    USUBJID) %>%
  mutate(ARMN=format_armn(ARM),
         TRT01PN=format_trt01pn(ARM),
         TRTDUR=TRTEDT-TRTSDT+1) # %>%
  
  
  
  






