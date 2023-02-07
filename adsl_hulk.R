# running the pharmaverse end-to-end ADSL example in blocks

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

# Read in input SDTM data 
data("admiral_dm")
data("admiral_ex")

# Read in metacore object 
load(metacore_example("pilot_ADaM.rda"))
metacore <- metacore %>% 
  select_dataset("ADSL")

metacore$ds_vars
