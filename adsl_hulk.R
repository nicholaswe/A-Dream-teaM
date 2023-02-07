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

# trying to read by haven

# Use haven to read sdtm domains
dm1 <- haven::read_xpt("sdtm/dm.xpt")

# Clean missing values after read xpt file
dm <- convert_blanks_to_na(dm1)

View(dm)
