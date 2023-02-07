# ADSL program for Ironman team
# Programmers : Naveen
#               Nelson
# Notes: Naveen from 192 - 271 and Nelson 218 - 240

#install.packages("admiral")
#install.packages("admiral.test")
#install.packages("xportr")
#install.packages("readxl")


library(admiral)
library(admiral.test)
library(dplyr)
library(lubridate)
library(stringr)
library(xportr)
library(readxl)

dm <- admiral_dm
ds <- admiral_ds
ex <- admiral_ex


# Reading ADSL specifications from specs sheet

adsl_spec <- readxl::read_xlsx("/cloud/project/metadata/specs.xlsx", sheet = "Variables") %>% 
      rlang::set_names(tolower) %>%
      filter(dataset == "ADSL") %>%
      mutate(format = str_to_lower(format))

# Replacing spece with NA in sas datasets

dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)


