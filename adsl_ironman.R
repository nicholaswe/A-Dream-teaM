# ADSL program for Ironman team
# Programmers : Naveen
#               Nelson
# Notes: Naveen from 192 - 271 and Nelson 218 - 240

#install.packages("admiral")
#install.packages("admiral.test")
#install.packages("xportr")
#install.packages("readxl")
#install.packages("haven")


library(admiral)
library(admiral.test)
library(dplyr)
library(lubridate)
library(stringr)
library(xportr)
library(readxl)
library(haven)

# Load source xpt datasets ----

dm <- haven::read_xpt("/cloud/project/sdtm/dm.xpt")
ds <- haven::read_xpt("/cloud/project/sdtm/ds.xpt")
ex <- haven::read_xpt("/cloud/project/sdtm/ex.xpt")


# Reading ADSL specifications from specs sheet

adsl_spec <- readxl::read_xlsx("/cloud/project/metadata/specs.xlsx", sheet = "Variables") %>% 
      rlang::set_names(tolower) %>%
      filter(dataset == "ADSL") %>%
      mutate(format = str_to_lower(format))

# Replacing spece with NA in sas datasets ----

dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)

# User defined functions -----

func_agegr1 <- function(x) {
   case_when(
     x < 65 ~ "1",
     between(x, 65, 80) ~ "2",
     x >80 ~ "3"
   )  
}






