
install.packages("admiral")
install.packages("admiral.test")
install.packages("dplyr")
install.packages("lubridate")
install.packages("stringr")
install.packages("xportr")
install.packages("haven")
install.packages("tidyverse")
install.packages("readxl")


library(admiral)
library(admiral.test) 
library(dplyr)
library(lubridate)
library(stringr)
library(xportr)
library(haven)
library(tidyverse)
library(readxl)

# Load source xpt datasets ----

dm <- haven::read_xpt("/cloud/project/sdtm/dm.xpt")
ds <- haven::read_xpt("/cloud/project/sdtm/ds.xpt")
ex <- haven::read_xpt("/cloud/project/sdtm/ex.xpt")


# Reading ADSL specifications from specs sheet

adsl_spec <- readxl::read_xlsx("/cloud/project/metadata/specs.xlsx", sheet = "Variables") %>% 
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>%
  filter(dataset == "ADSL") %>%
  mutate(format = str_to_lower(format))


# Replacing spece with NA in sas datasets ----

dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
