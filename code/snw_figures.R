library(here)
library(purrr)
library(readxl)

# Read in data -----------------------------------------------------------------
# Read in Em's compiled data
# dat <- read.csv(here("data", "oralhistory_ref.csv"))  # Error in make.names(col.names, unique = TRUE) : invalid multibyte string 15
dat_edit <- read.csv(here("data", "oralhistory_ref_edited.csv"))

# Read in individual .xslx files as a list of dataframes
file_list <- list.files(path = here("data", "original", "NVivo"),
                        full.names = TRUE,
                        recursive = TRUE)

dat_list <- file_list %>%
  # set df names to file names, minus extension
  set_names(gsub("\\.xlsx$", "", basename(.))) %>%  
  map(read_xlsx)

