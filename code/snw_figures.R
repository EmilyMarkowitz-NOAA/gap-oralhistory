library(here)
library(tidyverse)
library(readxl)
library(viridis)

# Set ggplot theme
if (!requireNamespace("ggsidekick", quietly = TRUE)) {
  devtools::install_github("seananderson/ggsidekick")
}
library(ggsidekick)
theme_set(theme_sleek())

# Read in data ----------------------------------------------------------------
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


df1 <- dat_list$code_references_interviews

# Figure 4: co-occurrence of coded themes in interview transcripts ------------
fig4 <- dat_list$co_occurrence_interviews
colnames(fig4) <- c("var1", "boat", "crab", "fishing", "net", "salmon")
fig4$var1 <- c("boat", "crab", "fishing", "net", "salmon")
fig4 <- fig4 %>%
  pivot_longer(-var1, names_to = "var2", values_to = "occurrence") %>%
  mutate(occurrence = as.integer(occurrence)) %>%
  ggplot(., aes(x = var1, y = var2, fill = occurrence)) +
  geom_tile() +
  scale_fill_viridis() 
fig4

# Figure 5: sentiment analysis ------------------------------------------------
fig5 <- dat_list$sentiment_raw
colnames(fig5) <- c("interview_name", 
                    "very negative", 
                    "moderately negative", 
                    "moderately positive", 
                    "very positive")
fig5$interview <- 1:nrow(fig5)  
fig5 <- fig5 %>%
  select(-interview_name) %>%
  pivot_longer(-interview, names_to = "language", values_to = "frequency") %>%
  mutate(language = factor(language, levels = c("very negative", 
                                                "moderately negative",
                                                "moderately positive", 
                                                "very positive"))) %>%
  ggplot(., aes(x = language, y = interview, fill = frequency)) +
  geom_tile() +
  scale_fill_viridis(option = "mako", direction = -1)
fig5
