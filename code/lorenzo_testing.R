# 0. INSTALL & LOAD PACKAGES ---------------------------------------------------

packages <- c(
  "pdftools",      # for PDF text extraction
  "tidytext",      # for the tidy NLP framework
  "tidyverse",     # Data wrangling + ggplot2
  "topicmodels",   # LDA topic modeling
  "seededlda",     # Guided/seeded topic modeling
  "quanteda",      # Corpus management + text stats
  "quanteda.textstats",
  "stm",           # Structural topic modeling (uses metadata)
  "SnowballC",     # Word stemming
  "wordcloud",     # word cloud visualizations
  "RColorBrewer"
)

lapply(packages, library, character.only = TRUE)

# 1. PDF -> DATA ---------------------------------------------------------------

raw_text <- pdf_text("~/NOAA_OralHistory/Transcripts/drive-download-20260428T072127Z-3-001/abston_virginia.pdf")

# each element is one page, collapse into one string
full_text <- paste(raw_text, collapse = " ")

cat(substr(full_text, 1, 500))
#looks good


# 2. PUT INTO DF ---------------------------------------------------------------

interview <- data.frame(
  name = "abston_virginia",
  text = full_text,
  stringsAsFactors = FALSE
)

# 3. TOKENIZE IT ---------------------------------------------------------------

custom_stops <- tibble(word = c(
  "yeah", "uh", "um", "gonna", "gotta",
  "kinda", "sorta", "okay", "right", "huh", "well"
))


tokens <- interview %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stops, by = "word") %>%
  filter(
    !str_detect(word, "^\\d+$"),  # remove numbers
    nchar(word) > 2               # remove very short words
  ) 

# 4. RESULTS ---------------------------------------------------------------

tokens %>% 
  count(word, sort = TRUE ) %>% 
  slice_head(n = 20) %>% 
  print()



