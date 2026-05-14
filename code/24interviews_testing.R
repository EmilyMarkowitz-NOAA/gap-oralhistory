# ── 1. READ ALL PDFs ----------------------------------------------------------
library(pdftools)
library(tidyverse)
library(tidytext)
library(tm)
library(topicmodels)
library(seededlda)
library(quanteda)
library(treemap)


install.packages('quanteda')


pdf_dir <- "~/NOAA_OralHistory/Transcripts/drive-download-20260428T072127Z-3-001"  

# Read every PDF, use filename as the document name
interviews <- tibble(
  file = list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)
) %>%
  mutate(
    name = tools::file_path_sans_ext(basename(file)),
    text = map_chr(file, ~ paste(pdf_text(.x), collapse = " "))
  ) %>%
  select(name, text)

# Check all 24 loaded
nrow(interviews)
print(interviews$name)

interviews <- interviews %>%
  mutate(text = str_replace_all(text, "[\u2019\u2018\u0060']", " "))

# ── 2. TOKENIZE ALL INTERVIEWS ----------------------------------------------------------

custom_stops <- tibble(word = c(
  "yeah", "uh", "um", "gonna", "gotta",
  "kinda", "sorta", "okay", "right", "well",
  "huh", "lot", "started", "stayed",
  "larsen", "gary", "kim", "sparks",
  "lavoie", "interviewer", "laughter", "laughs",  # transcript findings
  "it's", "don't", "that's", "i'm", "you're", 
  "we're", "they're", "there's","kitty", "sopow", "cpw", "ekuk", "hundred",
  "days", "time", "people", "nice", "guess",  "don", "didn", "wasn", "couldn", "wouldn",
  "isn", "aren", "haven", "hasn", "won", "carla", "pretty", "leilani", "luhrs", "jack", "schultheis", "nick",
  "mas", "tucker", "annette", "caruso", "cah",
  "sighs", "inaudible", "wanyer", "rhonda", "harmony", "betty", "bonin", "wilburn"                    
))

tokens <- interviews %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stops, by = "word") %>%
  filter(
    !str_detect(word, "^\\d+$"),
    !str_detect(word, "'"),    
    nchar(word) > 2
  ) 

# ── 3. BUILD DOCUMENT TERM MATRIX ----------------------------------------------------------

word_counts <- tokens %>%
  count(name, word, sort = TRUE)

dtm <- word_counts %>%
  cast_dtm(name, word, n)

#  shows 24 rows x however many unique words (6710)
print(dtm)

# remove words that appear in <3 interviews
dtm <- removeSparseTerms(dtm, 0.88)
print(dtm)
# 18q8 terms, 73% sparsity

# ── 4. FIND THEMES   ----------------------------------------------------------

# LDA looks at the table of word counts across 24 interviews, never the sentences. 
# notes down when concepts are appeared in the same interviews together - (like salmon and subsistence)

# Gibbs sampling: LDA randomly assigns words into topics. Then reassigns based on what topic 
# most words in this itnerview are already assigned to, and what topic most instances of __
# across all interviews are assigned to. Done for each word thousands of times. 

set.seed(42) # makes random results reproducible

lda_model <- LDA(
  dtm, 
  k      = 6, # find exactly 6 themes
  method = "Gibbs", # algorithm, random guesses and slowly improving the over many iterations
  control = list(seed = 42, burnin = 1000, iter = 2000) 
  # burnin = first 1000 iterations are thrown away (too noisy)
  # iter = run 2000 more iterations and use those to build the final result
)

# See top 15 words per topic
topicmodels::terms(lda_model, 15)


library(tidytext)
# ── 1. NRC LEXICON — 8 emotions ─────────────────────────────
# This is the most useful one for oral histories
nrc <- get_sentiments("nrc")

nrc_scores <- tokens %>%
  inner_join(nrc, by = "word") %>%
  count(name, sentiment) %>%
  group_by(name) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

# Overall emotion profile across all interviews
nrc_overall <- nrc_scores %>%
  group_by(sentiment) %>%
  summarise(total = sum(n)) %>%
  mutate(proportion = total / sum(total))

ggplot(nrc_overall, aes(x = reorder(sentiment, proportion), 
                        y = proportion, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Emotional Tone Across All 24 Interviews",
    x = NULL, y = "Proportion of emotional words"
  ) +
  theme_minimal()



# 5. TEST SEEDING THEME 'CHANGE' -----------------------------------------------

custom_stops <- tibble(word = c(
  # filler/spoken word artifacts
  "yeah", "uh", "um", "gonna", "gotta", "kinda", "sorta",
  "okay", "right", "well", "huh", "lot", "pretty", "stuff",
  "guess", "nice", "days", "time", "people", "day",
  "sighs", "inaudible", 
  
  # broken contractions from apostrophe stripping
  "don", "didn", "wasn", "couldn", "wouldn",
  "isn", "aren", "haven", "hasn", "won", "can",
  
  # interviewer/interviewee names
  "larsen", "gary", "kim", "sparks", "lavoie",
  "kitty", "sopow", "carla", "anna", "leilani",
  "luhrs", "jack", "schultheis", "nick", "mas",
  "tucker", "annette", "caruso", "wanyer", "rhonda",
  "harmony", "betty", "bonin", "wilburn", "judyjo", 
  "matson", "harris", "connie",
  
  # transcript formatting artifacts
  "interviewer", "laughter", "laughs", "cah",
  
  # generic verbs with low analytical value
  "started", "stayed", "came", "got", "said",
  "told", "went", "come", "get", "just", "shit", "called", "grew"
))

tokens_clean <- tokens %>%
  anti_join(custom_stops, by = "word")

# Build DFM from the cleaned tokens made earlier
dfm_q <- tokens_clean %>%
  count(name, word) %>%
  cast_dfm(name, word, n)

print(dfm_q)


# run without any seeded themes
set.seed(42)

lda_unseeded <- textmodel_lda(dfm_q, k = 6)

# Top 15 words per topic
terms(lda_unseeded, 15)


# do it with the change theme
seed_dict <- dictionary(list(
  change = c(
    "change", "changing", "different", "used", "anymore",
    "before", "remember", "decline", "less", "gone",
    "warming", "ice", "weather", "season", "shift",
    "future", "worried", "concern", "impact", "loss" 
  ),
  topic2 = character(0),  # free topic = no seeds present
  topic3 = character(0),
  topic4 = character(0),
  topic5 = character(0),
  topic6 = character(0)
))

set.seed(42)

lda_seeded <- textmodel_seededlda(
  dfm_q,
  dictionary = seed_dict,
  residual   = FALSE,   # extra 'catch-all' topic
  verbose    = TRUE
)

terms(lda_seeded, 15)


