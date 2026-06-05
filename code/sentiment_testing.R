library(tidytext)
library(tidyverse)

# ── NVIVO SENTIMENT DATA ────────────────────────────────────------------------
nvivo_sentiment <- tibble(
  name = c(
    "abston_virginia", "bennis_wassiliisa", "caruso_annette",
    "hall_daniel", "hester_carolann_schlais_meganna_hester_elizabeth",
    "hockema_rex", "hurley_alannah", "ilutsik_esther",
    "ingram_june", "layland_lindsay", "luhrs_leilani",
    "matson_judyjo_harris_carla", "moore_apayu", "nelson_joanne",
    "painter_ted", "redfox_bernadette", "robison_jan",
    "schultheis_jack", "shankle_anne", "smith_simuka",
    "timmerman_connie", "tucker_nick",
    "wayner_harmony_wayner_rhonda_bonin_betty", "wilson_mike"
  ),
  very_negative      = c(12,10,4,1,17,26,13,5,5,21,20,27,26,12,16,4,33,10,13,28,7,13,4,45),
  mod_negative       = c(17,23,7,3,24,36,13,10,10,7,27,37,25,12,23,7,26,14,29,43,22,14,11,72),
  mod_positive       = c(17,22,12,41,28,32,21,18,31,26,19,57,41,21,48,14,64,25,53,50,27,25,13,48),
  very_positive      = c(4,15,5,3,19,11,14,6,3,17,5,12,20,12,12,4,22,19,23,18,9,4,7,9)
) %>%
  mutate(
    total     = very_negative + mod_negative + mod_positive + very_positive,
    # weighted score: -2, -1, +1, +2
    nvivo_score = (very_negative * -2 + mod_negative * -1 +
                     mod_positive  *  1 + very_positive * 2) / total,
    # proportion negative vs positive
    nvivo_pct_positive = (mod_positive + very_positive) / total,
    nvivo_pct_negative = (very_negative + mod_negative) / total
  )


# ── 1. AFINN: scalar score per interview ────────────────────------------------
afinn_scores <- tokens_clean %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(name) %>%
  summarise(
    afinn_mean  = mean(value),
    afinn_total = sum(value),
    n_words     = n(),
    .groups = "drop"
  )

# ── 2. BING: positive/negative counts ───────────────────────-------------------
bing_scores <- tokens_clean %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(name, sentiment) %>%
  pivot_wider(names_from  = sentiment,
              values_from = n,
              values_fill = 0) %>%
  mutate(
    bing_total        = positive + negative,
    bing_pct_positive = positive / bing_total,
    bing_pct_negative = negative / bing_total,
    bing_score        = (positive - negative) / bing_total
  )

# ── 3. NRC: 8 emotions per interview-------------------------------------------
nrc_scores <- tokens_clean %>%
  inner_join(get_sentiments("nrc"),
             by = "word", relationship = "many-to-many") %>%
  count(name, sentiment) %>%
  group_by(name) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

# ── 4. COMBINE WITH NVIVO ────────────────────────────────────-----------------
sentiment_comparison <- nvivo_sentiment %>%
  left_join(afinn_scores, by = "name") %>%
  left_join(bing_scores,  by = "name")

# ── 5. CORRELATIONS W NVIVO ──────────────────────────────────────────-----------------
cat("=== AFINN mean vs NVivo weighted score ===\n")
cor.test(sentiment_comparison$afinn_mean,
         sentiment_comparison$nvivo_score,
         use = "complete.obs") %>% print()

cat("\n=== BING % positive vs NVivo % positive ===\n")
cor.test(sentiment_comparison$bing_pct_positive,
         sentiment_comparison$nvivo_pct_positive,
         use = "complete.obs") %>% print()

# ── 6. SCATTER: AFINN vs NVivo ───────────────────────────────-----------------
sentiment_comparison %>%
  mutate(short_name = str_extract(name, "^[^_]+")) %>%
  ggplot(aes(x = nvivo_score, y = afinn_mean,
             label = short_name)) +
  geom_point(size = 3, color = "#2980B9", alpha = 0.8) +
  geom_smooth(method = "lm", color = "#C0392B", se = TRUE) +
  geom_text(size = 2.8, vjust = -0.8) +
  labs(
    title    = "AFINN (tidytext) vs NVivo Sentiment",
    subtitle = "Each point = one interview. r = correlation coefficient.",
    x = "NVivo weighted score",
    y = "AFINN mean word score"
  ) +
  theme_minimal()

# ── 7. SIDE BY SIDE BAR: NVivo A/B/C/D stacked ───────────────-----------------
nvivo_sentiment %>%
  select(name, very_negative, mod_negative,
         mod_positive, very_positive) %>%
  pivot_longer(-name,
               names_to  = "category",
               values_to = "count") %>%
  mutate(
    category  = factor(category,
                       levels = c("very_negative", "mod_negative",
                                  "mod_positive",  "very_positive")),
    short_name = str_extract(name, "^[^_]+")
  ) %>%
  ggplot(aes(x = reorder(short_name, count),
             y = count, fill = category)) +
  geom_col(position = "fill") +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "very_negative" = "#922B21",
      "mod_negative"  = "#E59866",
      "mod_positive"  = "#7FB3D3",
      "very_positive" = "#1A5276"
    ),
    labels = c("Very negative (A)", "Mod. negative (B)",
               "Mod. positive (C)", "Very positive (D)")
  ) +
  labs(
    title    = "NVivo Sentiment Distribution per Interview",
    subtitle = "Proportional — mirrors NVivo Fig. 5",
    x = NULL, y = "Proportion", fill = "Category"
  ) +
  theme_minimal()


# ── 8. AFINN per interview bar ────────────────────────────────----------------
afinn_scores %>%
  mutate(
    short_name = str_extract(name, "^[^_]+"),
    valence    = if_else(afinn_mean >= 0, "Positive", "Negative")
  ) %>%
  ggplot(aes(x = reorder(short_name, afinn_mean),
             y = afinn_mean, fill = valence)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#2980B9",
                               "Negative" = "#C0392B")) +
  labs(
    title    = "AFINN Sentiment per Interview (tidytext)",
    subtitle = "Positive = net positive word score; Negative = net negative",
    x = NULL, y = "Mean AFINN score", fill = NULL
  ) +
  theme_minimal()


