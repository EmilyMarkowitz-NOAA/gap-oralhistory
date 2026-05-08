library(ggplot2)
library(here)

library(ggsidekick)
theme_set(theme_sleek())

manual <- read.csv(here("data/OHP_table2_manualcoding.csv"))

ggplot(manual, aes(
  x = Change.Language, 
  y = Emotional.Level, 
  color = Fishing.Experience, 
  shape = Indigenous.Status)) +
  geom_jitter(size = 2, width = 0.1, height = 0.1)

ggsave(here("output/manual_coding.png"), width = 6, height = 4, units = "in")


