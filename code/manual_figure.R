library(dplyr)
library(ggplot2)
library(here)

library(ggsidekick)
theme_set(theme_sleek())

manual <- read.csv(here("data/OHP_table2_manualcoding.csv"))

ggplot(manual, aes(x = Change.Language, y = Emotional.Level)) +
  geom_jitter(size = 4)


