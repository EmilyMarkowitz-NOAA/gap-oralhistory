
# Install Libraries ------------------------------------------------------------

# Here we list all the packages we will need for this whole process
PKG <- c(
  
  # Graphics
  "ggplot2", # Create Elegant Data Visualizations Using the Grammar of Graphics
  "cowplot",
  "png",
  "magick",
  "scales", # nicer lables in ggplot2
  # "flextable", # making pretty tables
  
  # other tidyverse
  "plyr",
  "dplyr",
  "googledrive",
  "readr",
  "tidyr",
  "readxl", 
  "here",
  "viridis",
  "janitor",
  
  # Text Management
  "stringr",
  
  # Spatial mapping
  "sf",
  "ggspatial", 
  "raster",
  "stars"
)

PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p, character.only = TRUE)) {
    install.packages(p)
  }
  require(p, character.only = TRUE)
}


# Load data --------------------------------------------------------------------

#XLSX
a <- list.files(path = here::here("data"), 
                full.names = TRUE, recursive = FALSE, pattern = ".xlsx")

for (i in 1:length(a)){
  b <- readxl::read_xlsx(path = a[i], sheet = 1)
  b <- janitor::clean_names(b)
  temp <- strsplit(x = a[i], split = "/")
  temp <- gsub(pattern = "\\.xlsx", replacement = "", x = temp[[1]][length(temp[[1]])])
  temp <- tolower(temp)
  assign(x = paste0(temp, "0"), value = b)
  print(paste0("Loaded: ", temp, "0"))
}

# CSVs
a <- list.files(path = here::here("data"), 
                full.names = TRUE, recursive = FALSE, pattern = ".csv")

for (i in 1:length(a)){
  b <- readr::read_csv(file = a[i], show_col_types = FALSE)
  b <- janitor::clean_names(b)
  temp <- strsplit(x = a[i], split = "/")
  temp <- gsub(pattern = "\\.csv", replacement = "", x = temp[[1]][length(temp[[1]])])
  temp <- tolower(temp)
  assign(x = paste0(temp, "0"), value = b)
  print(paste0("Loaded: ", temp, "0"))
}

# Wrangle Data -----------------------------------------------------------------

# oralhistory_ref <- oral_histories_original0 |> 
#   dplyr::select(source, date, link, id, word_count, weighted_percent, percent_category) |> 
#   dplyr::distinct() 
# write_csv(x = oralhistory_ref, file = "data/oralhistory_ref.csv", col_names = TRUE)

oralhistory_ref <- oralhistory_ref_edited0 |> 
  dplyr::mutate(indigenous00 = ifelse(indigenous == "Non-indigenous", "Non-indigenous", "Indigenous"))

# Aesthetics -------------------------------------------------------------------

full_page_portrait_width <- 6.5
full_page_portrait_height <- 7.5
full_page_landscape_width <- 9.5
# https://www.visualisingdata.com/2019/08/five-ways-to-design-for-red-green-colour-blindness/
negative <- "#EDA247"
positive <- "#57C4AD"
neutral <- "#E6E1BC"

# Functions --------------------------------------------------------------------


theme_custom <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
    panel.background = element_rect(fill = "white", colour = NA), 
    panel.border = element_rect(fill = NA, colour = "grey20"), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12), 
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8, face = "bold"),
    legend.background = element_rect(colour = "transparent", 
                                     fill = "transparent"),
    legend.key = element_rect(colour = "transparent",
                              fill = "transparent"),
    legend.position = "bottom",
    legend.box = "horizontal", 
    legend.box.spacing = unit(0, "pt")
  ) 
}


save_figures<-function(figure,
                       header = "",
                       footnotes = "",
                       filename0 = NULL,
                       path = "./output/",
                       width = 6,
                       height = 6,
                       output_type = c("pdf", "png"),
                       type = "Figure",
                       alttext = "",
                       filename_desc = "",
                       nickname = "",
                       table_raw = NULL, 
                       bg = "transparent"
){
  
  filename0 <- ifelse(is.null("filename0"), filename0, nickname)
  
  header<-trimws(header)
  header<-paste0(ifelse(substr(x = header,
                               start = nchar(header),
                               stop = nchar(header)) %in%
                          c(".", "!", "?", "...", "...."),
                        header, paste0(header, ".")))
  footnotes<-trimws(footnotes)
  caption<-ifelse(sum(footnotes %in% "") != 0,
                  header,
                  paste0(header, paste(paste0("^[", footnotes, "]"),
                                       collapse = " ^,^ ")))
  
  # Save
  if (!is.null(path)){
    
    # Save Graphic/Figure
    for (i in 1:length(output_type)){
      ggplot2::ggsave( # save your plot
        path = path,
        dpi = 1200,
        bg = bg,
        filename = paste0(nickname, ".", output_type[i]), # Always save in pdf so you can make last minute edits in adobe acrobat!
        plot = figure, # call the plot you are saving
        width = width, height = height, units = "in") #recall, A4 pages are 8.5 x 11 in - 1 in margins
      
    }
    
    # table_raw
    
    # Save table_raw file (no rounding, no dividing)
    if (!(is.null(table_raw)) &
        (is.data.frame(table_raw) | is.matrix(table_raw))) {
      # for (i in 1:length(output_type)){
      utils::write.table(x = table_raw,
                         file = paste0(path, nickname,
                                       ".csv"),
                         sep = ",",
                         row.names=FALSE, col.names = TRUE, append = F)
      # }
    } else {
      table_raw <- ""
    }
    
  }
  
  write.table(x = caption, 
              file = paste0(path, nickname, ".txt"), 
              row.names = FALSE, 
              col.names = FALSE, 
              quote = FALSE)
  
  # Save Graphic/Figure as .rdata
  obj <- list("figure" = figure,
              "table_raw" = table_raw,
              "caption" = caption,
              "header" = header,
              "nickname" = nickname,
              "alttext" = alttext,
              "footnotes" = footnotes,
              "filename" = nickname)
  
  save(obj, 
       file = paste0(path, nickname, ".rdata"))
  
}

# Prep figures -----------------------------------------------------------------

## Figure 1: Hierarchical plot of themes -----------------------------------------------
#NOT DONE
nickname0 <- "fig-1-hierarchical-themes-auto-"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw <- code_references_interviews0[,1:6] 
names(table_raw)[2:ncol(table_raw)] <- lapply(X = strsplit(x = names(table_raw)[2:ncol(table_raw)], split = "_"), '[[', 2)

table_raw0 <- table_raw <- hier_interviews0

colors0 <- viridis::mako(length(unique(table_raw$cat)), direction = -1, begin = 0.2, end = .8)

### Grouped bar plot by narrator name with grouping rectangle ------------------
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = name, y = freq, fill = cat)) +
  ggplot2::geom_bar(position="dodge", stat="identity") +
  ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Frequency of Code References") +
  ggplot2::scale_x_discrete(name = "Interviewee") +
  theme_custom() + 
  ggplot2::theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1) 
  ) 
nickname <- paste0(nickname0, "_group_name")
save_figures(figure = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

nickname0 <- "fig-1-bar-themes-auto-"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw <- code_references_interviews0[,1:6] 
names(table_raw)[2:ncol(table_raw)] <- lapply(X = strsplit(x = names(table_raw)[2:ncol(table_raw)], split = "_"), '[[', 2)

table_raw0 <- table_raw <- table_raw|> 
  dplyr::mutate(
    id = as.numeric(trimws(substr(x = x1, start = 1, stop = 2))) # , 
    # name = substr(x = x1, start = 12, stop = nchar(x1)), 
    # name = gsub(pattern = "\\\\", replacement = "", x = name), 
    # name = gsub(pattern = "_", replacement = " ", x = name), 
    # name = gsub(pattern = "-", replacement = " ", x = name), 
    # name = gsub(pattern = "[0-9]+", replacement = "", x = name), 
    # name = stringr::str_to_title(name)
  ) |> 
  dplyr::left_join(oralhistory_ref |> 
                     dplyr::select(id, name = source, indigenous, fishing_experience, collection, demographic, indigenous00)) |>
  dplyr::select(-x1) |> 
  tidyr::pivot_longer(cols = boat:salmon, names_to = "cat", values_to = "freq") |> 
  dplyr::mutate(cat = stringr::str_to_sentence(cat))

table_raw_rect <- table_raw |> 
  dplyr::select(id) |>
  dplyr::distinct() |> 
  dplyr::mutate(
    xmin = id - 0.45, # approx start of group
    xmax = id + 0.45,  # approx end of group
    ymin = 0, 
    ymax = max(table_raw$freq)
  )

colors0 <- viridis::mako(length(unique(table_raw$cat)), direction = -1, begin = 0.2, end = .8)

### Grouped bar plot by narrator name with grouping rectangle ------------------
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = name, y = freq, fill = cat)) +
  ggplot2::geom_bar(position="dodge", stat="identity") +
  ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Frequency of Code References") +
  ggplot2::scale_x_discrete(name = "Interviewee") +
  theme_custom() + 
  ggplot2::theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1) 
  ) 
nickname <- paste0(nickname0, "_group_name")
save_figures(figure = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)


## Figure 2: bar chart of themes -----------------------------------------------

nickname0 <- "fig-2-bar-themes"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw <- code_references_interviews0[,1:6] 
names(table_raw)[2:ncol(table_raw)] <- lapply(X = strsplit(x = names(table_raw)[2:ncol(table_raw)], split = "_"), '[[', 2)

table_raw0 <- table_raw <- table_raw |> 
  dplyr::mutate(
    id = as.numeric(trimws(substr(x = x1, start = 1, stop = 2))) # , 
    # name = substr(x = x1, start = 12, stop = nchar(x1)), 
    # name = gsub(pattern = "\\\\", replacement = "", x = name), 
    # name = gsub(pattern = "_", replacement = " ", x = name), 
    # name = gsub(pattern = "-", replacement = " ", x = name), 
    # name = gsub(pattern = "[0-9]+", replacement = "", x = name), 
    # name = stringr::str_to_title(name)
    ) |> 
  dplyr::left_join(oralhistory_ref |> 
                     dplyr::select(id, name = source, indigenous, fishing_experience, collection, demographic, indigenous00)) |>
  dplyr::select(-x1) |> 
  tidyr::pivot_longer(cols = boat:salmon, names_to = "cat", values_to = "freq") |> 
  dplyr::mutate(cat = stringr::str_to_sentence(cat))

table_raw_rect <- table_raw |> 
  dplyr::select(id) |>
  dplyr::distinct() |> 
  dplyr::mutate(
    xmin = id - 0.45, # approx start of group
    xmax = id + 0.45,  # approx end of group
    ymin = 0, 
    ymax = max(table_raw$freq)
  )

colors0 <- viridis::mako(length(unique(table_raw$cat)), direction = -1, begin = 0.2, end = .8)

### Grouped bar plot by narrator name with grouping rectangle ------------------
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = name, y = freq, fill = cat)) +
  ggplot2::geom_bar(position="dodge", stat="identity") +
  ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Frequency of Code References") +
  ggplot2::scale_x_discrete(name = "Interviewee") +
  theme_custom() + 
  ggplot2::theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1) 
  ) 
nickname <- paste0(nickname0, "_group_name")
save_figures(figure = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Grouped bar plot by ID with grouping rectangle -----------------------------
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = id, y = freq, fill = cat)) +
  ggplot2::geom_bar(position="dodge", stat="identity") +
  ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Frequency of Code References") +
  ggplot2::scale_x_continuous(name = "Interview ID", 
                              expand = FALSE) +
  theme_custom()  + 
  ggplot2::geom_rect(
    data = table_raw_rect,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "grey", # color of the border
    alpha = 0.1,     # transparency of the fill
    inherit.aes = FALSE # prevents inheriting main plot aesthetics
  ) 
nickname <- paste0(nickname0, "_groupid")
save_figures(figure = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Grouped bar plot by ID with flipped axis and grouping rectangle ------------
figure_print <- figure_print + 
  ggplot2::coord_flip()
nickname <- paste0(nickname0, "_groupid_flipped")
save_figures(figure = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Stacked by ID --------------------------------------------------------------
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = id, y = freq, fill = cat)) +
  ggplot2::geom_bar(position="stack", stat="identity") +
  ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Frequency of Code References") +
  ggplot2::scale_x_continuous(name = "Interview ID", 
                              expand = FALSE) +
  theme_custom() 
nickname <- paste0(nickname0, "_stackedid")
save_figures(figure = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Stacked by fishing experience + LM --------------------------------------

# https://steverxd.github.io/Stat_tests/three-or-more-means.html
# two ways of doing the same thing: anova and lm

# Anova
car::Anova(aov(freq ~ fishing_experience + cat, data = table_raw0))

# Anova Table (Type II tests)
# 
# Response: freq
# Sum Sq  Df F value    Pr(>F)    
# fishing_experience   9.45   2  0.5982    0.5515    
# cat                542.08   4 17.1495 5.109e-11 ***
#   Residuals          892.96 113                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Linear model
lm <- lm(freq ~ fishing_experience + cat, data = table_raw0)
temp <- lm %>% summary() %>% print(digits = 8) # show summary output
temp 
# Call:
#   lm(formula = freq ~ fishing_experience + cat, data = table_raw0)
# 
# Residuals:
#   Min          1Q      Median          3Q         Max 
# -7.30454545 -1.47261905 -0.51428571  1.02878788 14.07045455 
# 
# Coefficients:
#   Estimate   Std. Error  t value   Pr(>|t|)    
# (Intercept)                                 1.472619048  0.699422229  2.10548   0.037465 *  
#   fishing_experienceCommercial & Subsistence  0.540259740  0.607831621  0.88883   0.375982    
# fishing_experienceSubsistence              -0.047619048  0.699422229 -0.06808   0.945840    
# catCrab                                    -0.083333333  0.811496739 -0.10269   0.918390    
# catFishing                                  5.291666667  0.811496739  6.52087 2.0382e-09 ***
#   catNet                                     -0.041666667  0.811496739 -0.05135   0.959141    
# catSalmon                                   0.041666667  0.811496739  0.05135   0.959141    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.811107 on 113 degrees of freedom
# Multiple R-squared:  0.38181893,	Adjusted R-squared:  0.34899516 
# F-statistic:  11.63239 on 6 and 113 DF,  p-value: 4.0924117e-10

# R² = 0.38 → About 38% of the variation in freq is explained by fishing experience and category.
# Adjusted R² = 0.35 → Still solid after penalizing for predictors.
# F-statistic p < 0.001 → The model as a whole is meaningful (mostly because of catFishing).

# IN SUMMARY
# Frequency differed significantly among categories (F₆,₁₁₃ = 11.6, p < 0.001), with the Fishing category reporting frequencies approximately 5 units higher than the reference category. Fishing experience was not a significant predictor of frequency.

# temp0 <- as.data.frame(temp$coefficients)
# rownames(temp0)[rownames(temp0) == "(Intercept)"] <- "fishing_experienceCommerical"
# rownames(temp0)[temp0$`Pr(>|t|)` < 0.05]

table_raw <- table_raw0 |> 
  dplyr::group_by(cat, fishing_experience) |> 
  dplyr::summarise(freq = sum(freq, na.rm = TRUE)) |> 
  dplyr::ungroup() |> 
  dplyr::left_join(
    table_raw0 |> 
      dplyr::select(id, fishing_experience) |> 
      dplyr::distinct() |> 
  dplyr::group_by(fishing_experience) |> 
  dplyr::summarise(n_interviews = n()) |> 
  dplyr::ungroup()) |> 
  dplyr::mutate(freq_rel = freq/n_interviews, 
                fishing_experience0 = paste0(fishing_experience, "\n(", n_interviews, ")"))

# Frequency 
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = fishing_experience0, y = freq, fill = cat)) +
  ggplot2::geom_bar(position="dodge", stat="identity") +
  ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Frequency of Code References") +
  ggplot2::scale_x_discrete(name = "Fishing Experiance", 
                            labels = function(x) str_wrap(x, width = 20), 
                            expand = FALSE) +
  theme_custom() 
nickname <- paste0(nickname0, "_fishexper")
save_figures(figure = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

# Realitive frequency 
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = fishing_experience, y = freq, fill = cat)) +
  ggplot2::geom_bar(position="dodge", stat="identity") +
  ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Relative Frequency of Code References") +
  ggplot2::scale_x_discrete(name = "Fishing Experiance", 
                            labels = function(x) str_wrap(x, width = 20), 
                            expand = FALSE) +
  theme_custom() 
nickname <- paste0(nickname0, "_fishexper_rel")
save_figures(figure = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Stacked by oral history collection + LM --------------------------------------
# https://steverxd.github.io/Stat_tests/three-or-more-means.html
# two ways of doing the same thing: anova and lm

# Anova
car::Anova(aov(freq ~ collection + cat, data = table_raw0))

# Anova Table (Type II tests)
# 
# Response: freq
# Sum Sq  Df F value    Pr(>F)    
# collection  24.61   3  1.0465     0.375    
# cat        542.08   4 17.2911 4.508e-11 ***
#   Residuals  877.81 112                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Linear model
lm <- lm(freq ~ collection + cat, data = table_raw0)
temp <- lm %>% summary() %>% print(digits = 8) # show summary output
temp 
# Call:
#   lm(formula = freq ~ collection + cat, data = table_raw0)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.0500 -1.6750 -0.7583  1.2000 14.3250 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                1.02500    0.88530   1.158   0.2494    
# collectionOregon Residents in Alaska's Historical Fishing  0.57333    0.91434   0.627   0.5319    
# collectionWest Side Stories                                2.53333    1.44569   1.752   0.0825 .  
# collectionWomen in Alaska Fisheries                        0.73333    0.79184   0.926   0.3564    
# catCrab                                                   -0.08333    0.80817  -0.103   0.9181    
# catFishing                                                 5.29167    0.80817   6.548 1.83e-09 ***
# catNet                                                    -0.04167    0.80817  -0.052   0.9590    
# catSalmon                                                  0.04167    0.80817   0.052   0.9590    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.8 on 112 degrees of freedom
# Multiple R-squared:  0.3923,	Adjusted R-squared:  0.3543 
# F-statistic: 10.33 on 7 and 112 DF,  p-value: 6.056e-10
# 
# Residual standard error: 2.811107 on 113 degrees of freedom
# Multiple R-squared:  0.38181893,	Adjusted R-squared:  0.34899516 
# F-statistic:  11.63239 on 6 and 113 DF,  p-value: 4.0924117e-10
# 
# R² = 0.39 → ~39% of variation in freq is explained by collection + category
# 
# Adjusted R² = 0.35 → Very similar to your previous model
# 
# Overall F-test p < 0.001 → The model is meaningful overall (again, driven largely by catFishing)
# 
# IN SUMMARY
# Frequency differed significantly among categories (F₇,₁₁₂ = 10.3, p < 0.001), with the Fishing category exhibiting frequencies approximately 5 units higher than the reference category. Differences among collections were not statistically significant, although West Side Stories showed a marginally higher frequency.

# temp0 <- as.data.frame(temp$coefficients)
# rownames(temp0)[rownames(temp0) == "(Intercept)"] <- "fishing_experienceCommerical"
# rownames(temp0)[temp0$`Pr(>|t|)` < 0.05]


table_raw <- table_raw0 |> 
  dplyr::group_by(cat, collection) |> 
  dplyr::summarise(freq = sum(freq, na.rm = TRUE)) |> 
  dplyr::ungroup() |> 
  dplyr::left_join(
    table_raw0 |> 
      dplyr::select(id, collection) |> 
      dplyr::distinct() |> 
      dplyr::group_by(collection) |> 
      dplyr::summarise(n_interviews = n()) |> 
      dplyr::ungroup()) |> 
  dplyr::mutate(freq_rel = freq/n_interviews, 
                collection0 = paste0(collection, "\n(", n_interviews, ")"))

figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = collection0, y = freq, fill = cat)) +
  ggplot2::geom_bar(position="dodge", stat="identity") +
  ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Frequency of Code References") +
  ggplot2::scale_x_discrete(name = "Fishing Experiance", 
                            labels = function(x) str_wrap(x, width = 20), 
                            expand = FALSE) +
  theme_custom() 
nickname <- paste0(nickname0, "_collection")
save_figures(figure = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

# Realitive frequency 
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = collection, y = freq, fill = cat)) +
  ggplot2::geom_bar(position="dodge", stat="identity") +
  ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Relative Frequency of Code References") +
  ggplot2::scale_x_discrete(name = "Fishing Experiance", 
                            labels = function(x) str_wrap(x, width = 20), 
                            expand = FALSE) +
  theme_custom() 
nickname <- paste0(nickname0, "_collection_rel")
save_figures(figure = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)


### Stacked by Indigenous/Not + LM --------------------------------------
# https://steverxd.github.io/Stat_tests/three-or-more-means.html
# two ways of doing the same thing: anova and lm

# Does frequency differ by Indigenous identity and by category?

# Anova
car::Anova(aov(freq ~ indigenous00 + cat, data = table_raw0))

# Anova Table (Type II tests)
# 
# Response: freq
# Sum Sq  Df F value    Pr(>F)    
# indigenous00  14.70   1  1.8878    0.1721    
# cat         542.08   4 17.4035 3.591e-11 ***
#   Residuals   887.72 114                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Linear model
lm <- lm(freq ~ indigenous00 + cat, data = table_raw0)
temp <- lm %>% summary() %>% print(digits = 8) # show summary output
temp 
# Call:
#   lm(formula = freq ~ indigenous00 + cat, data = table_raw0)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.6500 -1.4000 -0.6500  0.9104 14.7250 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                1.35833    0.62398   2.177   0.0316 *  
#   indigenous00Non-indigenous  0.70000    0.50948   1.374   0.1721    
# catCrab                   -0.08333    0.80555  -0.103   0.9178    
# catFishing                 5.29167    0.80555   6.569 1.57e-09 ***
#   catNet                    -0.04167    0.80555  -0.052   0.9588    
# catSalmon                  0.04167    0.80555   0.052   0.9588    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.791 on 114 degrees of freedom
# Multiple R-squared:  0.3855,	Adjusted R-squared:  0.3585 
# F-statistic:  14.3 on 5 and 114 DF,  p-value: 7.392e-11
# 
# R² = 0.39 → About 39% of variation in frequency is explained by Indigenous identity and category
# Adjusted R² = 0.36
# Overall F-test p < 0.001
# 
# IN SUMMARY
# Frequency differed significantly among categories (F₅,₁₁₄ = 14.3, p < 0.001), with the Fishing category exhibiting frequencies approximately 5 units higher than the reference category. No significant differences in frequency were observed between Indigenous and non-Indigenous respondents.

# temp0 <- as.data.frame(temp$coefficients)
# rownames(temp0)[rownames(temp0) == "(Intercept)"] <- "fishing_experienceCommerical"
# rownames(temp0)[temp0$`Pr(>|t|)` < 0.05]


table_raw <- table_raw0 |> 
  dplyr::group_by(cat, indigenous00) |> 
  dplyr::summarise(freq = sum(freq, na.rm = TRUE)) |> 
  dplyr::ungroup() |> 
  dplyr::left_join(
    table_raw0 |> 
      dplyr::select(id, indigenous00) |> 
      dplyr::distinct() |> 
      dplyr::group_by(indigenous00) |> 
      dplyr::summarise(n_interviews = n()) |> 
      dplyr::ungroup()) |> 
  dplyr::mutate(freq_rel = freq/n_interviews, 
                indigenous0 = paste0(indigenous00, "\n(", n_interviews, ")"))

# freq
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = indigenous0, y = freq, fill = cat)) +
  ggplot2::geom_bar(position="dodge", stat="identity") +
  ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Frequency of Code References") +
  ggplot2::scale_x_discrete(name = "Demographic", 
                            labels = function(x) str_wrap(x, width = 20), 
                            expand = FALSE) +
  theme_custom() 
nickname <- paste0(nickname0, "_indigenous00")
save_figures(figure = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

# Realitive frequency 
# figure_print <- 
#   ggplot2::ggplot(
#     data = table_raw, 
#     mapping = aes(x = indigenous00, y = freq, fill = cat)) +
#   ggplot2::geom_bar(position="dodge", stat="identity") +
#   ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
#                              values = colors0) +
#   ggplot2::scale_y_continuous(name = "Relative Frequency of Code References") +
#   ggplot2::scale_x_discrete(name = "Demographic", 
#                             labels = function(x) str_wrap(x, width = 20), 
#                             expand = FALSE) +
#   theme_custom() 
# nickname <- paste0(nickname0, "_indigenous00_rel")
# save_figures(figure = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

## Figure 3: Hierarchical plot of themes -----------------------------------------------
#NOT DONE

nickname0 <- "fig-3-hierarchical-themes-manualcode-"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw <- code_references_interviews0[,1:6] 
names(table_raw)[2:ncol(table_raw)] <- lapply(X = strsplit(x = names(table_raw)[2:ncol(table_raw)], split = "_"), '[[', 2)

table_raw0 <- table_raw <- hier_interviews0

colors0 <- viridis::mako(length(unique(table_raw$cat)), direction = -1, begin = 0.2, end = .8)

### Grouped bar plot by narrator name with grouping rectangle ------------------
figure_print <- ""
  

## Figure 4: bar chart of themes -----------------------------------------------
#NOT DONE
nickname0 <- "fig-4-bar-themes-manualcode-"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw <- co_occurrence_interviews

colors0 <- viridis::mako(length(unique(table_raw$cat)), direction = -1, begin = 0.2, end = .8)

### Grouped bar plot by narrator name with grouping rectangle ------------------
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = name, y = freq, fill = cat)) +
  ggplot2::geom_bar(position="dodge", stat="identity") +
  ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Frequency of Code References") +
  ggplot2::scale_x_discrete(name = "Interviewee") +
  theme_custom() + 
  ggplot2::theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1) 
  ) 
nickname <- paste0(nickname0, "_group_name")
save_figures(figure = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

## Figure 5: bar chart of themes -----------------------------------------------
#NOT DONE

nickname0 <- "fig-5-sentiment-heatmap-"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw <- sentiment_raw0
names(table_raw)[2:ncol(table_raw)] <- substr(x = names(table_raw)[2:ncol(table_raw)],start = 3, stop = nchar(names(table_raw)[2:ncol(table_raw)]))

table_raw <- table_raw0 <- table_raw |> 
  dplyr::mutate(
    id = as.numeric(trimws(substr(x = x1, start = 1, stop = 2))) ) |> 
  dplyr::left_join(oralhistory_ref |> 
                     dplyr::select(id, name = source, indigenous, fishing_experience, collection, demographic, indigenous00)) |>
  dplyr::select(-x1) |> 
  tidyr::pivot_longer(cols = very_negative:very_positive, names_to = "cat", values_to = "freq") |> 
  dplyr::mutate(cat = stringr::str_to_sentence(cat), 
                cat = gsub(pattern = "_", replacement = " ", x = cat)) 

colors0 <- viridis::mako(length(unique(table_raw$cat)), direction = -1, begin = 0.2, end = .8)

figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = cat, y = id, fill = cat)) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Frequency of Code References") +
  ggplot2::scale_x_discrete(name = "Interviewee") +
  theme_custom() + 
  ggplot2::theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1) 
  ) 
nickname <- paste0(nickname0, "_group_name")
save_figures(figure = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)



