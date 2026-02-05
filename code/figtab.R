
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


save_figures<-function(figure_print,
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
        plot = figure_print, # call the plot you are saving
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
  obj <- list("figure_print" = figure_print,
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

stacked_figures <- function(table_raw0, var00, x_name, nickname0) {
  
  table_raw <- table_raw0 |> 
    dplyr::rename(var00 = {{var00}}) |> 
    dplyr::group_by(cat, var00) |> 
    dplyr::summarise(freq = sum(freq, na.rm = TRUE)) |> 
    dplyr::ungroup() |> 
    dplyr::left_join(
      table_raw0 |> 
        dplyr::rename(var00 = {{var00}}) |> 
        dplyr::select(id, var00) |> 
        dplyr::distinct() |> 
        dplyr::group_by(var00) |> 
        dplyr::summarise(n_interviews = n()) |> 
        dplyr::ungroup()) |> 
    dplyr::mutate(freq_rel = freq/n_interviews, 
                  var001 = paste0(var00, "\n(", n_interviews, ")"))
  
  figure_print <- 
    ggplot2::ggplot(
      data = table_raw, 
      mapping = aes(x = var001, y = freq, fill = cat)) +
    ggplot2::geom_bar(position="dodge", stat="identity") +
    ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                               values = colors0) +
    ggplot2::scale_y_continuous(name = "Frequency of Code References") +
    ggplot2::scale_x_discrete(name = x_name, 
                              labels = function(x) str_wrap(x, width = 20), 
                              expand = FALSE) +
    theme_custom() 
  nickname <- paste0(nickname0, var00)
  save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)
  
  
  # Realitive frequency 
  figure_print <- 
    ggplot2::ggplot(
      data = table_raw, 
      mapping = aes(x = var00, y = freq_rel, fill = cat)) +
    ggplot2::geom_bar(position="dodge", stat="identity") +
    ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                               values = colors0) +
    ggplot2::scale_y_continuous(name = "Relative Frequency of Code References") +
    ggplot2::scale_x_discrete(name = x_name, 
                              labels = function(x) str_wrap(x, width = 20), 
                              expand = FALSE) +
    theme_custom() 
  nickname <- paste0(nickname0, var00, "-rel")
  save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

  table_raw1 <- table_raw0 |> 
    dplyr::rename(var00 = {{var00}})
  lm <- lm(freq ~ var00 + cat + (1 | id), data = table_raw1)
  temp <- lm %>% summary() %>% print(digits = 8) # show summary output
  
  return(list("figure_print" = figure_print, "lm_results" = temp)) 
  
  }

plot_pca <- function(table_raw0, var00, legend_title = "") {
  
  # Example PCA Summary to Annotate the Plot
  # Principal component analysis (PCA) was performed on frequency values across categories for each participant (id). The first two principal components (PC1 and PC2) explain X% and Y% of the variance, respectively. PC1 captures the overall differences in frequencies across categories, while PC2 captures contrasting patterns among the categories. Points are colored by indigenous00 status, and the shaded ellipses indicate the general distribution (“cloud”) of each group. Non-Indigenous and Indigenous respondents show partial separation along PC1, suggesting some group-level differences in frequency patterns.
  
  
  # 1. Reshape data to wide format (one row per ID, columns = categories)
  table_raw <- table_raw0 |> 
    dplyr::rename(var00 = {{var00}}) |> 
    tidyr::pivot_wider(
      id_cols = c(id, var00),
      names_from = cat,
      values_from = freq
    )
  
  # 2. Run PCA on the numeric columns (categories)
  pca_res <- prcomp(table_raw[, 3:ncol(table_raw)], scale. = TRUE)
  
  # 3. Add PCA scores (PC1 and PC2) to the wide table
  table_raw <- table_raw |> 
    dplyr::mutate(
      PC1 = pca_res$x[, 1],
      PC2 = pca_res$x[, 2]
    )
  
  # 4. Make PCA scatter plot colored by indigenous00
  colors0 <- viridis::mako(length(unique(table_raw$var00)), direction = -1, begin = 0.2, end = .8)
  
  figure_print <- ggplot2::ggplot(table_raw, 
                                  aes(x = PC1, y = PC2, color = var00, fill = var00)) +
    ggplot2::stat_ellipse(alpha = 0.2, geom = "polygon", color = NA) +  # semi-transparent cloud
    ggplot2::geom_point(size = 3) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(name = legend_title, # "Temperature",
                               values = colors0) +
    ggplot2::scale_color_manual(name = legend_title, # "Temperature",
                                values = colors0) +
    ggplot2::labs(
      x = paste0("PC1 (", round(summary(pca_res)$importance[2,1]*100, 1), "%)"),
      y = paste0("PC2 (", round(summary(pca_res)$importance[2,2]*100, 1), "%)")#,
      # title = "PCA of Frequencies by ID"
    ) +
    theme_custom() 
  nickname <- paste0(nickname0, var00, "-pca")
  save_figures(figure_print = figure_print, table_raw = pca_res, nickname = nickname, width = width0, height = height0)
  
  save(pca_res, file = paste0("./output/", nickname, ".rdata"))
  
  
  table_raw1 <- table_raw0 |> 
    dplyr::rename(var00 = {{var00}})
  lm <- lm(freq ~ var00 + cat + (1 | id), data = table_raw1)
  temp <- lm %>% summary() %>% print(digits = 8) # show summary output
  
  return(list(
              "figure_print" = figure_print, 
              "pca_res" = pca_res, 
              "table_raw" = table_raw, 
              "lm_results" = temp))
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
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

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
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)


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
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

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
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Grouped bar plot by ID with flipped axis and grouping rectangle ------------
figure_print <- figure_print + 
  ggplot2::coord_flip()
nickname <- paste0(nickname0, "_groupid_flipped")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

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
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

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

stacked_figures(
  table_raw0 = table_raw0, 
  var00 = "fishing_experience", 
  x_name = "Fishing Experiance", 
  nickname0 = nickname0) 

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

stacked_figures(
  table_raw0 = table_raw0, 
  var00 = "collection", 
  x_name = "Oral History Collection", 
  nickname0 = nickname0) 

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


stacked_figures(
  table_raw0 = table_raw0, 
  var00 = "indigenous00", 
  x_name = "Demographic", 
  nickname0 = nickname0) 

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

table_raw <- co_occurrence_interviews0
names(table_raw)[2:ncol(table_raw)] <- unlist(lapply(X = strsplit(x = names(table_raw)[2:ncol(table_raw)], split = "_"), '[[', 2))

# make unique entries - crab x boat is the same as boat x crab
comb <- combn(names(table_raw)[2:ncol(table_raw)], 2, simplify = FALSE)
comb <- data.frame(matrix(data = unlist(comb), ncol = 2, byrow = TRUE))
names(comb) <- c("var", "var1")

table_raw <- table_raw |> 
  dplyr::mutate(
    x1 = gsub(pattern = "[0-9]+ : ", replacement = "", x = x1), 
    dplyr::across(everything(), ~na_if(., "NULL"))
  ) |> 
  tidyr::pivot_longer(cols = boat:salmon, names_to = "var", values_to = "val") |> 
  dplyr::distinct() |> 
  dplyr::rename(var1 = x1) |> 
  dplyr::filter(!is.na(val)) |>
  dplyr::mutate(val = as.numeric(val)) |>
  dplyr::right_join(comb)

### Grouped bar plot by co-theme ------------------
colors0 <- viridis::mako(length(unique(table_raw$var)), direction = -1, begin = 0.2, end = .8)
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = var, y = val, fill = var1)) +
  ggplot2::geom_bar(position="dodge", stat="identity", width = 0.5) +
  ggplot2::scale_fill_manual(name = "Co-occured Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Number of Co-occurrences Across Transcripts", 
                              breaks = scales::pretty_breaks()) +
  ggplot2::scale_x_discrete(name = "Themes") +
  theme_custom() 
nickname <- paste0(nickname0, "_group_name")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Tile plot by co-theme ------------------
colors0 <- viridis::mako(length(unique(table_raw$val)), begin = 0.2, end = .8)
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = var, y = var1, fill = val)) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(aes(label = val)) + 
  ggplot2::guides(fill = guide_coloursteps(
    even.steps = TRUE, 
    title = "Co-occurrences\nCount")) + 
  ggplot2::theme_void() +
  ggplot2::scale_y_discrete(name = "Co-occured Themes") +
  ggplot2::scale_x_discrete(name = "Co-occured Themes") +
  theme_custom() +
  ggplot2::theme(legend.direction = "horizontal", 
                 legend.position = c(0.6, 0.1))
nickname <- paste0(nickname0, "_tile")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)


### Network plot by co-theme ------------------
# devtools::install_github("briatte/ggnet")
# library(ggnet)
# library(network)
# library(sna)
# library(ggplot2)
# library(igraph)
# library(GGally)
# library(intergraph)

# packages
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(scales)

# 1️⃣ edges with val
edges <- table_raw |>
  dplyr::select(var1, var, val)

# 2️⃣ nodes with size = total val (node strength)
nodes <- table_raw |>
  tidyr::pivot_longer(cols = c(var1, var), values_to = "node") |>
  dplyr::group_by(node) |>
  dplyr::summarise(size = sum(val), .groups = "drop")

# 3️⃣ build igraph
g <- igraph::graph_from_data_frame(
  d = edges,
  vertices = nodes,
  directed = FALSE
)

# 4️⃣ network plot with only edge thickness
figure_print <- 
  ggraph(g, layout = "fr") +   # force-directed layout
  ggraph::geom_edge_fan(
    aes(width = val),
    color = "grey60",
    alpha = 0.8
  ) +
  ggraph::geom_node_point(aes(size = size), color = "grey20") +  # node size
  ggraph::geom_node_label(                                           # <-- use geom_node_label
    aes(label = name),
    repel = TRUE, 
    color = "black", 
    label.size = 0,          # remove box border
    size = 4,
    fill = "white",          # white box behind text
    # label.padding = ggplot2::unit(0.2, "lines")#,  # padding around text
    label.r = ggplot2::unit(0, "lines")        # rounded corners
  ) +
  ggraph::scale_edge_width(
    range = c(0.5, 3),
    name = "Co-occurrence Count",
    guide = "legend"
  ) +
  ggplot2::scale_size_continuous(
    name = "Node Strength",
    range = c(3, 8)
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.text = ggplot2::element_text(size = 8),
    legend.title.position = "top",
    legend.title = ggplot2::element_text(size = 8, face = "bold"),
    legend.background = ggplot2::element_rect(colour = "transparent", fill = "transparent"),
    legend.key = ggplot2::element_rect(colour = "transparent", fill = "transparent"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.spacing = ggplot2::unit(0, "pt")
  )

## Node Strength: 
# Node strength represents the total connection weight of a node, calculated by summing the values of all edges linked to it. It highlights how strongly a node is connected in the network, with higher values indicating more or stronger connections, and complements other metrics like degree and centrality.
nickname <- paste0(nickname0, "_network")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)


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

### sentiment heat map ------------------
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = cat, y = id, fill = freq)) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(aes(label = freq)) + 
  ggplot2::scale_fill_continuous(
    name = "Frequency", 
    breaks = scales::pretty_breaks(n = 10),  # create ~10 nicely spaced breaks
    guide = ggplot2::guide_colorbar(
      barwidth = ggplot2::unit(10, "cm")  # increase the height of the color bar
    )) +
  ggplot2::scale_x_discrete(
    name = "Sentiment", 
    labels = function(x) str_wrap(x, width = 10)) + 
  ggplot2::scale_y_continuous(
    name = "Interview ID", 
    breaks = scales::pretty_breaks(n = 12)) +
  ggplot2::theme_void() + 
  theme_custom() + 
  ggplot2::theme(
    panel.border = element_blank(),
    panel.grid = element_blank()#, 
    # axis.line = element_blank()
    # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

nickname <- paste0(nickname0, "_heatmap")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Stacked by fishing experience + LM --------------------------------------

# https://steverxd.github.io/Stat_tests/three-or-more-means.html
# two ways of doing the same thing: anova and lm

# Anova
car::Anova(aov(freq ~ fishing_experience + cat, data = table_raw0))

# Anova Table (Type II tests)
# 
# Response: freq
# Sum Sq Df F value    Pr(>F)    
# fishing_experience   446.5  2  1.4577    0.2382    
# cat                 5418.4  3 11.7944 1.379e-06 ***
#   Residuals          13782.1 90                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Linear model
# lm <- lm(freq ~ fishing_experience + cat, data = table_raw0)
# temp <- lm %>% summary() %>% print(digits = 8) # show summary output
# temp 
# Call:
#   lm(formula = freq ~ fishing_experience + cat, data = table_raw0)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -21.312  -8.351  -2.248   6.713  49.777 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                  22.223      3.202   6.940 5.86e-10 ***
#   fishing_experienceCommercial & Subsistence   -3.081      2.992  -1.030  0.30579    
# fishing_experienceSubsistence                 2.089      3.442   0.607  0.54542    
# catModerately positive                       10.042      3.572   2.811  0.00606 ** 
#   catVery negative                             -5.833      3.572  -1.633  0.10598    
# catVery positive                             -9.958      3.572  -2.788  0.00648 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 12.37 on 90 degrees of freedom
# Multiple R-squared:  0.2985,	Adjusted R-squared:  0.2595 
# F-statistic:  7.66 on 5 and 90 DF,  p-value: 4.9e-06

# R² = 0.38 → About 38% of the variation in freq is explained by fishing experience and category.
# Adjusted R² = 0.35 → Still solid after penalizing for predictors.
# F-statistic p < 0.001 → The model as a whole is meaningful (mostly because of catFishing).

# IN SUMMARY
# Q: How does frequency vary by fishing experience and by some categorical attitude/assessment (cat with levels like “Moderately positive”, “Very negative”, “Very positive”)?
# Differences in frequency by fishing experience are not statistically significant. After accounting for category, Commercial/Subsistence fishers report slightly lower frequency, and Subsistence slightly higher, but these are small and noisy.
# After accounting for category, Commercial/Subsistence fishers report slightly lower frequency, and Subsistence slightly higher, but these are small and noisy.
# Frequency differed significantly across categories (F₅,₉₀ = 7.66, p < 0.001). Respondents in the Moderately positive category reported frequencies approximately 10 units higher than the reference group, while those in the Very positive category reported frequencies approximately 10 units lower. Fishing experience was not a significant predictor of frequency.

# Category matters: Moderately positive responses increase frequency, Very positive responses decrease it
# 
# Fishing experience does not significantly predict frequency
# 
# The effect of category is directional and interpretable in absolute units (~±10)

# temp0 <- as.data.frame(temp$coefficients)
# rownames(temp0)[rownames(temp0) == "(Intercept)"] <- "fishing_experienceCommerical"
# rownames(temp0)[temp0$`Pr(>|t|)` < 0.05]

stacked_figures(
  table_raw0 = table_raw0, 
  var00 = "fishing_experience", 
  x_name = "Fishing Experiance", 
  nickname0 = nickname0) 

### Stacked by oral history collection + LM --------------------------------------
# https://steverxd.github.io/Stat_tests/three-or-more-means.html
# two ways of doing the same thing: anova and lm

# Anova
car::Anova(aov(freq ~ collection + cat, data = table_raw0))

# Anova Table (Type II tests)
# 
# Response: freq
# Sum Sq Df F value    Pr(>F)    
# collection   608.4  3  1.3251    0.2713    
# cat         5418.4  3 11.8019 1.401e-06 ***
#   Residuals  13620.2 89                        
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
# -23.854  -8.846  -2.100   6.392  48.812 
# 
# Coefficients:
#   Estimate Std. Error t value
# (Intercept)                                                 26.854      4.188   6.413
# collectionOregon Residents in Alaska's Historical Fishing   -3.667      4.517  -0.812
# collectionWest Side Stories                                 -8.417      7.142  -1.178
# collectionWomen in Alaska Fisheries                         -7.050      3.912  -1.802
# catModerately positive                                      10.042      3.571   2.812
# catVery negative                                            -5.833      3.571  -1.633
# catVery positive                                            -9.958      3.571  -2.789
#                                                           Pr(>|t|)    
# (Intercept)                                               6.69e-09 ***
# collectionOregon Residents in Alaska's Historical Fishing  0.41912    
# collectionWest Side Stories                                0.24177    
# collectionWomen in Alaska Fisheries                        0.07491 .  
# catModerately positive                                     0.00606 ** 
#   catVery negative                                           0.10590    
# catVery positive                                           0.00647 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 12.37 on 89 degrees of freedom
# Multiple R-squared:  0.3068,	Adjusted R-squared:   0.26 
# F-statistic: 6.564 on 6 and 89 DF,  p-value: 9.133e-06# 

# IN SUMMARY
# Category is the key predictor of frequency: Moderately positive ↑10 units, Very positive ↓10 units
# 
# Collection has little effect, though there’s a weak trend for Women in Alaska Fisheries (−7 units, marginal)
# 
# Overall, results are consistent: attitude category drives frequency, demographic/collection variables matter less

# Q: Does frequency vary by collection and by category?

# Paper sentence:
# Frequency differed significantly across categories (F₆,₈₉ = 6.56, p < 0.001), with respondents in the Moderately positive category reporting approximately 10 units higher frequency than the reference group, and those in the Very positive category reporting approximately 10 units lower. Differences among collections were mostly non-significant, with a marginally lower frequency observed for the Women in Alaska Fisheries collection.
  
stacked_figures(
  table_raw0 = table_raw0, 
  var00 = "collection", 
  x_name = "Oral History Collection", 
  nickname0 = nickname0) 
  
### Stacked by Indigenous/Not + LM --------------------------------------
# https://steverxd.github.io/Stat_tests/three-or-more-means.html
# two ways of doing the same thing: anova and lm

# Does frequency differ by Indigenous identity and by category?

# Anova
car::Anova(aov(freq ~ indigenous00 + cat, data = table_raw0))

# Anova Table (Type II tests)
# 
# Response: freq
# Sum Sq Df F value    Pr(>F)    
# indigenous00   726.0  1  4.8928   0.02947 *  
#   cat           5418.4  3 12.1723 9.073e-07 ***
#   Residuals    13502.6 91                      
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
# -21.125  -8.333  -2.375   6.625  47.917 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  18.583      2.780   6.685 1.82e-09 ***
#   indigenous00Non-indigenous    5.500      2.486   2.212  0.02947 *  
#   catModerately positive       10.042      3.516   2.856  0.00532 ** 
#   catVery negative             -5.833      3.516  -1.659  0.10058    
# catVery positive             -9.958      3.516  -2.832  0.00570 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 12.18 on 91 degrees of freedom
# Multiple R-squared:  0.3127,	Adjusted R-squared:  0.2825 
# F-statistic: 10.35 on 4 and 91 DF,  p-value: 5.912e-07
# 
# R² = 0.39 → About 39% of variation in frequency is explained by Indigenous identity and category
# Adjusted R² = 0.36
# Overall F-test p < 0.001
# 
# IN SUMMARY
# Q: Does frequency vary by Indigenous identity and by category?
# After accounting for category, non-Indigenous respondents report frequencies ~5.5 units higher than Indigenous respondents. This difference is statistically significant.
# Both category and Indigenous identity matter here
# Category effects are consistent with previous models (Moderately positive ↑10, Very positive ↓10)
# Non-Indigenous respondents report higher frequencies than Indigenous respondents by ~5.5 units
# Category remains the strongest predictor, but Indigenous identity shows a significant effect on this larger-scale frequency measure (unlike small-scale frequency models where it was non-significant)

# Paper sentence:
# Frequency differed significantly by category and Indigenous identity (F₄,₉₁ = 10.35, p < 0.001). Respondents in the Moderately positive category reported approximately 10 units higher frequency, while those in the Very positive category reported approximately 10 units lower than the reference group. Additionally, non-Indigenous respondents reported frequencies about 5.5 units higher than Indigenous respondents.

# temp0 <- as.data.frame(temp$coefficients)
# rownames(temp0)[rownames(temp0) == "(Intercept)"] <- "fishing_experienceCommerical"
# rownames(temp0)[temp0$`Pr(>|t|)` < 0.05]

stacked_figures(
  table_raw0 = table_raw0, 
  var00 = "indigenous00", 
  x_name = "Demographic", 
  nickname0 = nickname0) 

## Figure 6: Hierarchical plot of auto themes -----------------------------------------------
#NOT DONE

nickname0 <- "fig-6-hierarchical-themes-autocode-"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw <- sentiment_raw0 
names(table_raw)[2:ncol(table_raw)] <- substr(x = names(table_raw)[2:ncol(table_raw)],start = 3, stop = nchar(names(table_raw)[2:ncol(table_raw)]))

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
  tidyr::pivot_longer(cols = very_negative:very_positive, names_to = "cat", values_to = "freq") |> 
  dplyr::mutate(cat = stringr::str_to_sentence(cat))

colors0 <- viridis::mako(length(unique(table_raw$cat)), direction = -1, begin = 0.2, end = .8)

### Grouped bar plot by narrator name with grouping rectangle ------------------
figure_print <- ""


### PCA - fishing experience ---------

plot_pca(
  table_raw0 = table_raw0, 
  var00 = "fishing_experience", 
  legend_title = "Fishing Experience") 

# After accounting for repeated measures by participant (id), frequency differed significantly across categories, with “Moderately positive” associated with ~10 units higher and “Very positive” with ~10 units lower frequency compared to the reference category. Fishing experience (var00) did not significantly predict frequency.

### PCA - oral history collection ---------

plot_pca(
  table_raw0 = table_raw0, 
  var00 = "collection", 
  legend_title = "Oral History Collection") 

# After accounting for repeated measures by participant (id), frequency differed significantly across categories, with “Moderately positive” associated with ~10 units higher and “Very positive” with ~10 units lower frequency compared to the reference category. Differences among collections (var00) were generally not significant, though the Women in Alaska Fisheries collection showed a marginally lower frequency.

### PCA - indigenous00 ---------

plot_pca(
  table_raw0 = table_raw0, 
  var00 = "indigenous00", 
  legend_title = "Indigenous Status") 

# After accounting for repeated measures by participant (id), frequency differed significantly across categories, with “Moderately positive” associated with ~10 units higher and “Very positive” with ~10 units lower frequency compared to the reference category. Additionally, non-Indigenous respondents reported frequencies about 5.5 units higher than Indigenous respondents.


