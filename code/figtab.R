
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

oralhistory_ref <- oralhistory_ref_edited0

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
                       filename0 = "x",
                       path = "./",
                       width = 6,
                       height = 6,
                       output_type = c("pdf", "png"),
                       type = "Figure",
                       alttext = "",
                       filename_desc = "",
                       nickname = "",
                       raw = NULL, 
                       bg = "white"
){
  
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
    
    # raw
    
    # Save raw file (no rounding, no dividing)
    if (!(is.null(raw)) &
        (is.data.frame(raw) | is.matrix(raw))) {
      # for (i in 1:length(output_type)){
      utils::write.table(x = raw,
                         file = paste0(path, nickname,
                                       ".csv"),
                         sep = ",",
                         row.names=FALSE, col.names = TRUE, append = F)
      # }
    } else {
      raw <- ""
    }
    
  }
  
  write.table(x = caption, 
              file = paste0(path, nickname, ".txt"), 
              row.names = FALSE, 
              col.names = FALSE, 
              quote = FALSE)
  
  # Save Graphic/Figure as .rdata
  obj <- list("figure" = figure,
              "raw" = raw,
              "caption" = caption,
              "header" = header,
              "nickname" = nickname,
              "alttext" = alttext,
              "footnotes" = footnotes,
              "filename" = nickname)
  
  save(obj, 
       file = paste0(path, nickname, ".rdata"))
  
}



save_materials <- function(
    nickname, 
    table_raw, 
    
){
  figtab <- dplyr::case_when(
    grepl(pattern = "fig-", x = nickname) ~ "fig",
    grepl(pattern = "tab-", x = nickname) ~ "tab"
  )
  
  if (figtab == "fig") {
    # Systematically save your plot with this function
    save_figures(
      figure = figure_print, 
      raw = ifelse(exists("table_raw", mode = "list"), table_raw, ""), 
      # header = ifelse(exists("header", mode = "character"), header, ""),
      # footnotes = unlist(ifelse(exists("footnotes", mode = "character"), list(footnotes), "")), 
      alttext = ifelse(exists("alttext", mode = "character"), alttext, header),
      filename0 = ifelse(exists("filename0", mode = "character"), filename0, nickname), 
      nickname = ifelse(exists("nickname", mode = "character"), nickname, filename0),
      width = ifelse(exists("width", mode = "numeric"), width, 6), 
      height = ifelse(exists("height", mode = "numeric"), height, 6),
      path = dir_out_figtab, 
      bg = ifelse(exists("bg", mode = "character"), bg, "transparent"))
    
  } else if (figtab == "tab") {
    # Systematically save your table with this function
    save_tables(
      table_raw = table_raw, 
      table_print = table_print,
      header = ifelse(exists("header", mode = "character"), header, ""),
      footnotes = unlist(ifelse(exists("footnotes", mode = "character"), list(footnotes), "")), 
      alttext = ifelse(exists("alttext", mode = "character"), alttext, header),
      filename0 = ifelse(exists("filename0", mode = "character"), filename0, nickname), 
      nickname = ifelse(exists("nickname", mode = "character"), nickname, filename0),
      path = dir_out_figtab)
  }
  
  
  # make sure you dont mistakenly name other files with these names
  remove_who <- c()
  remove_who0 <- c("figure_print", "header", "footnotes", "subobj", "newobj", #"nickname", 
                   "filename_desc", "alttext", "width", "height", 
                   "table_raw", "table_print")
  for (i in 1:length(remove_who0)){
    if(exists(remove_who0[i]) & !exists(remove_who0[i], mode = "function")){
      remove_who <- c(remove_who, remove_who0[i])
    }
  }
  remove(list = remove_who)
  
}


# Prep figures -----------------------------------------------------------------

## Figure 2: bar chart of themes -----------------------------------------------

nickname <- "fig-bar-themes"
height <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width <- full_page_portrait_width

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
                     dplyr::select(id, name = source, category, fishing_experience, collection, demographic)) |>
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

### Grouped bar plot by ID with flipped axis and grouping rectangle ------------
figure_print <- figure_print + 
  ggplot2::coord_flip()

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

### Stacked by fishing experience + ANOVA --------------------------------------

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

### Stacked by oral history collection + ANOVA --------------------------------------
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


