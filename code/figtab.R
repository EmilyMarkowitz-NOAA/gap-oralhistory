
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
  
  # Network Plots
  # devtools::install_github("briatte/ggnet")
  "igraph", 
  "ggraph", 
  
  "ggalluvial",
  
  # Spatial mapping
  "sf",
  "ggspatial", 
  "raster",
  "stars", 
  
  "treemapify", 
  "plotly",
  "reticulate", 
  
  # stats
  "lme4"
)

PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p, character.only = TRUE)) {
    install.packages(p)
  }
  require(p, character.only = TRUE)
}


# Load data --------------------------------------------------------------------

library(googledrive)
googledrive::drive_auth()
2
googledrive::drive_download(
  file = googledrive::as_id("https://docs.google.com/spreadsheets/d/1T5E2qmzUWbS6bmiI7FAP9UlVwA1Rbfyu_ORYDhS9Ujs"), 
  path = here::here("data","data_analysis.xlsx"), 
  overwrite = TRUE)

sheets <- readxl::excel_sheets(here::here("data","data_analysis.xlsx"))

for (ii in sheets){
  temp <- readxl::read_excel(path = here::here("data","data_analysis.xlsx"), sheet = ii)
  assign(x = paste0(ii, "0"), value = temp)
}

# Wrangle Data -----------------------------------------------------------------

## Mapping ---------------------------------------------------------------------

crs_out <- "EPSG:3338"
crs_in <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

## Get world map ---------------------------------------------------------------

world_coordinates <- maps::map("world", plot = FALSE, fill = TRUE) |> 
  sf::st_as_sf() |>
  # sf::st_union() |> 
  sf::st_transform(crs = crs_out) |> 
  dplyr::filter(ID %in% c("USA", "Russia", "Canada")) |> 
  dplyr::mutate(ID = ifelse(ID == "USA", "Alaska", ID))

## Get place labels for map ----------------------------------------------------

place_labels <- data.frame(
  type = c("islands", "islands", "islands", "islands", 
           "mainland", "mainland", "mainland", 
           "convention line", "peninsula", 
           "survey", "survey", "survey", "survey", "survey"), 
  lab = c("Pribilof Isl.", "Nunivak", "St. Matthew", "St. Lawrence", 
          "Alaska", "Russia", "Canada", 
          "U.S.-Russia Maritime Boundary", "Alaska Peninsula", 
          "Aleutian Islands", "Gulf of Alaska", 
          "Bering\nSea\nSlope", "Eastern\nBering Sea", "Northern\nBering Sea"), 
  angle = c(0, 0, 0, 0, 0, 0, 0, 30, 45, 0, 0, 0, 0, 0), 
  lat = c(57.033348, 60.7, 61, 64.2, 
          61, 62.798276, 63.722890, 
          62.319419, 56.352495, 
          53.25, 54.720787, 
          57, 57.456912, 63.905936), 
  lon = c(-167.767168, -168, -174, -170.123016, 
          -157.377210, 173.205231, -136.664024, 
          -177.049063, -159.029430, 
          -173, -154.794131, 
          -176, -162, -165)) |>
  dplyr::bind_rows(data.frame(
    type = "Town", 
    lab = c("Nome", "Juneau", "Anchorage", 
            "Kodiak", "Dillingham", "Togiak", 
            "Dutch Harbor", "Alakanuk", "Chignik", 
            "Anchor Point", "Kongiganak", "Egegik"), 
    angle = 0, 
    lat = c(64.504889, 58.606215, 61.171028, 
            57.399941, 59.051468, 59.057762, 
            53.829101, 62.665597, 56.291547, 
            59.746374, 59.972180, 58.209292), 
    lon = c(-165.425886, -134.333415, -149.757040, 
            -153.720566, -158.535738, -160.463840, 
            -166.719176, -164.693953, -158.428504, 
            -151.801825, -162.847553, -157.481312)
  )) |>
  dplyr::filter(type != "peninsula") |> 
  dplyr::filter(type != "survey") |> 
  # dplyr::mutate(
  #   color = dplyr::case_when(
  #     type == "mainland" ~ "grey80", 
  #     TRUE ~ "grey30"), 
  #   fontface = dplyr::case_when(
  #     type == "mainland" ~ "bold", 
  #     TRUE ~ "regular"),
  #   size = dplyr::case_when(
  #     type == "mainland" ~ 3, 
  #     TRUE ~ 2) ) |> 
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") |>
  sf::st_transform(crs = crs_out) 


demographics0 <- demographics0 |> 
  dplyr::mutate(
    env_change_openness = factor(x = env_change_openness, 
                                 levels = c("Low", "Medium", "High" ), 
                                 labels = c("Low", "Medium", "High" ), 
                                 ordered = TRUE), 
    emotional_level = factor(x = emotional_level, 
                             levels = c("Low", "Medium", "High" ), 
                             labels = c("Low", "Medium", "High" ), 
                             ordered = TRUE), 
    change_language = factor(x = change_language, 
                             levels = c("Low", "Medium", "High" ), 
                             labels = c("Low", "Medium", "High" ), 
                             ordered = TRUE)
  )

locations0 <- locations0   |> 
  sf::st_as_sf(coords = c("longitude", "latitude"), 
               remove = FALSE,
               crs = crs_in) 
# https://stackoverflow.com/questions/77179007/group-spatial-points-by-distance-in-r-how-to-group-cluster-spatial-points-so-gr
dist0 <- 100000 # example : 11000
adj <- sf::st_distance(locations0) 
# Furthermore, we can turn this into a binary matrix telling us whether each pair of stations is within 11km of each other:
adj <- matrix(as.numeric(as.numeric(adj)) < dist0, nrow = nrow(adj))
# We will see if we plot this graph, there are 4 connected components, representing clusters of stations within 11km of each other:
g100 <- igraph::graph_from_adjacency_matrix(adj)
# We can get the number of these components and put these back into our original data frame:

dist0 <- 30000 # example : 11000
adj <- sf::st_distance(locations0)
adj <- matrix(as.numeric(as.numeric(adj)) < dist0, nrow = nrow(adj))
g30 <- igraph::graph_from_adjacency_matrix(adj)

locations0 <- locations0 |> 
  dplyr::mutate(
    region100 = factor(components(g100)$membership), 
    region100_desc  = dplyr::case_when(
      region100 == 1 ~ "Dillingham", 
      region100 == 2 ~ "Anchor Point", 
      region100 == 3 ~ "Alakanuk", 
      region100 == 4 ~ "Chignik", 
      region100 == 5 ~ "Kodiak", 
      region100 == 6 ~ "Kongiganak"#, 
      # location_category == "Oregon" ~ "Oregon"
    ), 
    region30 = factor(components(g30)$membership), 
    region30  = dplyr::case_when(
      region30 == 11 ~ 7, 
      region30 == 9 ~ 4, 
      region30 == 10 ~ 3, 
      .default = as.numeric(region30)
    ), 
    region30_desc  = dplyr::case_when(
      region30 == 1 ~ "Togiak", 
      region30 == 2 ~ "Anchor Point", 
      region30 == 3 ~ "Egegik", 
      region30 == 4 ~ "Dillingham", 
      region30 == 5 ~ "Alakanuk", 
      region30 == 6 ~ "Chignik", 
      region30 == 7 ~ "Kodiak", 
      region30 == 8 ~ "Kongiganak", 
      # region30 == 9 ~ "Kongiganak", 
      # region30 == 10 ~ "Egegik"#, 
      # region30 == 11 ~ "Kodiak", 
      # location_category == "Oregon" ~ "Oregon"
    ),
    region = region100, 
    region_desc = region100_desc) |> 
  dplyr::right_join(demographics0)

demographics0 <- dplyr::left_join(
  demographics0, 
  locations0 |> 
    dplyr::filter(location_category == "LOI") |>
    sf::st_drop_geometry() |> 
    dplyr::select(last, region100, region100_desc, region30, region30_desc, region, region_desc))

# Aesthetics -------------------------------------------------------------------

full_page_portrait_width <- 6.5
full_page_portrait_height <- 7.5
full_page_landscape_width <- 9.5
# https://www.visualisingdata.com/2019/08/five-ways-to-design-for-red-green-colour-blindness/
negative <- "#EDA247"
positive <- "#57C4AD"
neutral <- "#E6E1BC"

expand_custom <- c(.005, .005)

## NOTES ------
# Color choices begin = 0.2, end = 0.8
### G/mako - Themes
### E/cviridis - Counts/Frequencies
# Functions --------------------------------------------------------------------

theme_custom <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      panel.background = element_rect(fill = "white", colour = NA), 
      panel.border = element_rect(fill = NA, colour = "grey20"), 
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12), 
      strip.text = element_text(face = "bold"), 
      plot.margin=unit(c(0,0,0,0), "cm"), 
      strip.background = element_rect(fill = "transparent", colour = "white"), 
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8, face = "bold"),
      legend.background = element_rect(colour = "transparent", 
                                       fill = "transparent"),
      legend.key = element_rect(colour = "transparent",
                                fill = "transparent"),
      legend.position = "bottom",
      legend.box = "horizontal", 
      legend.box.spacing = unit(0, "pt"), 
      
      axis.text = element_text(face = "bold"), # , size = 12 , family = font0
      legend.title.position = "top" 
    ) 
}


save_figures<-function(figure_print,
                       header = "",
                       footnotes = "",
                       filename0 = NULL,
                       path = "./output/",
                       width = 6,
                       height = 6,
                       output_type = "png", # c("pdf", "png"),
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
    
    # # Save table_raw file (no rounding, no dividing)
    # if (!(is.null(table_raw)) &
    #     (is.data.frame(table_raw) | is.matrix(table_raw))) {
    #   # for (i in 1:length(output_type)){
    #   utils::write.table(x = table_raw,
    #                      file = paste0(path, nickname,
    #                                    ".csv"),
    #                      sep = ",",
    #                      row.names=FALSE, col.names = TRUE, append = F)
    #   # }
    # } else {
    #   table_raw <- ""
    # }
    
  }
  
  # write.table(x = caption, 
  #             file = paste0(path, nickname, ".txt"), 
  #             row.names = FALSE, 
  #             col.names = FALSE, 
  #             quote = FALSE)
  # 
  # # Save Graphic/Figure as .rdata
  # obj <- list("figure_print" = figure_print,
  #             "table_raw" = table_raw,
  #             "caption" = caption,
  #             "header" = header,
  #             "nickname" = nickname,
  #             "alttext" = alttext,
  #             "footnotes" = footnotes,
  #             "filename" = nickname)
  # 
  # save(obj, 
  #      file = paste0(path, nickname, ".rdata"))
  
  return(figure_print)
}

plot_lm_pca_stacked <- function(table_raw0, var00, x_name, nickname0, legend_title) {
  
  table_raw_comb <- table_raw_combpca <- c()
  table_lm_comb <- list()
  figs <- list()
  
  for (iii in 1:length(var00)) {
    
    ## Stacked bar plot-------------------------------  
    var000 <- var00[iii]
    x_name0 <- x_name[iii]
    
    table_raw <- table_raw0 |> 
      dplyr::rename(var00 = {{var000}}) |> 
      dplyr::group_by(cat, var00) |> 
      dplyr::summarise(freq = sum(freq, na.rm = TRUE)) |> 
      dplyr::ungroup() |> 
      dplyr::left_join(
        table_raw0 |> 
          dplyr::rename(var00 = {{var000}}) |> 
          dplyr::select(id, var00) |> 
          dplyr::distinct() |> 
          dplyr::group_by(var00) |> 
          sf::st_drop_geometry() |> 
          dplyr::summarise(n_interviews = n()) |> 
          dplyr::ungroup()) |> 
      dplyr::mutate(freq_rel = freq/n_interviews, 
                    var001 = paste0(var00, "\n(", n_interviews, ")"))
    
    table_raw_comb <- dplyr::bind_rows(table_raw_comb, 
                                       table_raw |> dplyr::mutate(var000 = var000, x_name = x_name0))
    
    ### frequency bar plot ---------------------------------------------
    figure_print <- figure_print_norel <- 
      ggplot2::ggplot(
        data = table_raw, 
        mapping = aes(x = var001, y = freq, fill = cat)) +
      ggplot2::geom_bar(position="dodge", stat="identity") +
      ggplot2::scale_fill_viridis_d(
        name = "Theme", 
        option = "E", 
        begin = .2, 
        end = .8, 
        direction = -1) +
      ggplot2::scale_y_continuous(name = "Frequency of Code References", 
                                  expand = expand_custom) +
      ggplot2::scale_x_discrete(name = x_name0, 
                                labels = function(x) str_wrap(x, width = 20), 
                                expand = expand_custom) +
      theme_custom() 
    nickname <- paste0(nickname0, var000, "-stack")
    save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)
    
    figs <- c(figs, figure_print)
    names(figs)[length(figs)] <- nickname
    
    ### Realitive frequency bar plot ---------------------------------------------
    figure_print <- figure_print_rel <- 
      ggplot2::ggplot(
        data = table_raw, 
        mapping = aes(x = var00, y = freq_rel, fill = cat)) +
      ggplot2::geom_bar(position="dodge", stat="identity") +
      ggplot2::scale_fill_viridis_d(
        name = "Theme", 
        option = "E", 
        begin = .2, 
        end = .8, 
        direction = -1) +
      ggplot2::scale_y_continuous(name = "Relative Frequency of Code References", 
                                  expand = expand_custom) +
      ggplot2::scale_x_discrete(name = x_name0, 
                                labels = function(x) str_wrap(x, width = 20), 
                                expand = expand_custom) +
      theme_custom() 
    nickname <- paste0(nickname0, var000, "-stackrel")
    save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)
    
    figs <- c(figs, figure_print)
    names(figs)[length(figs)] <- nickname
    
    ## PCA -------------------------------  
    # 1. Reshape data to wide format (one row per ID, columns = categories)
    table_raw <- table_raw0 |> 
      # tidyr::drop_na() |> 
      sf::st_drop_geometry() |> 
      dplyr::rename(var00 = {{var000}}) |> 
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
    
    # 4. Make PCA scatter plot colored by indigenous_status
    colors0 <- viridis::mako(length(unique(table_raw$var00)), direction = -1, begin = 0.2, end = .8)
    
    # figure_print <- figure_print_pca <- 
    #   ggplot2::ggplot(table_raw, 
    #                   aes(x = PC1, y = PC2, color = var00, fill = var00)) +
    #   ggplot2::stat_ellipse(alpha = 0.1, # , color = NA
    #                         geom = "polygon") +  # semi-transparent cloud
    #   ggplot2::geom_point(size = 3, 
    #                       alpha = 0.8) +
    #   ggplot2::theme_minimal()  + 
    #   # see::scale_fill_oi(name = x_name0,palette = "black_first") + 
    #   # see::scale_color_oi(name = x_name0,palette = "black_first") +
    #   ggplot2::scale_fill_discrete(name = x_name0,
    #                              values = colors0) +
    #   ggplot2::scale_color_manual(name = x_name0,
    #                               values = colors0) +
    #   ggplot2::labs(
    #     x = paste0("PC1 (", round(summary(pca_res)$importance[2,1]*100, 1), "%)"),
    #     y = paste0("PC2 (", round(summary(pca_res)$importance[2,2]*100, 1), "%)")#,
    #     # title = "PCA of Frequencies by ID"
    #   ) +
    #   theme_custom() 
    # nickname <- paste0(nickname0, var000, "-pca")
    # save_figures(figure_print = figure_print, table_raw = pca_res, nickname = nickname, width = width0, height = height0)
    
    figs <- c(figs, figure_print)
    names(figs)[length(figs)] <- nickname
    table_raw_combpca <- dplyr::bind_rows(table_raw_combpca, 
                                          table_raw |> dplyr::mutate(var000 = var000, x_name = x_name0))
    # ## Box and Whisker ---------------
    # 
    # figure_print <- figure_print_boxw <- 
    #   ggplot2::ggplot(
    #     data = table_raw, 
    #     mapping = aes(x = var001, y = freq, fill = cat)) +
    #   geom_boxplot(notch = TRUE) +
    #   ggplot2::scale_fill_viridis_d(
    #     name = "Theme", 
    #     option = "E", 
    #     begin = .2, 
    #     end = .8, 
    #     direction = -1) +
    #   ggplot2::scale_y_continuous(name = "Frequency of Code References", 
    #                               expand = expand_custom) +
    #   ggplot2::scale_x_discrete(name = x_name, 
    #                             labels = function(x) str_wrap(x, width = 20), 
    #                             expand = expand_custom) +
    #   theme_custom() 
    # nickname <- paste0(nickname0, var00)
    # save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)
    
    # https://steverxd.github.io/Stat_tests/three-or-more-means.html
    # two ways of doing the same thing: anova and lm
    
    # ## Anova -----------------------------------------
    # table_aov <- car::Anova(aov(freq ~ cat + cat, data = table_raw0))
    # 
    # ## Linear mixed-effects model -------------------------------
    # table_lm <- table_raw0 |> 
    #   dplyr::rename(var00 = {{var000}}) |> 
    #   dplyr::mutate(id = as.numeric(paste0(id)))
    # # table_lm <- lm(freq ~ var00 + cat + (1 | id), data = table_lm)
    # table_lm <- lme4::lmer(freq ~ var00 + cat + (1 | id), data = table_lm)
    # # + (1 | id) ==> Each person gets their own baseline level of freq.
    # 
    # # table_lm <- lm0 |> summary() |> print(digits = 8) # show summary output
    # 
    # table_lm_comb <- c(table_lm_comb, table_lm)
    # names(table_lm_comb)[length(table_lm_comb)] <- x_name0
    
  }
  
  ## Combined facet ---------------------------------------------------
  ### frequency bar plot ---------------------------------------------
  figure_print <- figure_print_norel <- 
    ggplot2::ggplot(
      data = table_raw_comb, 
      mapping = aes(x = var001, y = freq, fill = cat)) +
    ggplot2::geom_bar(position="dodge", stat="identity") +
    ggplot2::scale_fill_viridis_d(
      name = "Theme", 
      option = "E", 
      begin = .2, 
      end = .8, 
      direction = -1) +
    ggplot2::scale_y_continuous(name = "Frequency of Code References",
                                expand = expand_custom) +
    ggplot2::scale_x_discrete(name = x_name,
                              labels = function(x) str_wrap(x, width = 20),
                              expand = expand_custom) +
    theme_custom() + 
    ggplot2::facet_wrap(vars(x_name), scales = "free")
  nickname <- paste0(nickname0, "comb-stack")
  save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)
  figs <- c(figs, figure_print)
  names(figs)[length(figs)] <- nickname
  
  ### Realitive frequency bar plot ---------------------------------------------
  figure_print <- figure_print_rel <- 
    ggplot2::ggplot(
      data = table_raw_comb, 
      mapping = aes(x = var00, y = freq_rel, fill = cat)) +
    ggplot2::geom_bar(position="dodge", stat="identity") +
    ggplot2::scale_fill_viridis_d(
      name = "Theme", 
      option = "E", 
      begin = .2, 
      end = .8, 
      direction = -1) +
    ggplot2::scale_y_continuous(name = "Relative Frequency of Code References", 
                                expand = expand_custom) +
    ggplot2::scale_x_discrete(name = x_name, 
                              labels = function(x) str_wrap(x, width = 20), 
                              expand = expand_custom) +
    theme_custom() + 
    ggplot2::facet_wrap(vars(x_name), scales = "free")
  nickname <- paste0(nickname0, "comb-stackrel")
  save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)
  figs <- c(figs, figure_print)
  names(figs)[length(figs)] <- nickname
  
  ## PCA -------------------------------  
  
  figure_print <- figure_print_pca <- 
    ggplot2::ggplot(table_raw_combpca, 
                    aes(x = PC1, y = PC2, color = var00, fill = var00)) +
    ggplot2::stat_ellipse(alpha = 0.1, # , color = NA
                          geom = "polygon") +  # semi-transparent cloud
    ggplot2::geom_point(size = 3, 
                        alpha = 0.8) +
    ggplot2::theme_minimal() +
    # see::scale_fill_oi(name = x_name0, palette = "black_first") + 
    # see::scale_color_oi(name = x_name0, palette = "black_first") +
    # ggplot2::scale_fill_viridis_d(name = "", option = "E", begin = .2, end = .8) +
    # ggplot2::scale_color_viridis_d(name = "", option = "E", begin = .2, end = .8) +
    # ggplot2::scale_shape(name = "") +    
    ggplot2::labs(
      x = paste0("PC1 (", round(summary(pca_res)$importance[2,1]*100, 1), "%)"),
      y = paste0("PC2 (", round(summary(pca_res)$importance[2,2]*100, 1), "%)")#,
      # title = "PCA of Frequencies by ID"
    ) +
    theme_custom() + 
    ggplot2::facet_wrap(vars(x_name))
  nickname <- paste0(nickname0, "comb-pca")
  save_figures(figure_print = figure_print, table_raw = pca_res, nickname = nickname, width = width0, height = height0)
  figs <- c(figs, figure_print)
  names(figs)[length(figs)] <- nickname
  
  return(list("figs" = figs, "table_lm_comb" = table_lm_comb)) 
  
}


plot_tileheat <- function(table_raw0, nickname0, facet_var = NULL) {
  
  table_raw <- table_raw0 |> 
    dplyr::mutate(cat0 = dplyr::case_when(
      cat == "Moderately negative" ~ " -",
      cat == "Very negative" ~ "--",
      cat == "Moderately positive" ~ " +",
      cat == "Very positive" ~ "++"
    )) |> 
    dplyr::mutate(cat0 = factor(x = cat0, 
                                levels = c("++", " +", " -", "--"), 
                                labels = c("++", " +", " -", "--"),
                                ordered = TRUE))
  
  temp <- table_raw |> 
    dplyr::group_by(cat0) |> 
    dplyr::summarise(mean = mean(freq, na.rm = TRUE), 
                     n = n(), 
                     sd = sd(freq, na.rm = TRUE)) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      across(where(is.numeric), round, digits = 1), 
      facet_var1 = paste0(cat0, ": ", 
                          "x̄=",mean,", sd=",sd,", n=",n))
  
  if (!is.null(facet_var)) {
    temp <- table_raw |> 
      dplyr::rename(facet_var = {{facet_var}}) |> 
      dplyr::group_by(facet_var, cat0) |> 
      dplyr::summarise(mean = mean(freq, na.rm = TRUE), 
                       n = n(), 
                       sd = sd(freq, na.rm = TRUE)) |> 
      dplyr::ungroup() |> 
      dplyr::mutate(
        across(where(is.numeric), round, digits = 1), 
        str0 = paste0(cat0, ifelse(n==1, "", paste0(": x̄=",mean,", sd=",sd)))) #,", n=",)
    
    temp <- temp |> 
      dplyr::group_by(facet_var) |> 
      dplyr::summarise(str0 = paste0(str0, collapse = "\n"), 
                       n = mean(n)) |> 
      dplyr::ungroup() |> 
      dplyr::mutate(facet_var1 = paste0(facet_var, " (", n, ")\n", str0))
    
    table_raw <- table_raw |> 
      dplyr::rename(facet_var = {{facet_var}}) |> 
      dplyr::left_join(temp) |> 
      dplyr::mutate(facet_var = facet_var1)
  }
  
  figure_print <- 
    ggplot2::ggplot(
      data = table_raw |> dplyr::mutate(id = factor(id, ordered = TRUE)), 
      mapping = aes(x = cat, y = id, fill = freq)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(aes(label = freq)) + 
    ggplot2::scale_fill_viridis_c(
      name = "Frequency", 
      option = "E", 
      begin = .2, 
      end = .8, 
      direction = -1, 
      breaks = scales::pretty_breaks(n = 10),  # create ~10 nicely spaced breaks
      guide = ggplot2::guide_colorbar(
        # draw.lim = TRUE, 
        barwidth = ggplot2::unit(10, "cm")  # increase the height of the color bar
      )) +
    ggplot2::scale_x_discrete(
      name = "Sentiment", 
      expand = expand_custom, 
      labels = function(x) str_wrap(x, width = 10)) + 
    ggplot2::scale_y_discrete(
      expand = expand_custom, 
      name = "Interview ID") +
    ggplot2::theme_void() + 
    theme_custom() + 
    ggplot2::theme(
      panel.border = element_blank(),
      panel.grid = element_blank()
    )
  
  nickname <- paste0(nickname0, "heatmap")
  if (!is.null(facet_var)) {
    figure_print <- 
      figure_print + 
      ggplot2::facet_wrap(vars(facet_var), scales = "free_y")
    nickname <- paste0(nickname, "-facet-", facet_var)
  } else {
    figure_print <- figure_print +
      ggplot2::ggtitle(label = "All Oral Histories", 
                       subtitle = temp$facet_var1)
  }
  save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)
  return(list(
    "figure_print" = figure_print, 
    "stats" = temp))
}


# Prepare figures --------------------------------------------------------------

## Figure 1: Hierarchical plot of themes ---------------------------------------

nickname0 <- "fig-1-hierarchical-themes-"

height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_landscape_width

table_raw0 <- autocoded_by_theme_node0 |> 
  dplyr::rename(Count = number_of_coding_references, 
                Theme = theme, 
                Node = node, 
                Method = method) |> 
  # dplyr::filter(Method != "NVivo_Notes") |> 
  dplyr::mutate(Method = ifelse(Method == "NVivo_Transcript", "Transcript", Method), 
                Method = ifelse(Method == "NVivo_Notes", "Notes", Method), 
                Theme = stringr::str_to_sentence(Theme))

### Treemap -------------------------------------------------------------------
# A treemap is excellent for showing the relative "weight" of nodes within a theme. We use subgroup to create the visual borders between "boat" and "change."
library(treemapify)
# table_raw <- table_raw0 |>
#   # 1. Group by everything except value to consolidate duplicates
#   dplyr::group_by(method, Theme, node) |>
#   dplyr::summarise(Count = sum(Count), .groups = "drop") |>
#   # 2. Filter out nodes that appear fewer than (e.g.) 3 times
#   dplyr::filter(Count >= 2)

# # Including phrases with greater than 1 node
# table_raw <- dplyr::bind_rows(
#   table_raw0 |> 
#     dplyr::filter(Count > 1), 
#   table_raw0 |>
#     dplyr::filter(Count == 1) |>
#     dplyr::group_by(method, Theme) |>
#     dplyr::summarise(Count = sum(Count), .groups = "drop") |>
#     dplyr::mutate(node = "other")) |> 
#   dplyr::rename(Count = Count)

table_raw <- table_raw0

figure_print <- ggplot2::ggplot(
  data = table_raw, 
  mapping = aes(area = Count, 
                fill = Theme, 
                label = Node, 
                subgroup = Theme)) +
  treemapify::geom_treemap() +
  # Add white borders around the themes
  treemapify::geom_treemap_subgroup_border(colour = "white", size = 5) +
  # Add the theme names as large background text
  treemapify::geom_treemap_subgroup_text(
    place = "centre", 
    grow = TRUE, 
    alpha = 0.25, 
    colour = "black", 
    fontface = "italic") +
  # Add the specific node labels
  treemapify::geom_treemap_text(
    colour = "white", 
    place = "centre", 
    reflow = TRUE) +
  # ggplot2::labs(title = "Hierarchy Treemap", 
  #               subtitle = "Including phrases with greater than 1 node") +
  ggplot2::facet_wrap(
    ~ Method, 
    ncol = 1, 
    strip.position = "left") +
  ggplot2::guides(
    fill = guide_legend(nrow = 1, 
                        title.position = "left",
                        label.position = "right")) +
  theme_custom() + 
  ggplot2::theme(
    strip.text = element_text(size = 15), 
    # legend.position = "bottom"
    legend.position = "none") +
  ggplot2::scale_color_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8)  + 
  ggplot2::scale_fill_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8) 

nickname <- paste0(nickname0, "treemap")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = 13, height = width0)

### Sunburst Chart -------------------------------------------------------------

# 1. Prepare data for faceted rings by Method
# Level 1: Theme (Inner ring)
lev1 <- table_raw0 |>
  dplyr::group_by(Method, Theme) |>
  dplyr::summarise(value = sum(Count), .groups = "drop") |>
  dplyr::mutate(level = 1, 
                label = Theme)

# Level 2: Node (Outer ring)
lev2 <- table_raw0 |>
  dplyr::mutate(level = 2, 
                label = Node, 
                value = Count)

# 2. Combine and calculate together
table_raw <- dplyr::bind_rows(lev1, lev2) |>
  dplyr::arrange(Method, level, Theme, label) |>
  dplyr::mutate(Theme = factor(Theme), 
                label = factor(label)) |>
  # 1. Calculate the TOTAL refs for each method to find the overall max
  dplyr::group_by(Method) |>
  dplyr::mutate(method_max = sum(value[level == 2])) |> 
  dplyr::ungroup() |>
  # 2. Identify the global maximum (the Transcript total)
  dplyr::mutate(global_total = max(method_max)) |>
  # 3. Calculate positions relative to the GLOBAL total
  dplyr::group_by(Method, level) |>
  dplyr::mutate(
    facet_total = sum(value),
    running_total = cumsum(value),
    # Use global_total so that Notes and Transcript start and end at the same relative angles
    y_pos = (running_total - 0.5 * value),
    # y_pos = facet_total - (running_total - 0.5 * value),
    
    # ANGLE CALCULATION: Now using the global_total as the denominator
    raw_angle = 90 - (360 * (y_pos / global_total)),
    
    # FLIP LOGIC
    angle = ifelse(raw_angle < -90, raw_angle + 180, raw_angle),
    hjust_var = ifelse(raw_angle < -90, 1, 0)
  ) |>
  dplyr::ungroup()

# 2. Create the Plot
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = level, y = value, fill = Theme)) +
  
  # RINGS - Use "stack" but explicitly tell it NOT to sort
  ggplot2::geom_col(
    width = 1, 
    linewidth = .2, 
    color = "white", 
    position = position_stack(reverse = TRUE)) +
  # position = position_stack(reverse = FALSE)) +
  
  # CENTER TITLE
  ggplot2::geom_text(
    data = table_raw |> dplyr::distinct(Method),
    mapping = aes(x = 0, y = 0, label = Method),
    inherit.aes = FALSE, 
    size = 5, 
    fontface = "bold") +
  
  # INNER LABELS - Use manual y_pos to match Level 2 exactly
  ggplot2::geom_text(
    data = table_raw |> filter(level == 1),
    mapping = aes(x = 1, y = y_pos, label = label),
    color = "black", 
    size = 2.5, 
    inherit.aes = FALSE) +
  
  # OUTER LABELS - Use manual y_pos and geom_text
  ggplot2::geom_text(
    data = table_raw |> filter(level == 2),
    mapping = aes(
      x = 2.6,        # Distance from center (just outside Level 2)
      y = y_pos, 
      label = label,
      angle = angle, 
      hjust = hjust_var,
      colour = Theme
    ),
    size = 1.5,
    check_overlap = FALSE, # Optional: hide labels that would crash into each other
    inherit.aes = FALSE
  ) +
  
  # IMPORTANT: theta = "y" and keep the scales consistent
  ggplot2::coord_polar(
    theta = "y", 
    clip = "off") + 
  ggplot2::facet_wrap(~ Method, scales = "fixed") + 
  theme_custom() +
  ggplot2::theme_void() +
  ggplot2::theme(
    # 0.15 is roughly the left side, 0.3 is roughly the bottom-left area
    # legend.position = c(0.15, 0.6), 
    legend.position = "none", 
    # 2. Adjust legend styling for the small space
    legend.direction = "vertical",
    legend.background = element_blank(),
    # legend.key.size = unit(0.8, "lines"),
    # legend.text = element_text(size = 8),
    
    # Forces the area behind the circles to be white
    # panel.background = element_rect(fill = "white", color = NA),
    # Forces the entire image canvas to be white
    # plot.background = element_rect(fill = "white", color = NA),
    # Remove outer margins (top, right, bottom, left)
    plot.margin = margin(t = 35, r = 40, b = 35, l = -40, unit = "pt"),
    # Increase space between the subplots
    # You can use "cm", "in", or "lines"
    panel.spacing = unit(5, "lines"),
    # legend.position = "bottom",
    strip.text = element_blank()
  ) +
  # Legend on one row, title on the left
  ggplot2::guides(fill = guide_legend(
    ncol = 1, 
    title.position = "top",
    title.vjust = 0.5
  )) + 
  ggplot2::scale_color_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8)  + 
  ggplot2::scale_fill_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8) 

nickname <- paste0(nickname0, "sunburst")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### The Sankey Diagram - mixed methods ----------------------------------------

# If you want to see how the volume of references "flows" from the Method (Transcript/Notes) to the Theme and finally to the specific Node, a Sankey is much more intuitive.
# install.packages("ggalluvial")
library(ggalluvial)

table_raw <- table_raw0 |> 
  dplyr::mutate(Theme = stringr::str_to_title(Theme) )

figure_print <- ggplot2::ggplot(
  data = table_raw,
  mapping = aes(y = Count, 
                axis1 = Method, 
                axis2 = Theme, 
                axis3 = Node)) +
  ggalluvial::geom_alluvium(
    mapping = aes(fill = Theme), 
    width = 1/12) +
  ggalluvial::geom_stratum(
    width = 1/12, 
    fill = "white", 
    color = "grey") +
  # 1. Labels for Method and Theme (Rotated 90)
  ggplot2::geom_text(
    stat = "stratum", 
    aes(label = after_stat(ifelse(stratum %in% table_raw$Node, NA, as.character(stratum)))), 
    size = 3, 
    angle = 90,
    na.rm = TRUE
  ) +
  
  # 2. Labels for Node (Horizontal)
  ggplot2::geom_text(
    stat = "stratum", 
    aes(label = after_stat(ifelse(stratum %in% table_raw$Node, as.character(stratum), NA))), 
    size = 2.5, 
    angle = 0, # Horizontal
    hjust = 1, # 1 = Right justified
    nudge_x = 0.04,     # Adjust this decimal to move text closer to/further from the right line
    na.rm = TRUE
  ) +
  ggplot2::theme_void() +
  # theme_custom() + 
  ggplot2::scale_x_discrete(
    limits = c("Method", "Theme", "Node"), 
    expand = c(.05, .05),
    position = "top" 
  ) +
  ggplot2::theme(
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    
    legend.position = "none") + 
  ggplot2::scale_color_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8)  + 
  ggplot2::scale_fill_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8) 

nickname <- paste0(nickname0, "sankey-mixedmethods")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = full_page_portrait_width, height = full_page_portrait_height)

### The Sankey Diagram - facet methods ----------------------------------------

figure_print <- ggplot2::ggplot(
  data = table_raw,
  mapping = aes(y = Count, 
                # axis1 = method, 
                axis2 = Theme, 
                axis3 = Node)) +
  ggalluvial::geom_alluvium(
    mapping = aes(fill = Theme), 
    width = 1/12) +
  ggalluvial::geom_stratum(
    width = 1/12, 
    fill = "white", 
    color = "grey") +
  # 1. Labels for Method and Theme (Rotated 90)
  ggplot2::geom_text(
    stat = "stratum", 
    aes(label = after_stat(ifelse(stratum %in% table_raw$Node, NA, as.character(stratum)))), 
    size = 3, 
    angle = 90,
    na.rm = TRUE
  ) +
  
  # 2. Labels for Node (Horizontal)
  ggplot2::geom_text(
    stat = "stratum", 
    aes(label = after_stat(ifelse(stratum %in% table_raw$Node, as.character(stratum), NA))), 
    size = 2.5, 
    hjust = 1, # 1 = Right justified
    nudge_x = 0.04,     # Adjust this decimal to move text closer to/further from the right line
    angle = 0, # Horizontal
    na.rm = TRUE
  ) +
  ggplot2::theme_void() +
  # theme_custom() + 
  ggplot2::scale_x_discrete(
    limits = c("Theme", "Node"), 
    expand = c(.05, .05),
    position = "top" 
  ) +
  ggplot2::theme(
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    strip.text.y.left = element_text(
      size = 12, 
      face = "bold", 
      angle = 90,           # 0 = Horizontal
      # hjust = 1,           # Right-align the labels
      # margin = margin(r = 10)
    ), 
    legend.position = "none") + 
  ggplot2::facet_wrap(
    ~ Method, 
    scales = "free_y", 
    ncol = 1, 
    strip.position = "left") + 
  ggplot2::scale_color_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8)  + 
  ggplot2::scale_fill_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8) 

nickname <- paste0(nickname0, "sankey-facetmethods")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = full_page_portrait_width, height = full_page_portrait_height)

### Box and whisker ------------------------------------------------------------

table_raw <- table_raw0

# 1. Plotting the statistical distribution of nodes within each Theme
figure_print <- ggplot2::ggplot(
  data = table_raw, 
  mapping = aes(x = reorder(Theme, Count, FUN = median), 
                y = Count, 
                fill = Method)) +
  # 'position_dodge' puts the boxplots side-by-side for each Theme
  ggplot2::geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7, outlier.size = 1) +
  
  # # Flip the plot so Theme names are on the left and easy to read
  # coord_flip() +
  
  # Professional Colors (Viridis)
  ggplot2::scale_fill_viridis_d(
    option = "G", direction = -1, begin = 0.2, end = 0.8) +
  
  # Clean White Theme
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "top",
    # Remove horizontal grid lines to focus on the comparison groups
    panel.grid.major.y = element_blank(), 
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(face = "bold")
  ) +
  theme_custom() + 
  ggplot2::labs(
    # title = "Theme-Level Statistical Comparison",
    # subtitle = "Boxplots show the distribution of coding references across nodes within each theme",
    x = "Theme",
    y = "Number of Coding References",
    fill = "Method"
  )

nickname <- paste0(nickname0, "boxwhisker")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = full_page_portrait_width, height = full_page_portrait_height)


## Figure 1 cont: Summarized themes -----------------------------------------------

nickname0 <- paste0(nickname0, "summarizedthemes-")

table_raw0 <- table_raw0 |> 
  dplyr::mutate(Theme = dplyr::case_when(
    Theme %in% c("Net", "Boat", "Fishing") ~ "Net", 
    Theme %in% c("Salmon", "Crab", "Fish") ~ "Fish", 
    Theme == "Change" ~ "Change"
  ))  # |> 
# dplyr::group_by(Theme, Method) |> 
# dplyr::summarise(Count = sum(Count, na.rm = TRUE), 
#                  ) #|> 
# tidyr::pivot_wider(id_cols = "Theme", names_from = Method, values_from = Count) 

expand_custom <- c(.005, .005)

### dumbell/gap plot -----------------------------------------------------------

# Percent Difference (The "Loss" Metric)
# A simple but powerful descriptive statistic is the Mean Percentage Catch.
# $$\text{Catch Rate} = \left( \frac{\sum \text{Count}_{\text{Notes}}}{\sum \text{Count}_{\text{Transcript}}} \right) \times 100$$
# This tells you, for example, that "Notes only captured 15% of the total thematic density found in Transcripts."
table_raw <- table_raw0

# catch_rate <- (sum(table_raw$Count[table_raw$Method == "Notes"], na.rm = TRUE)/sum(table_raw$Count[table_raw$Method == "Transcript"], na.rm = TRUE))*100

# The Paired Comparison (Dumbbell Plot)This is the most effective visual for showing the "gap" for every single person.
# Why it works: It shows the direction and magnitude of the difference simultaneously.

# Pivot data to compare methods side-by-side

figure_print <- ggplot2::ggplot(
  data = table_raw, 
  mapping = aes(x = Count, 
                y = Theme)) +
  ggplot2::geom_line(aes(group = Theme), color = "grey80") +
  ggplot2::geom_point(aes(color = Method), alpha = .7, size = 3) +
  # ggplot2::facet_wrap(~Theme) +
  theme_custom() +
  ggplot2::labs(title = "Discrepancy between Methods by Interviewee"#, 
                # subtitle = paste0("The notes method only captured ",round(catch_rate, digits = 1),
                #                   "% of the total thematic density found in the Transcript method.") 
                ) + 
  ggplot2::scale_color_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8)  + 
  ggplot2::scale_fill_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8) 

nickname <- paste0(nickname0, "-dumbell")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Relative Emphasis (Percentage Bar Chart) ----------------------------------

# Transcripts will almost always have higher raw numbers because they are wordier. The real question is: Is the "flavor" of the data the same? If "Fish" makes up 50% of the transcript coding, does it also make up 50% of the notes coding?

# Statistical Goal: Compare Proportional Composition.

# The Look: 100% stacked bar chart showing the ratio of themes within each method.

figure_print <- ggplot2::ggplot(
  data = table_raw, 
  mapping = aes(x = Method, y = Count, fill = Theme)) +
  ggplot2::geom_bar(stat = "identity", position = "fill") +
  ggplot2::theme_minimal() +
  theme_custom() +
  # ggplot2::theme(bor)
  ggplot2::labs(title = "Thematic Composition by Method"#, 
                # subtitle = paste0("The notes method only captured ",
                # round(catch_rate, digits = 1),"% of the total thematic density found in the Transcript method.")
                ) + 
  ggplot2::scale_color_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8)  + 
  ggplot2::scale_fill_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8) +
  ggplot2::scale_y_continuous(name = "Proportion of Thematic Coding", 
                              expand = expand_custom, 
                              labels = scales::percent) 

nickname <- paste0(nickname0, "-percentage")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Correlation Analysis (Scatter Plot with Regression) ---------------------

# # Do the methods at least trend together? If an interviewee talks a lot about "fish" in the transcript, do the notes reflect that increase proportionally?
# 
# # 1. Calculate the stats manually for each Theme
# table_raw <- table_raw0 |> 
#   tidyr::pivot_wider(id_cols = "Theme", names_from = Method, values_from = Count) 
# 
# stats_data <- table_raw |>
#   group_by(Theme) |>
#   do({
#     mod <- lm(Transcript ~ Notes, data = .)
#     # Create the label strings
#     data.frame(
#       intercept = coef(mod)[1],
#       slope = coef(mod)[2],
#       r2 = summary(mod)$r.squared,
#       label = paste0("y = ", round(coef(mod)[2], 2), "x + ", round(coef(mod)[1], 2), 
#                      "\nR² = ", round(summary(mod)$r.squared, 3))
#     )
#   }) |>
#   ungroup() |> 
#   dplyr::mutate(
#     label = ifelse(is.nan(r2), "", label), 
#     r2 = ifelse(is.nan(r2), "", r2))
# 
# # 2. Plotting
# figure_print <- ggplot2::ggplot(
#   data = table_raw, 
#   mapping = aes(x = Notes, y = Transcript)) +
#   ggplot2::geom_point(alpha = 0.6) +
#   ggplot2::geom_smooth(method = "lm", se = FALSE, color = "blue") +
#   # Add the text using the stats_data table
#   # We use Inf/-Inf to pin the text to the corners regardless of the scale
#   ggplot2::geom_text(data = stats_data, 
#                      aes(x = -Inf, y = Inf, label = label), 
#                      hjust = -0.1, vjust = 1.1, size = 3.5, inherit.aes = FALSE) +
#   ggplot2::facet_wrap(~Theme, scales = "free") +
#   ggplot2::theme_minimal() +
#   ggplot2::labs(title = "Correlation of Theme Density with Regression")
# 
# nickname <- paste0(nickname0, "-correllation")
# save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Paired Wilcoxon Signed-Rank Test -------------------------------------------

# Because coding counts are often non-normal (skewed by a few very talkative people), a parametric t-test is usually inappropriate. The Wilcoxon Signed-Rank Test evaluates whether the "Transcript" method consistently yields higher counts than the "Notes" method.Hypothesis: $H_0: \text{Median Difference} = 0$.Interpretation: If $p < 0.05$, the methods produce significantly different volumes of data.Rlibrary(dplyr)
table_raw <- table_raw0 |>
  tidyr::pivot_wider(id_cols = c(Theme, Node), names_from = Method, values_from = Count, values_fn = sum, values_fill = 0) 

# Run test for a specific theme (e.g., 'Fish')
pp <- wilcox.test(table_raw$Transcript, table_raw$Notes, paired = TRUE)
# Wilcoxon signed rank test with continuity correction
# 
# data:  table_raw$Transcript and table_raw$Notes
# V = 1363.5, p-value = 1.914e-05
# alternative hypothesis: true location shift is not equal to 0

# 1. The Statistical Verdict

# The p-value ($1.108 \times 10^{-5}$) is extremely small (well below the standard $0.05$ threshold).
# Significant Difference: There is a statistically significant difference between the counts produced by the Transcript method and the Notes method.
# Direction of Bias: Since $V = 1015.5$ is a high positive value in this context (assuming your Transcript counts are higher than Notes), it confirms that the Transcript method is yielding significantly more thematic density.The "So What?": You have statistical proof that relying solely on notes results in a significant "loss" of data volume compared to full transcription.
# Figure: Since the Wilcoxon test proves a "location shift" (one is higher than the other), the best way to visualize this for a reader is a Boxplot with Significance Brackets. This makes the $p$-value result intuitive.

library(ggpubr) # Great for adding p-values to plots
table_raw <- table_raw0
figure_print <- ggplot2::ggplot(table_raw, aes(x = Method, y = Count, fill = Method)) +
  ggplot2::geom_boxplot(alpha = 0.7, width = 0.5) +
  ggplot2::geom_jitter(width = 0.1, alpha = 0.3) + # Shows the individual interviewees
  ggplot2::theme_minimal() +
  ggplot2::scale_fill_manual(values = c("Notes" = "#E69F00", "Transcript" = "#56B4E9")) +
  ggplot2::labs(
    title = "Comparison of Coding Density",
    subtitle = paste0("Wilcoxon Signed-Rank Test with Continuity Correction: p = ", formatC(pp$p.value, digits = 2, format = "f")),
    y = "Number of Coding References"
  ) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::scale_color_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8)  + 
  ggplot2::scale_fill_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8) 

nickname <- paste0(nickname0, "-wilcoxon")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Spearman’s Rank Correlation ----------------------------------------------- 

# ($\rho$)This measures consistency rather than raw volume. 
# It answers: "If Interviewee A is the top person for 'Net' mentions in the Transcript, are they also the top person in the Notes?"
# Interpretation: A high $\rho$ (e.g., $> 0.8$) suggests that while Notes might have fewer words, they capture the relative importance of themes correctly.


### Bland-Altman Plot ------------------------------------------------------------
# This is the gold standard for "Method Agreement" in statistics. 
# It plots the Average of the two methods on the $x$-axis and the Difference on the $y$-axis.
# Why it works: It reveals if the "Notes" method gets less accurate as the interview gets longer/more complex. 
# If the dots fan out as you move right, it means Notes fail specifically in high-density interviews.

# How to Read Your Result:
#   The Bias (Blue Line): Since your Wilcoxon test was significant, this line will be above zero. This represents the average number of references "lost" when using notes instead of transcripts.
# 
# The "Fan" Effect (Heteroscedasticity): Check the shape of the points.
# 
# Consistent: If the dots stay in a horizontal band, your notes are equally reliable regardless of how much the interviewee talks.
# 
# Fanning Out: If the cloud of dots gets wider as you move to the right (higher Average), it means your notes become less reliable during very intense, high-density interviews.
# 
# Outliers: Any points outside the Red Dotted Lines (Limits of Agreement) are cases where the discrepancy between the two methods was unusually high.

# 1. Prepare the data (assuming you have 'Notes' and 'Transcript' columns)
table_raw <- table_raw0 |>
  tidyr::pivot_wider(id_cols = c(Theme, Node), names_from = Method, values_from = Count, values_fn = sum, values_fill = 0) 

ba_data <- table_raw |>
  mutate(
    Avg  = (Transcript + Notes) / 2,
    Diff = Transcript - Notes
  )

# 2. Calculate the statistical lines
bias      <- mean(ba_data$Diff, na.rm = TRUE)
sd_diff   <- sd(ba_data$Diff, na.rm = TRUE)
upper_loa <- bias + (1.96 * sd_diff)
lower_loa <- bias - (1.96 * sd_diff)

# 3. Create the Plot
figure_print <- ggplot(ba_data, aes(x = Avg, y = Diff)) +
  geom_point(alpha = 0.6, size = 2) +
  
  # Horizontal line at 0 (Perfect Agreement)
  geom_hline(yintercept = 0, linetype = "solid", color = "grey50") +
  
  # Bias line (The average gap)
  geom_hline(yintercept = bias, linetype = "dashed", color = "blue", size = 0.8) +
  
  # Limits of Agreement (LoA)
  geom_hline(yintercept = upper_loa, linetype = "dotted", color = "red") +
  geom_hline(yintercept = lower_loa, linetype = "dotted", color = "red") +
  
  # Labels for the lines
  annotate("text", x = max(ba_data$Avg), y = bias, label = "Bias", 
           vjust = -1, hjust = 1, color = "blue") +
  annotate("text", x = max(ba_data$Avg), y = upper_loa, label = "+1.96 SD", 
           vjust = -1, hjust = 1, color = "red") +
  annotate("text", x = max(ba_data$Avg), y = lower_loa, label = "-1.96 SD", 
           vjust = 1.5, hjust = 1, color = "red") +
  
  theme_minimal() +
  labs(
    title = "Bland-Altman Plot: Agreement Analysis",
    subtitle = "Assessing if method bias changes with coding density",
    x = "Average of Transcript & Notes (Overall Density)",
    y = "Difference (Transcript - Notes)"
  ) +
  facet_wrap(~Theme, scales = "free")

nickname <- paste0(nickname0, "-blandaltman")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

##### Analysis
# 1. The "Change" Theme: High Accuracy, Low Volume
# The Look: The points are clustered very tightly near zero.
# 
# What it means: You are very accurate at capturing "Change." There is almost no difference between your notes and the transcript.
# 
# The Catch: This is likely because "Change" is a rare or very specific topic. When it comes up, you catch it. The bias (blue line) is small because there isn't much data to "lose."
# 
# 2. The "Fish" Theme: The "Classic" Note-Taking Gap
# The Look: This is a textbook "fan" shape (heteroscedasticity). As the average mentions increase (moving right), the dots spread out vertically.
# 
# What it means: This is your most inconsistent category.
# 
# For short discussions about fish, you are accurate.
# 
# For long, dense discussions, your notes "drift." Sometimes you catch a lot (dots near 0), but sometimes you miss a huge amount (the dots near the bottom red line).
# 
# Verdict: Notes are a risky substitute for transcripts in this category if the interview is "fish-heavy."
# 
# 3. The "Net" Theme: Systemic Under-Reporting
# The Look: Notice how the dots follow a clear upward diagonal trend. This is unusual for a Bland-Altman and very telling.
# 
# What it means: There is a proportional bias. The more an interviewee talks about "Nets," the more you fall behind in your notes at a constant rate.
# 
# Verdict: You aren't just "missing" things randomly; you are consistently capturing only a small fraction of the "Net" conversation. The transcript is exponentially richer here.

## Figure 2: bar chart of themes -----------------------------------------------

nickname0 <- "fig-2-bar-themes"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw <- autocoded_theme_by_interview0 |> 
  dplyr::left_join(locations0 |> 
                     dplyr::filter(location_category == "LOI") |>
                     # dplyr::mutate(id = as.numeric(paste0(id))) |> 
                     dplyr::select(last, 
                                   indigenous, indigenous_status, 
                                   fishing_experience, collection, demographic, 
                                   region30, region30_desc, region100, region100_desc, 
                                   env_change_openness, emotional_level)) |> 
  dplyr::mutate(id = dplyr::row_number())

table_raw_rect <- table_raw |> 
  dplyr::select(id) |>
  dplyr::distinct() |> 
  dplyr::mutate(
    xmin = id - 0.45, # approx start of group
    xmax = id + 0.45,  # approx end of group
    ymin = 0, 
    ymax = max(table_raw$Count)
  )

colors0 <- viridis::mako(length(unique(table_raw$Theme)), direction = -1, begin = 0.2, end = .8)

### Grouped bar plot by narrator name with grouping rectangle ------------------
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = id, 
                  y = Count, 
                  fill = Theme)) +
  ggplot2::geom_bar(position="dodge", stat="identity") +
  ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Frequency of Code References", 
                              expand = expand_custom) +
  ggplot2::scale_x_discrete(name = "Interviewee", 
                            expand = expand_custom) +
  theme_custom() + 
  ggplot2::theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1) 
  ) 
nickname <- paste0(nickname0, "group-name")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Grouped bar plot by ID with grouping rectangle -----------------------------
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = id, y = Count, fill = Theme)) +
  ggplot2::geom_bar(position="dodge", stat="identity") +
  ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Frequency of Code References", 
                              expand = expand_custom) +
  ggplot2::scale_x_discrete(name = "Interview ID", 
                            expand = expand_custom) +
  theme_custom()  + 
  ggplot2::geom_rect(
    data = table_raw_rect,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "grey", # color of the border
    alpha = 0.1,     # transparency of the fill
    inherit.aes = FALSE # prevents inheriting main plot aesthetics
  ) 
nickname <- paste0(nickname0, "groupid")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Grouped bar plot by ID with flipped axis and grouping rectangle ------------
figure_print <- figure_print + 
  ggplot2::coord_flip()
nickname <- paste0(nickname0, "groupid_flipped")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Stacked by ID --------------------------------------------------------------
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = id, y = Count, fill = Theme)) +
  ggplot2::geom_bar(position="stack", stat="identity") +
  ggplot2::scale_fill_manual(name = "Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Frequency of Code References", 
                              expand = expand_custom) +
  ggplot2::scale_x_discrete(name = "Interview ID", 
                            expand = expand_custom) +
  theme_custom() 
nickname <- paste0(nickname0, "stackedid")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

## Figure 3: Hierarchical plot of themes -----------------------------------------------
#NOT POSSIBLE TO RECREATE, this is a plot of what we've got

nickname0 <- "fig-3-hierarchical-byinterview-"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

# table_raw0 <- dplyr::bind_rows(
#   code_references_notes0 |> 
#     rename_with(~ gsub("^[a-c]_", "", .x)) |> 
#     dplyr::rename(interviewee = x1) |> 
#     dplyr::mutate(interviewee = gsub("^\\d+ : ", "", interviewee), 
#                   method = "Notes") , 
#   code_references_interviews0[,1:6] |> 
#     dplyr::rename(interviewee = x1) |>
#     dplyr::rename_with(~ gsub("^[a-z]_|_\\d+$", "", .x)) |> 
#     dplyr::mutate( 
#                   method = "Transcript", 
#                   interviewee = gsub("^\\d+ : Files\\\\", "", interviewee)) )

table_raw0 <- autocoded_theme_by_interview0 |> 
  dplyr::rename(Interviewee = last) |>
  dplyr::mutate(Theme = dplyr::case_when(
    Theme %in% c("Net", "Boat", "Fishing") ~ "Net",
    Theme %in% c("Salmon", "Crab", "Fish") ~ "Fish",
    Theme == "Change" ~ "Change"
  ))  |>
  dplyr::group_by(Interviewee, Method, Theme) |>
  dplyr::summarise(Count = sum(Count, na.rm = TRUE)) |> 
  # dplyr::filter(Method != "NVivo_Notes") |> 
  dplyr::mutate(Method = ifelse(Method == "NVivo_Transcript", "Transcript", Method), 
                Method = ifelse(Method == "NVivo_Notes", "Notes", Method))

expand_custom <- c(.005, .005)

### dumbell/gap plot -----------------------------------------------------------

# Percent Difference (The "Loss" Metric)
# A simple but powerful descriptive statistic is the Mean Percentage Catch.
# $$\text{Catch Rate} = \left( \frac{\sum \text{Count}_{\text{Notes}}}{\sum \text{Count}_{\text{Transcript}}} \right) \times 100$$
# This tells you, for example, that "Notes only captured 15% of the total thematic density found in Transcripts."
table_raw <- table_raw0

catch_rate <- (sum(table_raw$Count[table_raw$Method == "Notes"], na.rm = TRUE)/sum(table_raw$Count[table_raw$Method == "Transcript"], na.rm = TRUE))*100

# The Paired Comparison (Dumbbell Plot)This is the most effective visual for showing the "gap" for every single person.
# Why it works: It shows the direction and magnitude of the difference simultaneously.

# Pivot data to compare methods side-by-side

figure_print <- ggplot2::ggplot(
  data = table_raw, 
  mapping = aes(x = Count, 
                y = Interviewee)) +
  ggplot2::geom_line(aes(group = Interviewee), color = "grey80") +
  ggplot2::geom_point(aes(color = Method), alpha = .7, size = 3) +
  ggplot2::facet_wrap(~Theme) +
  theme_custom() +
  ggplot2::labs(title = "Discrepancy between Methods by Interviewee", 
                subtitle = paste0("The notes method only captured ",round(catch_rate, digits = 1),"% of the total thematic density found in the Transcript method.") ) + 
  ggplot2::scale_color_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8)  + 
  ggplot2::scale_fill_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8) 

nickname <- paste0(nickname0, "-dumbell")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Relative Emphasis (Percentage Bar Chart) ----------------------------------

# Transcripts will almost always have higher raw numbers because they are wordier. The real question is: Is the "flavor" of the data the same? If "Fish" makes up 50% of the transcript coding, does it also make up 50% of the notes coding?

# Statistical Goal: Compare Proportional Composition.

# The Look: 100% stacked bar chart showing the ratio of themes within each method.

figure_print <- ggplot2::ggplot(
  data = table_raw, 
  mapping = aes(x = Method, y = Count, fill = Theme)) +
  ggplot2::geom_bar(stat = "identity", position = "fill") +
  ggplot2::theme_minimal() +
  theme_custom() +
  # ggplot2::theme(bor)
  ggplot2::labs(title = "Thematic Composition by Method", 
                subtitle = paste0("The notes method only captured ",round(catch_rate, digits = 1),"% of the total thematic density found in the Transcript method.")) + 
  ggplot2::scale_color_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8)  + 
  ggplot2::scale_fill_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8) +
  ggplot2::scale_y_continuous(name = "Proportion of Thematic Coding", 
                              expand = expand_custom, 
                              labels = scales::percent) 

nickname <- paste0(nickname0, "-percentage")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Correlation Analysis (Scatter Plot with Regression) ---------------------

# Do the methods at least trend together? If an interviewee talks a lot about "fish" in the transcript, do the notes reflect that increase proportionally?

table_raw <- table_raw0 |> 
  tidyr::pivot_wider(id_cols = c(Interviewee, Theme), names_from = Method, values_from = Count, values_fill = 0)

# 1. Calculate the stats manually for each Theme
stats_data <- table_raw |>
  dplyr::group_by(Theme) |>
  do({
    mod <- lm(Transcript ~ Notes, data = .)
    # Create the label strings
    data.frame(
      intercept = coef(mod)[1],
      slope = coef(mod)[2],
      r2 = summary(mod)$r.squared,
      label = paste0("y = ", round(coef(mod)[2], 2), "x + ", round(coef(mod)[1], 2), 
                     "\nR² = ", round(summary(mod)$r.squared, 3))
    )
  }) |>
  ungroup() |> 
  dplyr::mutate(
    label = ifelse(is.nan(r2), "", label), 
    r2 = ifelse(is.nan(r2), "", r2))

# 2. Plotting
figure_print <- ggplot2::ggplot(
  data = table_raw, 
  mapping = aes(x = Notes, y = Transcript)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_smooth(method = "lm", se = FALSE, color = "blue") +
  # Add the text using the stats_data table
  # We use Inf/-Inf to pin the text to the corners regardless of the scale
  ggplot2::geom_text(data = stats_data, 
                     aes(x = -Inf, y = Inf, label = label), 
                     hjust = -0.1, vjust = 1.1, 
                     size = 3.5, 
                     inherit.aes = FALSE) +
  ggplot2::facet_wrap(~Theme, scales = "free") +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Correlation of Theme Density with Regression")

nickname <- paste0(nickname0, "-correllation")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Paired Wilcoxon Signed-Rank Test -------------------------------------------

# Because coding counts are often non-normal (skewed by a few very talkative people), a parametric t-test is usually inappropriate. The Wilcoxon Signed-Rank Test evaluates whether the "Transcript" method consistently yields higher counts than the "Notes" method.Hypothesis: $H_0: \text{Median Difference} = 0$.Interpretation: If $p < 0.05$, the methods produce significantly different volumes of data.Rlibrary(dplyr)
table_raw <- table_raw0 |>
  tidyr::pivot_wider(id_cols = c(Interviewee, Theme), names_from = Method, values_from = Count, values_fill = 0) 

# Run test for a specific theme (e.g., 'Fish')
pp <- wilcox.test(table_raw$Transcript, table_raw$Notes, paired = TRUE)
# Wilcoxon signed rank test with continuity correction
# 
# data:  table_raw$Transcript and table_raw$Notes
# V = 1363.5, p-value = 1.914e-05
# alternative hypothesis: true location shift is not equal to 0

# 1. The Statistical Verdict

# The p-value ($1.108 \times 10^{-5}$) is extremely small (well below the standard $0.05$ threshold).
# Significant Difference: There is a statistically significant difference between the counts produced by the Transcript method and the Notes method.
# Direction of Bias: Since $V = 1015.5$ is a high positive value in this context (assuming your Transcript counts are higher than Notes), it confirms that the Transcript method is yielding significantly more thematic density.The "So What?": You have statistical proof that relying solely on notes results in a significant "loss" of data volume compared to full transcription.
# Figure: Since the Wilcoxon test proves a "location shift" (one is higher than the other), the best way to visualize this for a reader is a Boxplot with Significance Brackets. This makes the $p$-value result intuitive.

library(ggpubr) # Great for adding p-values to plots
table_raw <- table_raw0
figure_print <- ggplot2::ggplot(table_raw, aes(x = Method, y = Count, fill = Method)) +
  ggplot2::geom_boxplot(alpha = 0.7, width = 0.5) +
  ggplot2::geom_jitter(width = 0.1, alpha = 0.3) + # Shows the individual interviewees
  ggplot2::theme_minimal() +
  ggplot2::scale_fill_manual(values = c("Notes" = "#E69F00", "Transcript" = "#56B4E9")) +
  ggplot2::labs(
    title = "Comparison of Coding Density",
    subtitle = paste0("Wilcoxon Signed-Rank Test with Continuity Correction: p = ", formatC(pp$p.value, digits = 7, format = "f")),
    y = "Number of Coding References"
  ) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::scale_color_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8)  + 
  ggplot2::scale_fill_viridis_d(
    option = "D", 
    direction = -1, 
    begin = .2, 
    end = .8) 

nickname <- paste0(nickname0, "-wilcoxon")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Spearman’s Rank Correlation ----------------------------------------------- 

# ($\rho$)This measures consistency rather than raw volume. 
# It answers: "If Interviewee A is the top person for 'Net' mentions in the Transcript, are they also the top person in the Notes?"
# Interpretation: A high $\rho$ (e.g., $> 0.8$) suggests that while Notes might have fewer words, they capture the relative importance of themes correctly.


### Bland-Altman Plot ------------------------------------------------------------
# This is the gold standard for "Method Agreement" in statistics. 
# It plots the Average of the two methods on the $x$-axis and the Difference on the $y$-axis.
# Why it works: It reveals if the "Notes" method gets less accurate as the interview gets longer/more complex. 
# If the dots fan out as you move right, it means Notes fail specifically in high-density interviews.

# How to Read Your Result:
#   The Bias (Blue Line): Since your Wilcoxon test was significant, this line will be above zero. This represents the average number of references "lost" when using notes instead of transcripts.
# 
# The "Fan" Effect (Heteroscedasticity): Check the shape of the points.
# 
# Consistent: If the dots stay in a horizontal band, your notes are equally reliable regardless of how much the interviewee talks.
# 
# Fanning Out: If the cloud of dots gets wider as you move to the right (higher Average), it means your notes become less reliable during very intense, high-density interviews.
# 
# Outliers: Any points outside the Red Dotted Lines (Limits of Agreement) are cases where the discrepancy between the two methods was unusually high.

# 1. Prepare the data (assuming you have 'Notes' and 'Transcript' columns)
table_raw <- table_raw0 |>
  tidyr::pivot_wider(id_cols = c(Interviewee, Theme), names_from = Method, values_from = Count) |>
  # Replace NAs with 0 in case an interviewee is missing in one method
  dplyr::mutate(Notes = replace_na(Notes, 0),
                Transcript = replace_na(Transcript, 0))

ba_data <- table_raw |>
  mutate(
    Avg  = (Transcript + Notes) / 2,
    Diff = Transcript - Notes
  )

# 2. Calculate the statistical lines
bias      <- mean(ba_data$Diff, na.rm = TRUE)
sd_diff   <- sd(ba_data$Diff, na.rm = TRUE)
upper_loa <- bias + (1.96 * sd_diff)
lower_loa <- bias - (1.96 * sd_diff)

# 3. Create the Plot
figure_print <- ggplot(ba_data, aes(x = Avg, y = Diff)) +
  geom_point(alpha = 0.6, size = 2) +
  
  # Horizontal line at 0 (Perfect Agreement)
  geom_hline(yintercept = 0, linetype = "solid", color = "grey50") +
  
  # Bias line (The average gap)
  geom_hline(yintercept = bias, linetype = "dashed", color = "blue", size = 0.8) +
  
  # Limits of Agreement (LoA)
  geom_hline(yintercept = upper_loa, linetype = "dotted", color = "red") +
  geom_hline(yintercept = lower_loa, linetype = "dotted", color = "red") +
  
  # Labels for the lines
  annotate("text", x = max(ba_data$Avg), y = bias, label = "Bias", 
           vjust = -1, hjust = 1, color = "blue") +
  annotate("text", x = max(ba_data$Avg), y = upper_loa, label = "+1.96 SD", 
           vjust = -1, hjust = 1, color = "red") +
  annotate("text", x = max(ba_data$Avg), y = lower_loa, label = "-1.96 SD", 
           vjust = 1.5, hjust = 1, color = "red") +
  
  theme_minimal() +
  labs(
    title = "Bland-Altman Plot: Agreement Analysis",
    subtitle = "Assessing if method bias changes with coding density",
    x = "Average of Transcript & Notes (Overall Density)",
    y = "Difference (Transcript - Notes)"
  ) +
  facet_wrap(~Theme, scales = "free")

nickname <- paste0(nickname0, "-blandaltman")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

##### Analysis
# 1. The "Change" Theme: High Accuracy, Low Volume
# The Look: The points are clustered very tightly near zero.
# 
# What it means: You are very accurate at capturing "Change." There is almost no difference between your notes and the transcript.
# 
# The Catch: This is likely because "Change" is a rare or very specific topic. When it comes up, you catch it. The bias (blue line) is small because there isn't much data to "lose."
# 
# 2. The "Fish" Theme: The "Classic" Note-Taking Gap
# The Look: This is a textbook "fan" shape (heteroscedasticity). As the average mentions increase (moving right), the dots spread out vertically.
# 
# What it means: This is your most inconsistent category.
# 
# For short discussions about fish, you are accurate.
# 
# For long, dense discussions, your notes "drift." Sometimes you catch a lot (dots near 0), but sometimes you miss a huge amount (the dots near the bottom red line).
# 
# Verdict: Notes are a risky substitute for transcripts in this category if the interview is "fish-heavy."
# 
# 3. The "Net" Theme: Systemic Under-Reporting
# The Look: Notice how the dots follow a clear upward diagonal trend. This is unusual for a Bland-Altman and very telling.
# 
# What it means: There is a proportional bias. The more an interviewee talks about "Nets," the more you fall behind in your notes at a constant rate.
# 
# Verdict: You aren't just "missing" things randomly; you are consistently capturing only a small fraction of the "Net" conversation. The transcript is exponentially richer here.

## Figure 4: bar chart of themes -----------------------------------------------

nickname0 <- "fig-4-bar-themes-manualcode-"
height0 <- 6 
width0 <- full_page_portrait_width

table_raw <- cooccurance0

### Grouped bar plot by co-theme ------------------
colors0 <- viridis::mako(length(unique(table_raw$Count)), direction = -1, begin = 0.2, end = .8)
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = Theme1, y = Count, fill = Theme2)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) + 
  # ggplot2::geom_bar(position="dodge", stat="identity", width = 0.5) +
  ggplot2::scale_fill_manual(name = "Co-occured Theme", # "Temperature",
                             values = colors0) +
  ggplot2::scale_y_continuous(name = "Number of Co-occurrences Across Transcripts", 
                              breaks = scales::pretty_breaks(), 
                              expand = expand_custom) +
  ggplot2::scale_x_discrete(name = "Themes", 
                            expand = expand_custom) +
  theme_custom() 
nickname <- paste0(nickname0, "group-name")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Tile plot by co-theme ------------------
colors0 <- c("grey80", 
             viridis::cividis(n = length(1:max(table_raw$Count)), 
                              begin = 0.2, end = .8, direction = -1))
# breaks = min(table_raw):max(table_raw), 

figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    # data = table_raw |> dplyr::mutate(val = ifelse(val == 0, NA, val)), 
    mapping = aes(x = Theme2, y = Theme1, fill = Count)) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(aes(label = Count)) + 
  ggplot2::scale_fill_continuous(
    name = "Co-occurrences Count",
    palette = colors0, 
    breaks = 0:max(table_raw$Count)
  ) +
  # ggplot2::scale_fill_viridis_b(
  #   name = "Co-occurrences Count", 
  #   begin = .2, 
  #   end = .8, 
  #   na.value = "white", 
  #   breaks = 1:max(table_raw$val),
  #   option = "E") + 
  ggplot2::guides(fill = guide_coloursteps(
    even.steps = FALSE,
    show.limits = TRUE, 
    barwidth = ggplot2::unit(10, "cm"))) +  # increase the height of the color bar)) + 
  ggplot2::theme_void() +
  # ggplot2::theme_minimal() +
  ggplot2::scale_y_discrete(
    expand = expand_custom, 
    name = "", 
    # name = "Co-occured Themes"
  ) +
  ggplot2::scale_x_discrete(
    expand = expand_custom, 
    name = "", 
    # name = "Co-occured Themes", 
    position = "top") +
  theme_custom() +
  ggplot2::theme(
    panel.grid = element_blank(), 
    panel.border = element_blank(), 
    axis.ticks = element_blank(), 
    legend.direction = "horizontal", 
    legend.title.position = "top", 
    axis.text.y = element_text(angle = 90, hjust = 1, vjust = 1),
    legend.position = c(0.6, 0.1))
nickname <- paste0(nickname0, "tile")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Tile plot by co-theme no legned------------------
figure_print <- 
  figure_print +
  ggplot2::scale_x_discrete(
    expand = expand_custom, 
    name = "Co-occurrences Count", 
    # name = "Co-occured Themes", 
    position = "top") +
  ggplot2::theme(
    legend.position = "none")
nickname <- paste0(nickname0, "tilenolegend")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Network plot by co-theme ------------------

# edges with val
edges <- table_raw |>
  dplyr::select(Theme1, Theme2, Count)

# nodes with size = total val (node strength)
nodes <- table_raw |>
  tidyr::pivot_longer(cols = c(Theme2, Theme1), values_to = "Node") |>
  dplyr::group_by(Node) |>
  dplyr::summarise(size = sum(Count), .groups = "drop")

# build igraph
g <- igraph::graph_from_data_frame(
  d = edges,
  vertices = nodes,
  directed = FALSE
)

# network plot with only edge thickness
figure_print <- 
  ggraph(g, layout = "fr") +   # force-directed layout
  ggraph::geom_edge_fan(
    aes(width = Count),
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
    range = c(0.5, 4),
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

nickname <- paste0(nickname0, "network")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

## Figure 5: bar chart of themes -----------------------------------------------

nickname0 <- "fig-5-sentiment-heatmap-"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw <- table_raw0 <- demographics0 |> 
  tidyr::pivot_longer(cols = very_negative:very_positive, names_to = "cat", values_to = "freq") |> 
  dplyr::mutate(
    cat = gsub(pattern = "_", replacement = " ", x = cat), 
    cat = stringr::str_to_sentence(cat), 
    cat0 = cat,
    cat = factor(x = cat, 
                 levels = c("Very negative", "Moderately negative", "Moderately positive", "Very positive" ), 
                 labels = c("Very negative", "Moderately negative", "Moderately positive", "Very positive" ), 
                 ordered = TRUE)#,
# cat0 = gsub(pattern = " ", replacement = "\n", x = as.character(cat0)), 
# cat0 = factor(x = cat0,
#                  levels = c("Very\nnegative", "Moderately\nnegative", "Moderately\npositive", "Very\npositive" ),
#                  labels = c("Very\nnegative", "Moderately\nnegative", "Moderately\npositive", "Very\npositive" ),
#                  ordered = TRUE)
  )

### scatter jitter -------------------------------------------------------------

figure_print <- ggplot2::ggplot(data = table_raw, 
       mapping = aes(
  x = change_language, 
  y = emotional_level, 
  color = fishing_experience, 
  shape = indigenous_status)) +
  geom_jitter(size = 2, width = 0.1, height = 0.1) 

nickname <- paste0(nickname0, "scatter-jitter")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Tile map -------------------------------------------------------------------
plot_tileheat(table_raw0 = table_raw0, nickname0 = nickname0, facet_var = NULL)

#### Tile facet by indigenous_status -------------------------------------------------
plot_tileheat(table_raw0 = table_raw0, nickname0 = nickname0, facet_var = "indigenous_status")

#### Tile facet by oral history collection --------------------------------------
plot_tileheat(table_raw0 = table_raw0, nickname0 = nickname0, facet_var = "collection")

#### Tile facet fishing experience  ---------------------------------------------
plot_tileheat(table_raw0 = table_raw0, nickname0 = nickname0, facet_var = "fishing_experience")

#### Tile facet language  ---------------------------------------------
plot_tileheat(table_raw0 = table_raw0, nickname0 = nickname0, facet_var = "emotional_level")
plot_tileheat(table_raw0 = table_raw0, nickname0 = nickname0, facet_var = "env_change_openness")
plot_tileheat(table_raw0 = table_raw0, nickname0 = nickname0, facet_var = "change_language")

#### Boxplot demographic with change ---------------------------------------------------------

boxplot_change <- function(x_change = "env_change_openness", 
                           y_facet = "indigenous_status", 
                           table_raw){
  title0 <- dplyr::case_when(
    x_change  == "env_change_openness" ~ "Openness to Environmental Change", 
    x_change  == "change_language" ~ "Change Language Score", 
    x_change  == "emotional_level" ~ "Emotional Level"
    )
  
  # colors0 <- viridis::cividis(nrow(unique(table_raw[,x_change])), direction = -1, begin = 0.2, end = .8)
  
  table_raw <- table_raw0 |> 
    dplyr::rename(x_change = {{x_change}}, 
                  y_facet = {{y_facet}}) 
  
  figure_print <- ggplot2::ggplot(
    data = table_raw,  
    mapping = aes(x = cat, y = freq, fill = x_change)) +
    geom_boxplot(position = position_dodge2(preserve = "single")) + 
    labs(title = title0, # "Sentiment Analysis",
         x = "Sentiment",
         y = "Frequency") +
    theme_bw() +
    theme_custom() +
    ggplot2::theme(
      legend.position = "none") +
    # ggplot2::scale_color_viridis_b(begin = .2, end = .8, direction = -1, option = "D") +
    ggplot2::facet_grid(x_change~y_facet) +
    ggplot2::scale_fill_viridis_d(
      option = "D", 
      direction = -1, begin = 0.2, end = .8#,
      # labels = function(x) str_wrap(x, width = 10)
      ) +
    ggplot2::scale_x_discrete(#palette = colors0,
                              labels = function(x) str_wrap(x, width = 10))
  
  nickname <- paste0(nickname0, "boxplot_", x_change, "_", y_facet)
  save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)
  return(figure_print)
}

table_raw <- table_raw0 

boxplot_change(x_change = "env_change_openness", 
               y_facet = "indigenous_status", 
               table_raw = table_raw)

boxplot_change(x_change = "emotional_level", 
               y_facet = "indigenous_status", 
               table_raw = table_raw)

boxplot_change(x_change = "change_language", 
               y_facet = "indigenous_status", 
               table_raw = table_raw)

boxplot_change(x_change = "env_change_openness", 
               y_facet = "fishing_experience", 
               table_raw = table_raw)

boxplot_change(x_change = "emotional_level", 
               y_facet = "fishing_experience", 
               table_raw = table_raw)

boxplot_change(x_change = "change_language", 
               y_facet = "fishing_experience", 
               table_raw = table_raw)

#### Boxplot demographic with change types grouped ---------------------------------------------------------

boxplot_change <- function(x_change = "change_score", 
                           y_facet = "indigenous_status", 
                           table_raw){
  
  table_raw <- table_raw |> 
    dplyr::rename(x_change = {{x_change}}, 
                  y_facet = {{y_facet}}) 
  # colors0 <- viridis::cividis(length(unique(table_raw$change)), direction = -1, begin = 0.2, end = .8)
  
  figure_print <- ggplot2::ggplot(
    data = table_raw,  
    mapping = aes(x = cat, y = freq, fill = change)) +
    geom_boxplot(position = position_dodge2(preserve = "single")) + # fill = "orange", alpha = 0.7) +
    labs(#title = "Sentiment Analysis",
      x = "Sentiment",
      y = "Frequency") +
    theme_custom() +
    ggplot2::theme(legend.position = "bottom", 
                   legend.title = element_blank(), 
                   legend.direction = "horizontal") + 
    ggplot2::scale_fill_viridis_d(
      option = "D", 
      direction = -1, begin = 0.2, end = .8#,
      # labels = function(x) str_wrap(x, width = 10)
      ) +
    ggplot2::scale_x_discrete(#palette = colors0,
      labels = function(x) str_wrap(x, width = 10))+
    # ggplot2::scale_x_discrete(palette = colors0, 
    #                           labels = function(x) str_wrap(x, width = 10)) +
    ggplot2::facet_grid(x_change~y_facet) 
  
  nickname <- paste0(nickname0, "boxplot_",x_change, "_", y_facet)
  save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)
  return(figure_print)
}

width0 <- full_page_landscape_width

table_raw <- table_raw0 |> 
  dplyr::select(id, last, env_change_openness, change_language, emotional_level, 
                fishing_experience, indigenous_status, collection, region100_desc, 
                cat, freq) |> 
  tidyr::pivot_longer(cols = c("env_change_openness", "change_language", "emotional_level"), 
                      names_to = "change", values_to = "change_score") |> 
  dplyr::mutate(change = dplyr::case_when(
    change  == "env_change_openness" ~ "Openness to Environmental Change", 
    change  == "change_language" ~ "Change Language Score", 
    change  == "emotional_level" ~ "Emotional Level"
  )) 

boxplot_change(x_change = "change_score", 
               y_facet = "indigenous_status", 
               table_raw = table_raw)

boxplot_change(x_change = "change_score", 
               y_facet = "fishing_experience", 
               table_raw = table_raw)

boxplot_change(x_change = "change_score", 
               y_facet = "collection", 
               table_raw = table_raw)

boxplot_change(x_change = "change_score", 
               y_facet = "region100_desc", 
               table_raw = table_raw)

figure_print <- ggplot2::ggplot(
  data = table_raw,  
  mapping = aes(x = cat, y = freq, fill = change)) +
  geom_boxplot(position = position_dodge2(preserve = "single")) + # fill = "orange", alpha = 0.7) +
  labs(#title = "Sentiment Analysis",
    x = "Sentiment",
    y = "Frequency") +
  theme_custom() +
  ggplot2::theme(legend.position = "bottom", 
                 legend.title = element_blank(), 
                 legend.direction = "horizontal") + 
  ggplot2::scale_fill_viridis_d(
    option = "D", 
    direction = -1, begin = 0.2, end = .8#,
    # labels = function(x) str_wrap(x, width = 10)
  ) +
  ggplot2::scale_x_discrete(#palette = colors0,
    labels = function(x) str_wrap(x, width = 10))+
  # ggplot2::scale_x_discrete(palette = colors0, 
  #                           labels = function(x) str_wrap(x, width = 10)) +
  ggplot2::facet_wrap(~change_score, ncol = 1) 

nickname <- paste0(nickname0, "boxplot_change_score")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### pca/lm/Stacked by [var] -----------------------------------------

# Does frequency differ by region and by category?
width0 <- full_page_portrait_width

temp <- plot_lm_pca_stacked(
  table_raw0 = table_raw0, 
  var00 = c("region100_desc","fishing_experience", "collection", "indigenous_status"),
  x_name = c("Region", "Fishing Experience", "Oral History Collection", "Indigenous Status"),
  nickname0 = nickname0) 

# a <- temp$table_lm_comb
# anova(a$Region, a$`Fishing Experience`, a$`Oral History Collection`, a$`Indigenous Status`)

### pca/lm/Stacked by [var] -----------------------------------------

# Does frequency differ by region and by category?

temp <- plot_lm_pca_stacked(
  table_raw0 = table_raw0, 
  var00 = c("change_language","env_change_openness", "emotional_level"),#, "indigenous_status"),
  x_name = c("Change Score", "Change Language Score", "Emotional Score"),#, "Indigenous Status"),
  nickname0 = nickname0) 

# a <- temp$table_lm_comb
# anova(a$`Change Score`, a$`Change Language Score`, a$`Emotional Score`)

# Figure 7: Map of Oral Histories -----------------------------------------------

nickname0 <- "fig-7-map-"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw0 <- table_raw <- locations0 

### Map of all points ------------------


#### Map boundaries ----------------------------------------------------

boundaries <- data.frame(lon = c(-170, -150), # c(-180, -140)
                         lat = c(53, 64) )  |> # c(46, 66)
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") |>
  sf::st_transform(crs = crs_out) |>
  sf::st_coordinates() |>
  data.frame()

#### Plot map --------------------------------------------------------------------

inset_map <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = world_coordinates,
                   fill = "grey70",
                   color = "grey80")  + 
  # Manage Axis extents (limits) and breaks
  ggplot2::scale_x_continuous(name = "", #"Longitude °W",
                              breaks = seq(-180, -150, 5)) +
  ggplot2::scale_y_continuous(name = "", 
                              breaks = seq(50, 65, 5)) + # seq(52, 62, 2)
  theme_custom() +
  ggplot2::theme(
    panel.grid = element_line(colour="grey80", linewidth = 0.5), 
    # legend.text = element_text(size = 10, angle = 90),
    legend.direction = "vertical",
    legend.box = "horizontal",
    # legend.justification = c(0, 1),
    legend.key = element_blank(), 
    legend.position = "bottom", # "bottom",
    legend.text.position = "right"# "bottom"
  )  + 
  ggplot2::coord_sf(xlim = boundaries$X,
                    ylim = boundaries$Y) +
  ggplot2::geom_polygon(data = data.frame(X = c(min(table_raw$X), min(table_raw$X), max(table_raw$X), max(table_raw$X)), 
                                          Y = c(min(table_raw$Y), max(table_raw$Y), max(table_raw$Y), min(table_raw$Y)) ), 
                        mapping = aes(x = X, y = Y), 
                        fill = NA, 
                        color = "red") +
  ggplot2::theme(axis.text = element_blank(), 
                 axis.ticks = element_blank(), 
                 axis.title = element_blank(), 
                 panel.grid = element_line(colour="grey80", linewidth = 0.5)
  )

map_funct  <- function(table_raw, points0 = TRUE) {
  
  figure_print <- 
    ggplot2::ggplot() + # data = table_raw) +
    ggplot2::geom_sf(data = world_coordinates,
                     fill = "grey70",
                     color = "grey80")  + 
    # Manage Axis extents (limits) and breaks
    ggplot2::scale_x_continuous(name = "Longitude °W",
                                breaks = seq(-180, -150, 2)) +
    ggplot2::scale_y_continuous(name = "Latitude °N",
                                breaks = seq(50, 65, 2)) + # seq(52, 62, 2)
    ggplot2::geom_sf_text(
      data = place_labels |> dplyr::filter(type == "mainland"),
      mapping = aes(label = lab, angle = angle), 
      color = "grey60", 
      size = 3, 
      show.legend = FALSE) + 
    ggplot2::geom_sf_text(
      data = place_labels |> dplyr::filter(type == "survey"),
      mapping = aes(label = lab, angle = angle), 
      color = "black",
      fontface = "bold",
      size = 2, 
      show.legend = FALSE) + 
    ggplot2::geom_sf_text(
      data = place_labels |> dplyr::filter(!(type %in% c("mainland", "survey"))),
      mapping = aes(label = lab, angle = angle), 
      color = "grey10", 
      fontface = "italic", 
      size = 2, 
      show.legend = FALSE) 
  
  if (points0) {
    figure_print <- figure_print +
      ggplot2::geom_sf(
        data = table_raw,
        mapping = aes(
          geometry = geometry, # st_jitter(geometry),
          # color = id,
          shape = location_category0),
        # position = ggplot2::position_jitter(width = 0.2, height = 0.2),
        size = 3, 
        color = "black",
        alpha = 0.5) 
  }
  
  figure_print <- figure_print +
    # manually define color for points
    # ggplot2::scale_color_viridis_d(
    #   option = "E", 
    #   begin = .2, 
    #   end = .8, 
    #   name = "Interview ID") +
    ggplot2::guides(color = guide_legend(ncol = 6)) + 
    ggplot2::scale_shape(name = "Location Type")  +
    theme_custom() +
    ggplot2::theme(
      panel.grid = element_line(colour="grey80", linewidth = 0.5), 
      # legend.text = element_text(size = 10, angle = 90),
      legend.direction = "vertical",
      legend.box = "horizontal",
      # legend.justification = c(0, 1),
      legend.key = element_blank(), 
      legend.position = "bottom", # "bottom",
      legend.text.position = "right"# "bottom"
    )
  return(figure_print)
}

### Map with no grouping, all data  -------------------------------------------

figure_print <- map_funct(table_raw)  + 
  ggplot2::theme(legend.position = c(0.37, 0.85)) +
  ggplot2::coord_sf(xlim = c(min(table_raw$X)-25000, max(table_raw$X)+25000),
                    ylim = c(min(table_raw$Y)-25000, max(table_raw$Y)+25000))

figure_print <- ggdraw() +
  draw_plot(figure_print) +
  draw_plot(inset_map, x = 0.675, y = 0.75, width = 0.25, height = 0.25)

nickname <- paste0(nickname0, "everything")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Map with location_category facet -------------------------------------------

figure_print <- map_funct(table_raw) + 
  ggplot2::facet_wrap(vars(location_category0), labeller = label_wrap_gen(width = 25)) + 
  ggplot2::guides(shape = "none") +
  ggplot2::coord_sf(xlim = c(min(table_raw$X), max(table_raw$X)),
                    ylim = c(min(table_raw$Y), max(table_raw$Y)))

nickname <- paste0(nickname0, "location_category")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Map by region -------------------------------------------

# figure_print <- map_funct(table_raw) +
#   ggplot2::facet_wrap(vars(region_desc), scales = "free", labeller = label_wrap_gen(width = 25)) 
# nickname <- paste0(nickname0, "_byregion")
# save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

### Map of only LOI with 100lm grouping-------------------------------------------

table_raw <- table_raw0 |> 
  dplyr::filter(location_category == "LOI") |> 
  dplyr::group_by(region, region_desc, location_category0, location_category) |> 
  dplyr::summarise(#geometry = geometry, 
    X = mean(X), 
    Y = mean(Y), 
    id = n())

figure_print <- map_funct(table_raw, points0 = FALSE)  + 
  ggplot2::theme(legend.position = c(0.37, 0.85))  + 
  ggplot2::guides(shape = "none") + 
  # ggplot2::geom_sf(
  #   data = table_raw,
  #   mapping = aes(
  #     geometry = geometry, # st_jitter(geometry),
  #     ),
  #   fill = "grey20",
  #   # color = "transparent", 
  #   alpha = .5,
  #   # vjust = -.7,
  #   size = 5,
  #   show.legend = FALSE
  # ) +
  ggplot2::geom_sf_label(
    data = table_raw,
    mapping = aes(
      geometry = geometry, # st_jitter(geometry),
      label = id
    ),
    size = 5, 
    label.r = unit(0, "lines"), 
    border.color = "grey80", 
    # color = "grey80",
    vjust = -.25,
    show.legend = FALSE
  ) +
  ggplot2::coord_sf(xlim = c(min(table_raw$X)-25000, max(table_raw$X)+25000),
                    ylim = c(min(table_raw$Y)-25000, max(table_raw$Y)+25000))

figure_print <- ggdraw() +
  draw_plot(figure_print) +
  draw_plot(inset_map, x = 0.675, y = 0.75, width = 0.25, height = 0.25)

nickname <- paste0(nickname0, "LOIonly-grouped100")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

ggplot2::ggsave( # save your plot
  path = "./output/",
  dpi = 1200,
  bg = "transparent",
  filename = paste0(nickname, ".png"), # Always save in pdf so you can make last minute edits in adobe acrobat!
  plot = figure_print, # call the plot you are saving
  width = width0, 
  height = height0, 
  units = "in") #recall, A4 pages are 8.5 x 11 in - 1 in margins


### Map of only LOI with 100lm grouping-------------------------------------------

table_raw <- table_raw0 |> 
  dplyr::filter(location_category == "LOI") |> 
  dplyr::group_by(region = region30, region_desc = region30_desc, location_category0, location_category) |> 
  dplyr::summarise(#geometry = geometry, 
    X = mean(X), 
    Y = mean(Y), 
    id = n())

figure_print <- map_funct(table_raw, points0 = FALSE)  + 
  ggplot2::theme(legend.position = c(0.37, 0.85))  + 
  ggplot2::guides(shape = "none") + 
  # ggplot2::geom_sf(
  #   data = table_raw,
  #   mapping = aes(
  #     geometry = geometry, # st_jitter(geometry),
  #     ),
  #   fill = "grey20",
  #   # color = "transparent", 
  #   alpha = .5,
  #   # vjust = -.7,
  #   size = 5,
  #   show.legend = FALSE
  # ) +
  ggplot2::geom_sf_label(
    data = table_raw,
    mapping = aes(
      geometry = geometry, # st_jitter(geometry),
      label = id
    ),
    size = 5, 
    label.r = unit(0, "lines"), 
    border.color = "grey80", 
    # color = "grey80",
    vjust = -.25,
    show.legend = FALSE
  ) +
  ggplot2::coord_sf(xlim = c(min(table_raw$X)-25000, max(table_raw$X)+25000),
                    ylim = c(min(table_raw$Y)-25000, max(table_raw$Y)+25000))

figure_print <- ggdraw() +
  draw_plot(figure_print) +
  draw_plot(inset_map, x = 0.675, y = 0.75, width = 0.25, height = 0.25)

nickname <- paste0(nickname0, "LOIonly-grouped30")
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

ggplot2::ggsave( # save your plot
  path = "./output/",
  dpi = 1200,
  bg = "transparent",
  filename = paste0(nickname, ".png"), # Always save in pdf so you can make last minute edits in adobe acrobat!
  plot = figure_print, # call the plot you are saving
  width = width0, 
  height = height0, 
  units = "in") #recall, A4 pages are 8.5 x 11 in - 1 in margins

