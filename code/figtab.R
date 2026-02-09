
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
  
  # Spatial mapping
  "sf",
  "ggspatial", 
  "raster",
  "stars", 
  
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

## oralhistory_orig ----------------------------------------------------

# oralhistory_ref <- oral_histories_original0 |> 
#   dplyr::select(source, date, link, id, word_count, weighted_percent, percent_category) |> 
#   dplyr::distinct() 
# write_csv(x = oralhistory_ref, file = "data/oralhistory_ref.csv", col_names = TRUE)

oralhistory_ref <- oralhistory_ref_edited0  |> 
  dplyr::select(-word_count, -weighted_percent, -source, 
                -date, -link, -percent_category) |> 
  dplyr::mutate(id = factor(id, ordered = TRUE), 
                indigenous00 = ifelse(indigenous == "Non-indigenous", "Non-indigenous", "Indigenous"))

oralhistory_orig <- oral_histories_original0 |> 
  sf::st_as_sf(coords = c("longitude", "latitude"), 
               remove = FALSE,
               crs = crs_in) |>
  sf::st_transform(crs = crs_out) |> 
  dplyr::mutate(id = factor(id, ordered = TRUE), 
                location_category = ifelse(location_category == "Oregon", "LOI", location_category), 
                location_category = ifelse(objectid == 12, "Additional", location_category),
                location_category0 = dplyr::case_when(
                  location_category == "LOI" ~ "Location of Interview", 
                  location_category == "Additional" ~ "Mentioned in Interview", 
                  location_category == "Oregon" ~ "Location of Interview", 
                )) 
oralhistory_orig <- oralhistory_orig |> 
  dplyr::bind_cols(st_coordinates(oralhistory_orig))

# https://stackoverflow.com/questions/77179007/group-spatial-points-by-distance-in-r-how-to-group-cluster-spatial-points-so-gr
dist0 <- 100000 # example : 11000
adj <- sf::st_distance(oralhistory_orig) 
# Furthermore, we can turn this into a binary matrix telling us whether each pair of stations is within 11km of each other:
adj <- matrix(as.numeric(as.numeric(adj)) < dist0, nrow = nrow(adj))
# We will see if we plot this graph, there are 4 connected components, representing clusters of stations within 11km of each other:
g100 <- igraph::graph_from_adjacency_matrix(adj)
# We can get the number of these components and put these back into our original data frame:

dist0 <- 30000 # example : 11000
adj <- sf::st_distance(oralhistory_orig)
adj <- matrix(as.numeric(as.numeric(adj)) < dist0, nrow = nrow(adj))
g30 <- igraph::graph_from_adjacency_matrix(adj)

oralhistory_orig <- oralhistory_orig |> 
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
  dplyr::right_join(oralhistory_ref)


if (FALSE) { # test
  # ggplot() +  geom_sf(data = oralhistory_orig, aes(color = region)) 
  
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
      show.legend = FALSE)  +  
    ggplot2::geom_sf_label(
      data = oralhistory_orig,
      mapping = aes(label = region), 
      show.legend = FALSE)  +  
    # geom_sf(data = oralhistory_orig, aes(color = region))  +
    ggplot2::coord_sf(xlim = c(min(oralhistory_orig$X)-25000, max(oralhistory_orig$X)+25000),
                      ylim = c(min(oralhistory_orig$Y)-25000, max(oralhistory_orig$Y)+25000))
}
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
  tidyr::drop_na() |> 
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
  
  # 4. Make PCA scatter plot colored by indigenous00
  colors0 <- viridis::mako(length(unique(table_raw$var00)), direction = -1, begin = 0.2, end = .8)
  
  figure_print <- figure_print_pca <- 
    ggplot2::ggplot(table_raw, 
                    aes(x = PC1, y = PC2, color = var00, fill = var00)) +
    ggplot2::stat_ellipse(alpha = 0.1, # , color = NA
                          geom = "polygon") +  # semi-transparent cloud
    ggplot2::geom_point(size = 3, 
                        alpha = 0.8) +
    ggplot2::theme_minimal()  + 
    # see::scale_fill_oi(name = x_name0,palette = "black_first") + 
    # see::scale_color_oi(name = x_name0,palette = "black_first") +
    ggplot2::scale_fill_manual(name = x_name0,
                               values = colors0) +
    ggplot2::scale_color_manual(name = x_name0,
                                values = colors0) +
    ggplot2::labs(
      x = paste0("PC1 (", round(summary(pca_res)$importance[2,1]*100, 1), "%)"),
      y = paste0("PC2 (", round(summary(pca_res)$importance[2,2]*100, 1), "%)")#,
      # title = "PCA of Frequencies by ID"
    ) +
    theme_custom() 
  nickname <- paste0(nickname0, var000, "-pca")
  save_figures(figure_print = figure_print, table_raw = pca_res, nickname = nickname, width = width0, height = height0)
  
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
  
  ## Anova -----------------------------------------
  table_aov <- car::Anova(aov(freq ~ fishing_experience + cat, data = table_raw0))
  
  ## Linear mixed-effects model -------------------------------
  table_lm <- table_raw0 |> 
    dplyr::rename(var00 = {{var000}}) |> 
    dplyr::mutate(id = as.numeric(paste0(id)))
  # table_lm <- lm(freq ~ var00 + cat + (1 | id), data = table_lm)
  table_lm <- lme4::lmer(freq ~ var00 + cat + (1 | id), data = table_lm)
  # + (1 | id) ==> Each person gets their own baseline level of freq.
  
  # table_lm <- lm0 %>% summary() %>% print(digits = 8) # show summary output
  
  table_lm_comb <- c(table_lm_comb, table_lm)
  names(table_lm_comb)[length(table_lm_comb)] <- x_name0
  
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
    ggplot2::scale_fill_viridis_d(name = "", option = "E", begin = .2, end = .8) +
    ggplot2::scale_color_viridis_d(name = "", option = "E", begin = .2, end = .8) +
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


# Prep figures -----------------------------------------------------------------

## NOTES ------

# Color choices begin = 0.2, end = 0.8
### G/mako - Themes
### E/cviridis - Counts/Frequencies

expand_custom <- c(.005, .005)

## Figure 1: Hierarchical plot of themes -----------------------------------------------

# TOLDEO - NOT POSSIBLE TO COMPLETE

nickname0 <- "fig-1-hierarchical-themes-auto-"

## Figure 2: bar chart of themes -----------------------------------------------

nickname0 <- "fig-2-bar-themes"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw <- code_references_interviews0[,1:6] 
names(table_raw)[2:ncol(table_raw)] <- lapply(X = strsplit(x = names(table_raw)[2:ncol(table_raw)], split = "_"), '[[', 2)

table_raw0 <- table_raw <- table_raw |> 
  dplyr::mutate(
    id = as.numeric(trimws(substr(x = x1, start = 1, stop = 2)))# , 
    # name = substr(x = x1, start = 12, stop = nchar(x1)), 
    # name = gsub(pattern = "\\\\", replacement = "", x = name), 
    # name = gsub(pattern = "_", replacement = " ", x = name), 
    # name = gsub(pattern = "-", replacement = " ", x = name), 
    # name = gsub(pattern = "[0-9]+", replacement = "", x = name), 
    # name = stringr::str_to_title(name)
  ) |> 
  dplyr::select(-x1) |> 
  dplyr::left_join(oralhistory_orig |> 
                     dplyr::filter(location_category == "LOI") |>
                     dplyr::mutate(id = as.numeric(paste0(id))) |> 
                     dplyr::select(id, name = source, 
                                   indigenous, indigenous00, 
                                   fishing_experience, collection, demographic, 
                                   region30, region30_desc, region100, region100_desc, 
                                   change_score_language, emotional_score)) |>
  tidyr::pivot_longer(cols = boat:salmon, names_to = "cat", values_to = "freq") |> 
  dplyr::mutate(cat = stringr::str_to_title(cat), , 
                id = factor(id, ordered = TRUE))

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
    mapping = aes(x = id, y = freq, fill = cat)) +
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
    mapping = aes(x = id, y = freq, fill = cat)) +
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

### pca/lm/Stacked by [var] -----------------------------------------

# Does frequency differ by region and by category?

temp <- plot_lm_pca_stacked(
  table_raw0 = table_raw0, 
  var00 = c("region100_desc","fishing_experience", "collection", "indigenous00"),
  x_name = c("Region", "Fishing Experience", "Oral History Collection", "Indigenous Status"),
  nickname0 = nickname0) 

a <- temp$table_lm_comb
anova(a$Region, a$`Fishing Experience`, a$`Oral History Collection`, a$`Indigenous Status`)

# Data: table_lm
# Models:
#   a$`Indigenous Status`: freq ~ var00 + cat + (1 | id)
# a$`Fishing Experience`: freq ~ var00 + cat + (1 | id)
# a$Region: freq ~ var00 + cat + (1 | id)
# a$`Oral History Collection`: freq ~ var00 + cat + (1 | id)
# npar    AIC    BIC  logLik -2*log(L)  Chisq Df Pr(>Chisq)
# a$`Indigenous Status`          8 593.68 615.98 -288.84    577.68                     
# a$`Fishing Experience`         9 596.15 621.24 -289.08    578.15 0.0000  1     1.0000
# a$Region                      10 597.11 624.99 -288.56    577.11 1.0411  1     0.3076
# a$`Oral History Collection`   10 596.77 624.64 -288.38    576.77 0.3438  0

# The model including Indigenous status provides the best overall fit to the data (lowest AIC), suggesting it explains slightly more variation in how frequently different fishing-related categories are mentioned than the other grouping variables. Adding fishing experience, region, or oral history collection does not significantly improve model fit relative to the Indigenous-status model (likelihood ratio tests all non-significant, p ≥ 0.31). This indicates that, after accounting for the type of fishing-related category and repeated observations within individuals, differences in mention frequency are more strongly structured by Indigenous status than by region, fishing experience, or collection source, and that these latter factors do not contribute substantial additional explanatory power for Boat, Crab, Fishing, Net, and Salmon references.

## Figure 3: Hierarchical plot of themes -----------------------------------------------
#NOT POSSIBLE TO RECREATE, this is a plot of what we've got

nickname0 <- "fig-3-hierarchical-themes-manualcode-"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw <- themes_notesonly0[,-1]
names(table_raw) <- substr(x = names(table_raw),start = 3, stop = nchar(names(table_raw)))

table_raw0 <- table_raw <- table_raw |>
  # dplyr::select(-x1) |> 
  tidyr::pivot_longer(names_to = "var", values_to = "val", cols = names(table_raw)) |> 
  dplyr::mutate(var = gsub(x = var, pattern = "_", replacement = " "), 
                var = stringr::str_to_title(var))

### Bar plot of theme ------------------
colors0 <- viridis::mako(length(unique(table_raw$var)), direction = -1, begin = 0.2, end = .8)
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = var, y = val, fill = var)) +
  ggplot2::geom_bar(position="dodge", stat="identity", width = 0.5) +
  ggplot2::scale_fill_manual(name = "Co-occured Theme", # "Temperature",
                             values = colors0, 
                             expand = expand_custom) +
  ggplot2::scale_y_continuous(name = "Coded Themes from Research Notes", 
                              breaks = scales::pretty_breaks(), 
                              expand = expand_custom) +
  ggplot2::scale_x_discrete(name = "Themes") +
  theme_custom() 
nickname <- paste0(nickname0)
save_figures(figure_print = figure_print, table_raw = table_raw, nickname = nickname, width = width0, height = height0)

## Figure 4: bar chart of themes -----------------------------------------------

nickname0 <- "fig-4-bar-themes-manualcode-"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw <- co_occurrence_interviews0
names(table_raw)[2:ncol(table_raw)] <- unlist(lapply(X = strsplit(x = names(table_raw)[2:ncol(table_raw)], split = "_"), '[[', 2))

# make unique entries - crab x boat is the same as boat x crab
comb <- combn(names(table_raw)[2:ncol(table_raw)], 2, simplify = FALSE)
comb <- data.frame(matrix(data = unlist(comb), ncol = 2, byrow = TRUE))
names(comb) <- c("var", "var1")

table_raw0 <- table_raw <- table_raw |> 
  dplyr::mutate(
    x1 = gsub(pattern = "[0-9]+ : ", replacement = "", x = x1), 
    dplyr::across(everything(), ~na_if(., "NULL"))
  ) |> 
  tidyr::pivot_longer(cols = boat:salmon, names_to = "var", values_to = "val") |> 
  dplyr::distinct() |> 
  dplyr::rename(var1 = x1) |> 
  dplyr::filter(!is.na(val)) |>
  dplyr::mutate(val = as.numeric(val)) |>
  dplyr::right_join(comb) |> 
  dplyr::mutate(var = stringr::str_to_title(var), 
                var1 = stringr::str_to_title(var1))

### Grouped bar plot by co-theme ------------------
colors0 <- viridis::mako(length(unique(table_raw$var)), direction = -1, begin = 0.2, end = .8)
figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    mapping = aes(x = var, y = val, fill = var1)) +
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
             viridis::cividis(n = length(1:max(table_raw$val)), 
                              begin = 0.2, end = .8, direction = -1))
# breaks = min(table_raw):max(table_raw), 

figure_print <- 
  ggplot2::ggplot(
    data = table_raw, 
    # data = table_raw |> dplyr::mutate(val = ifelse(val == 0, NA, val)), 
    mapping = aes(x = var, y = var1, fill = val)) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(aes(label = val)) + 
  ggplot2::scale_fill_continuous(
    name = "Co-occurrences Count",
    palette = colors0, 
    breaks = 0:max(table_raw$val)
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
  dplyr::select(var1, var, val)

# nodes with size = total val (node strength)
nodes <- table_raw |>
  tidyr::pivot_longer(cols = c(var1, var), values_to = "node") |>
  dplyr::group_by(node) |>
  dplyr::summarise(size = sum(val), .groups = "drop")

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

## Figure 4b: bar chart of themes -----------------------------------------------

nickname0 <- "fig-4b-themes-manualcode-"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw <- row_percent_interviews0 
names(table_raw)[2:ncol(table_raw)] <- lapply(X = strsplit(x = names(table_raw)[2:ncol(table_raw)], split = "_"), '[[', 2)

table_raw0 <- table_raw <- table_raw |> 
  dplyr::mutate(
    id = as.numeric(trimws(substr(x = x1, start = 1, stop = 2)))  ) |> 
  dplyr::select(-x1) |> 
  dplyr::left_join(oralhistory_orig |> 
                     dplyr::filter(location_category == "LOI") |>
                     dplyr::mutate(id = as.numeric(paste0(id))) |> 
                     dplyr::select(id, name = source, 
                                   indigenous, indigenous00, 
                                   fishing_experience, collection, demographic, 
                                   region30, region30_desc, region100, region100_desc, 
                                   change_score_language, emotional_score)) |>
  tidyr::pivot_longer(cols = boat:salmon, names_to = "cat", values_to = "freq") |> 
  dplyr::mutate(cat = stringr::str_to_title(cat), , 
                id = factor(id, ordered = TRUE), 
                freq = as.numeric(gsub(pattern = "%", replacement = "", x = freq)))


### pca/lm/Stacked by [var] -----------------------------------------

# Does frequency differ by region and by category?

temp <- plot_lm_pca_stacked(
  table_raw0 = table_raw0, 
  var00 = c("region100_desc","fishing_experience", "collection", "indigenous00"),
  x_name = c("Region", "Fishing Experience", "Oral History Collection", "Indigenous Status"),
  nickname0 = nickname0) 

a <- temp$table_lm_comb
anova(a$Region, a$`Fishing Experience`, a$`Oral History Collection`, a$`Indigenous Status`)
# > unique(table_raw0$cat)
# [1] "Boat"    "Crab"    "Fishing" "Net"     "Salmon" 

# refitting model(s) with ML (instead of REML)
# Data: table_lm
# Models:
#   a$`Indigenous Status`: freq ~ var00 + cat + (1 | id)
# a$`Fishing Experience`: freq ~ var00 + cat + (1 | id)
# a$Region: freq ~ var00 + cat + (1 | id)
# a$`Oral History Collection`: freq ~ var00 + cat + (1 | id)
# npar    AIC    BIC  logLik -2*log(L) Chisq Df Pr(>Chisq)
# a$`Indigenous Status`          8 1073.5 1095.8 -528.77    1057.5                    
# a$`Fishing Experience`         9 1075.5 1100.6 -528.77    1057.5     0  1     1.0000
# a$Region                      10 1077.5 1105.4 -528.77    1057.5     0  1     0.9998
# a$`Oral History Collection`   10 1077.5 1105.4 -528.77    1057.5     0  0   

# We compared linear mixed-effects models predicting the frequency of mentions of Boat, Crab, Fishing, Net, and Salmon, including a random intercept for individual and fixed effects for category and respondent-level characteristics (Indigenous status, fishing experience, region, or oral history collection). Model comparisons using maximum likelihood indicated that all four models fit the data equivalently, with identical log-likelihoods and non-significant likelihood-ratio tests (p ≈ 1), and minor differences in AIC/BIC driven only by the number of parameters. These results indicate that, after accounting for category and individual-level variation, variation in term frequency is not meaningfully explained by respondent experience, region, or collection type, and is only slightly structured by Indigenous status. Consequently, individual differences and the category of fishing-related term largely drive patterns of mention frequency.

# Across models predicting the frequency of Boat, Crab, Fishing, Net, and Salmon mentions (with a random intercept for individual), the Indigenous Status model shows the best fit (lowest AIC), indicating it explains slightly more variation than the alternatives. Adding Fishing Experience, Region, or Oral History Collection does not significantly improve model fit (all likelihood‐ratio tests non-significant), suggesting these factors do not add explanatory power beyond Indigenous status for patterns of fishing-related references. Overall, variation in how often these fishing terms are mentioned appears to be structured more by Indigenous status than by regional context, experience category, or collection source.

# The model including Indigenous status provides the best overall fit to the data (lowest AIC), suggesting it explains slightly more variation in how frequently different fishing-related categories are mentioned than the other grouping variables. Adding fishing experience, region, or oral history collection does not significantly improve model fit relative to the Indigenous-status model (likelihood ratio tests all non-significant, p ≥ 0.31). This indicates that, after accounting for the type of fishing-related category and repeated observations within individuals, differences in mention frequency are more strongly structured by Indigenous status than by region, fishing experience, or collection source, and that these latter factors do not contribute substantial additional explanatory power for Boat, Crab, Fishing, Net, and Salmon references.


## Figure 5: bar chart of themes -----------------------------------------------

nickname0 <- "fig-5-sentiment-heatmap-"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw <- sentiment_raw0
names(table_raw)[2:ncol(table_raw)] <- substr(x = names(table_raw)[2:ncol(table_raw)],start = 3, stop = nchar(names(table_raw)[2:ncol(table_raw)]))

table_raw <- table_raw0 <- table_raw |> 
  dplyr::mutate(
    id = as.numeric(trimws(substr(x = x1, start = 1, stop = 2))) ) |> 
  dplyr::left_join(oralhistory_orig |>
                     dplyr::mutate(id = as.numeric(paste0(id))) |> 
                     dplyr::filter(location_category == "LOI") |> 
                     dplyr::select(id, name = source, indigenous, indigenous00, 
                                   fishing_experience, collection, demographic, 
                                   region30, region30_desc, region100, region100_desc, 
                                   change_score_language, emotional_score)) |>
  dplyr::select(-x1) |> 
  tidyr::pivot_longer(cols = very_negative:very_positive, names_to = "cat", values_to = "freq") |> 
  dplyr::mutate(cat = stringr::str_to_title(cat), 
                cat = gsub(pattern = "_", replacement = " ", x = cat)) 

### Tile map -------------------------------------------------------------------
plot_tileheat(table_raw0 = table_raw0, nickname0 = nickname0, facet_var = NULL)

### Tile facet by indigenous00 -------------------------------------------------
plot_tileheat(table_raw0 = table_raw0, nickname0 = nickname0, facet_var = "indigenous00")

### Tile facet by oral history collection --------------------------------------
plot_tileheat(table_raw0 = table_raw0, nickname0 = nickname0, facet_var = "collection")

### Tile facet fishing experience  ---------------------------------------------
plot_tileheat(table_raw0 = table_raw0, nickname0 = nickname0, facet_var = "fishing_experience")

### pca/lm/Stacked by [var] -----------------------------------------

# Does frequency differ by region and by category?

temp <- plot_lm_pca_stacked(
  table_raw0 = table_raw0, 
  var00 = c("region100_desc","fishing_experience", "collection", "indigenous00"),
  x_name = c("Region", "Fishing Experience", "Oral History Collection", "Indigenous Status"),
  nickname0 = nickname0) 

# None of these alternative ways of representing respondent characteristics (region, fishing experience, collection, Indigenous identity) meaningfully improve model fit beyond category and individual-level random effects.
# Likelihood-ratio tests indicated no improvement in model fit when alternative respondent-level predictors were included (all p > 0.4), supporting a parsimonious model including category and a random intercept for respondent.

a <- temp$table_lm_comb
anova(a$Region, a$`Fishing Experience`, a$`Oral History Collection`, a$`Indigenous Status`)

# ## Figure 6: Hierarchical plot of auto themes -----------------------------------------------
# #NOT DONE
# 
# nickname0 <- "fig-6-hierarchical-themes-autocode-"
# height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
# width0 <- full_page_portrait_width
# 
# table_raw <- sentiment_raw0 
# names(table_raw)[2:ncol(table_raw)] <- substr(x = names(table_raw)[2:ncol(table_raw)],start = 3, stop = nchar(names(table_raw)[2:ncol(table_raw)]))
# 
# table_raw0 <- table_raw <- table_raw |> 
#   dplyr::mutate(
#     id = as.numeric(trimws(substr(x = x1, start = 1, stop = 2))) # , 
#     # name = substr(x = x1, start = 12, stop = nchar(x1)), 
#     # name = gsub(pattern = "\\\\", replacement = "", x = name), 
#     # name = gsub(pattern = "_", replacement = " ", x = name), 
#     # name = gsub(pattern = "-", replacement = " ", x = name), 
#     # name = gsub(pattern = "[0-9]+", replacement = "", x = name), 
#     # name = stringr::str_to_title(name)
#   ) |> 
#   dplyr::left_join(oralhistory_ref |>
#                      dplyr::select(id, name = source, indigenous, fishing_experience, collection, demographic, indigenous00)) |>
#   dplyr::select(-x1) |>
#   tidyr::pivot_longer(cols = very_negative:very_positive, names_to = "cat", values_to = "freq") |> 
#   dplyr::mutate(cat = stringr::str_to_sentence(cat))
# 
# colors0 <- viridis::mako(length(unique(table_raw$cat)), direction = -1, begin = 0.2, end = .8)
# 
### Grouped bar plot by narrator name with grouping rectangle ------------------
# figure_print <- ""


# Figure 7: Map of Oral Histories -----------------------------------------------

nickname0 <- "fig-7-map-"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw0 <- table_raw <- oralhistory_orig 

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

### Map of only LOI -------------------------------------------

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

nickname <- paste0(nickname0, "LOIonly-grouped")
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


# Figure 8: bar chart of themes -----------------------------------------------

nickname0 <- "fig-8-scores-"
height0 <- 6 # ifelse(srvy == "NEBS", full_page_portrait_height, 6)
width0 <- full_page_portrait_width

table_raw <- table_raw0 <- 
  oralhistory_orig |>
                     dplyr::mutate(id = as.numeric(paste0(id))) |> 
                     dplyr::filter(location_category == "LOI") |> 
                     dplyr::select(id, name = source, indigenous, indigenous00, 
                                   fishing_experience, collection, demographic, 
                                   region30, region30_desc, region100, region100_desc, 
                                   change_score_language, emotional_score, change_score)  |>
  tidyr::pivot_longer(cols = change_score_language:change_score, names_to = "cat", values_to = "freq") |> 
  dplyr::mutate(cat = stringr::str_to_title(cat), 
                cat = gsub(pattern = "_", replacement = " ", x = cat)) 


### pca/lm/Stacked by [var] for change_score_language -----------------------------------------

temp <- plot_lm_pca_stacked(
  table_raw0 = table_raw0, 
  var00 = c("region100_desc","fishing_experience", "collection", "indigenous00"),
  x_name = c("Region", "Fishing Experience", "Oral History Collection", "Indigenous Status"),
  nickname0 = nickname0) 

a <- temp$table_lm_comb
anova(a$Region, a$`Fishing Experience`, a$`Oral History Collection`, a$`Indigenous Status`)

# refitting model(s) with ML (instead of REML)
# Data: table_lm
# Models:
#   a$`Indigenous Status`: freq ~ var00 + cat + (1 | id)
# a$Region: freq ~ var00 + cat + (1 | id)
# a$`Fishing Experience`: freq ~ var00 + cat + (1 | id)
# a$`Oral History Collection`: freq ~ var00 + cat + (1 | id)
# npar    AIC    BIC  logLik -2*log(L)  Chisq Df Pr(>Chisq)
# a$`Indigenous Status`          6 180.90 190.88 -84.450    168.90                     
# a$Region                       7 183.88 195.53 -84.940    169.88 0.0000  1          1
# a$`Fishing Experience`         7 187.54 199.18 -86.768    173.54 0.0000  0           
# a$`Oral History Collection`    7 183.88 195.53 -84.940    169.88 3.6568  0 

# Model comparisons indicate that variation in frequency is best explained by change-related and emotional scoring variables, while respondent characteristics such as Indigenous status, region, fishing experience, and oral history collection do not meaningfully improve model fit. This suggests that frequency is structured by how individuals perceive and articulate change, rather than by demographic or positional attributes.