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

## oralhistory_orig ----------------------------------------------------

# oralhistory_ref <- oral_histories_original0 |> 
#   dplyr::select(source, date, link, id, word_count, weighted_percent, percent_category) |> 
#   dplyr::distinct() 
# write_csv(x = oralhistory_ref, file = "data/oralhistory_ref.csv", col_names = TRUE)

oralhistory_orig <- oral_histories_original0 |>
  dplyr::mutate(interviewee = source, 
                interviewee = gsub(",", " ", interviewee), 
                interviewee = stringr::str_to_title(interviewee))  |> 
  tidyr::separate(interviewee, into = c("last", "first"), #remove = FALSE, 
                  sep = " ", extra = "drop", fill = "right")  |> 
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
                ) )  #|> 
  # dplyr::select(-id, -first)

oralhistory_orig <- oralhistory_orig |> 
  dplyr::bind_cols(st_coordinates(oralhistory_orig))

write.csv(x = oralhistory_orig, file = paste0("output/oralhistory_orig.csv"))

oralhistory_ref <- oralhistory_ref_edited0  |> 
  dplyr::select(-word_count, -weighted_percent, -id, #-source, 
                # -indigenous_status, -fishing_experience, 
                -date, -link, -percent_category) |> 
  dplyr::mutate(#id = factor(id, ordered = TRUE), 
    source_fl = gsub(pattern = ",", replacement = "", x = source_fl),
    # indigenous00 = ifelse(indigenous == "Non-indigenous", "Non-indigenous", "Indigenous"), 
    env_change_openness = dplyr::case_when(
      env_change_openness == 1 ~ "Low",
      env_change_openness == 2 ~ "Medium",
      env_change_openness == 3 ~ "High"
    ), 
    emotional_level = dplyr::case_when(
      emotional_level == 1 ~ "Low",
      emotional_level == 2 ~ "Medium",
      emotional_level == 3 ~ "High"
    ), 
    change_language = dplyr::case_when(
      change_language == 1 ~ "Low",
      change_language == 2 ~ "Medium",
      change_language == 3 ~ "High"
    ), 
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
                             ordered = TRUE) ) |> 
  tidyr::separate(source_fl, into = c("first", "last"), #remove = FALSE, 
                  sep = " ", extra = "drop", fill = "right") |> 
  # dplyr::select(-first) |> 
  dplyr::left_join(oralhistory_orig |> 
                     dplyr::filter(location_category == "LOI"))
write.csv(x = oralhistory_ref, file = paste0("output/oralhistory_ref.csv"))




table_raw0 <- dplyr::bind_rows(
  autocoded_themes_transcripts0 |> 
    dplyr::mutate(method = "Transcript") |>
    # head() |> 
    dplyr::mutate(codes = gsub(pattern = "Codes\\\\transcripts\\\\",
                               replacement = "", x = codes, fixed = TRUE)), 
  autocoded_themes_notes0 |> 
    # head() |> 
    dplyr::mutate(method = "Notes") |>
    dplyr::mutate(codes = gsub(pattern = "Codes\\\\notes_themes\\\\",
                               replacement = "", x = codes, fixed = TRUE)) ) |> 
  tidyr::separate(codes, into = c("theme", "node"), sep = "\\\\") |> 
  # dplyr::select(Theme = theme, node, number_of_coding_references, method) |> 
  dplyr::ungroup()
write.csv(x = table_raw0, file = paste0("output/autocoded_by_theme.csv"))


table_raw0 <- dplyr::bind_rows(
  code_references_notes0 |> 
    rename_with(~ gsub("^[a-c]_", "", .x)) |> 
    dplyr::rename(interviewee = x1) |> 
    dplyr::mutate(interviewee = gsub("^\\d+ : ", "", interviewee), 
                  interviewee = gsub(pattern = "_Deedee_ ", replacement = "", x = interviewee), 
                  interviewee = gsub(pattern = ",", replacement = "", x = interviewee), 
                  interviewee = gsub(pattern = " and", replacement = "", x = interviewee), 
                  interviewee = stringr::str_to_title(interviewee) , 
                  method = "Notes")  |> 
    tidyr::separate(interviewee, into = c("first", "last"), #remove = FALSE, 
                    sep = " ", extra = "drop", fill = "right") |> 
    dplyr::select(-first) |>
    tidyr::pivot_longer(cols = change:net, names_to = "cat", values_to = "val"), 
  code_references_interviews0[,1:6] |> 
    dplyr::rename(interviewee = x1) |>
    dplyr::rename_with(~ gsub("^[a-z]_|_\\d+$", "", .x)) |> 
    dplyr::mutate( method = "Transcript", 
      interviewee = gsub(pattern = "_Deedee_ ", replacement = "", x = interviewee), 
      interviewee = gsub("^\\d+ : Files\\\\", "", interviewee), 
      interviewee = gsub("[^[:alnum:]]", " ", interviewee), # 1. Replace special chars with space
      interviewee = gsub("[^[:alpha:]]", " ", interviewee), 
      interviewee = trimws(interviewee), 
      interviewee = stringr::str_to_title(interviewee)
    ) |> 
    tidyr::separate(interviewee, into = c("last", "first"), #remove = FALSE, 
                    sep = " ", extra = "drop", fill = "right") |> 
    dplyr::select(-first) |> 
    dplyr::relocate(last, method)  |>
  tidyr::pivot_longer(cols = boat:salmon, names_to = "cat", values_to = "val") ) |>
  dplyr::arrange((last)) |> 
  dplyr::rename(Method = method, 
                Count = val, 
                Theme = cat, 
                Interviewee = last)  |>
  dplyr::mutate(Count = replace_na(Count, 0), 
                Theme = stringr::str_to_title(Theme), 
                Interviewee = factor(Interviewee, levels = rev(unique(Interviewee))))
write.csv(x = table_raw0, file = paste0("output/autocoded_theme_node_by_interview.csv"))



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
  
  table_raw <- row_percent_interviews0 
  names(table_raw)[2:ncol(table_raw)] <- lapply(X = strsplit(x = names(table_raw)[2:ncol(table_raw)], split = "_"), '[[', 2)
  
  table_raw0 <- table_raw <- table_raw |> 
    # dplyr::mutate(
    #   id = as.numeric(trimws(substr(x = x1, start = 1, stop = 2)))  ) |> 
    # dplyr::select(-x1) |> 
    # dplyr::left_join(locations0 |> 
    #                    dplyr::filter(location_category == "LOI") |>
    #                    dplyr::mutate(id = as.numeric(paste0(id))) |> 
    #                    dplyr::select(id, name = source, 
    #                                  indigenous, indigenous00, 
    #                                  fishing_experience, collection, demographic, 
    #                                  region30, region30_desc, region100, region100_desc, 
    #                                  env_change_openness, emotional_level, change_language)) |>
    tidyr::pivot_longer(cols = boat:salmon, names_to = "Theme", values_to = "Percent") |> 
    dplyr::mutate(Theme = stringr::str_to_title(Theme), #, 
                  interviewee = gsub("^\\d+ : Files\\\\", "", x1), 
                  
                  Percent = as.numeric(gsub(pattern = "%", replacement = "", x = Percent))) |>
    tidyr::separate(interviewee, into = c("last", "first"), #remove = FALSE, 
                    sep = "_", extra = "drop", fill = "right") 
    
  write.csv(x = table_raw0, file = paste0("output/percent.csv"))
  
