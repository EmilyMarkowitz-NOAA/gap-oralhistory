### Bar plot of theme ------------------
table_raw <- themes_notesonly0[,-1]
names(table_raw) <- substr(x = names(table_raw),start = 3, stop = nchar(names(table_raw)))

table_raw0 <- table_raw <- table_raw |>
  # dplyr::select(-x1) |>
  tidyr::pivot_longer(names_to = "var", values_to = "val", cols = names(table_raw)) |>
  dplyr::mutate(var = gsub(x = var, pattern = "_", replacement = " "),
                var = stringr::str_to_title(var))

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


### Bar plot of theme ------------------
table_raw <- themes_notesonly0[,-1]
names(table_raw) <- substr(x = names(table_raw),start = 3, stop = nchar(names(table_raw)))

table_raw0 <- table_raw <- table_raw |>
  # dplyr::select(-x1) |> 
  tidyr::pivot_longer(names_to = "var", values_to = "val", cols = names(table_raw)) |> 
  dplyr::mutate(var = gsub(x = var, pattern = "_", replacement = " "), 
                var = stringr::str_to_title(var))

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





