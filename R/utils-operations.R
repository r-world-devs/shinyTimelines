rename_df_columns <- function(df_to_rename, old_names_vector, new_names_vector) {
  new_names <- stats::setNames(old_names_vector, new_names_vector)
  df_to_rename <- df_to_rename %>%
    dplyr::select(dplyr::any_of(new_names))

  return(df_to_rename)
}

map_rename <- function(df, name, case, mapping) {
  rename_df_columns(
    df,
    as.vector(unlist(mapping[[case]][[name]])),
    names(mapping[[case]][[name]])
  )
}

color_palette <- list(
  bootstrap = c(
    primary = '#3c8dbc'
  ),
  primary = c(
    blue = "#0066CC",
    white = "#FFFFFF",
    grey = "#B1B3B3",
    black = "#000000"),
  secondary = c(
    blue = "#0066CC",
    red = "#E40046",
    violet = "#A05EB5",
    green = "#00965E",
    yellow = "#FFC72C",
    orange = "#ED8B00",
    cyan = "#00E5EF"
  ),
  shades = list(
    blues = c("#00346a", "#004c9e", "#51a2e5", "#c9dff6"),
    reds = c("#7d0020", "#ba0031", "#ff6696", "#ffa6c1"),
    violets = c("#552b5e", "#80428b", "#d1a4d9", "#e4c7e8"),
    greens = c("#004d2a", "#007342", "#4cc8a4", "#9ddec6"),
    yellows = c("#856200", "#c49300", "#ffde7e", "#ffebb1"),
    oranges = c("#7f4400", "#bd6400", "#ffbc62", "#fed6a4"),
    grayscale = c(
      "#000000", "#161616", "#2d2d2d", "#464646", "#5d5d5d", "#747474",
      "#8a8a8a", "#a1a1a1", "#b8b8b8", "#d1d1d1", "#e8e8e8", "#ffffff"
    )
  )
)

reformat_palette <- function(color_palette) {
  palette <- unlist(
    color_palette$shades["grayscale" != names(color_palette$shades)]
  )
  palette <- c(
    palette[seq(3, 6 * 4, by = 4)],
    palette[seq(4, 6 * 4, by = 4)],
    palette[seq(2, 6 * 4, by = 4)],
    palette[seq(1, 6 * 4, by = 4)]
  )
  palette <- unname(palette)

  return(palette)
}

timelines_palette <- reformat_palette(color_palette)
