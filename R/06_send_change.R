# Optional geographic change analysis -------------------------------------
#
# Describes annual change in the local-authority distribution of recorded
# SEND profiles. This is supplementary and is not required to reproduce the
# main regression tables. Geography files are not distributed publicly.

dir.create("outputs/maps/annual_change", recursive = TRUE, showWarnings = FALSE)

Education <- read_parquet_labeled("data/Education")
england_ct_2011 <- sf::st_read(
  "geography/england_ct_2011.shp",
  quiet = TRUE
) |>
  sf::st_as_sf() |>
  janitor::clean_names() |>
  dplyr::filter(stringr::str_detect(code, "^E"))

sen_indicator <- data.table::as.data.table(Education)[
  !is.na(year) &
    !is.na(school_local_authority_9code) &
    !is.na(sen_indicator),
  .(code = school_local_authority_9code, year, sen_indicator)
]

denom <- sen_indicator[, .(n_pupils = .N), by = .(code, year)]
by_cat <- sen_indicator[, .(n_cat = .N), by = .(code, year, sen_indicator)]

sen_year <- by_cat[
  denom,
  on = .(code, year)
][
  , pct_cat := 100 * n_cat / n_pupils
][
  order(code, sen_indicator, year)
][
  , pct_change := pct_cat - data.table::shift(pct_cat),
  by = .(code, sen_indicator)
][
  !is.na(pct_change)
]

sen_year <- merge(
  england_ct_2011[c("code", "geometry")],
  sen_year,
  by = "code",
  all.y = TRUE
) |>
  sf::st_as_sf()

slugify <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

region_bounds <- list(
  england = NULL,
  london = list(xlim = c(500000, 570000), ylim = c(155000, 205000)),
  manchester = list(xlim = c(360000, 405000), ylim = c(380000, 420000)),
  birmingham = list(xlim = c(385000, 440000), ylim = c(265000, 310000))
)

make_change_map <- function(data, category, region, bounds = NULL) {
  p <- ggplot2::ggplot(data[data$sen_indicator == category, ]) +
    ggplot2::geom_sf(ggplot2::aes(fill = pct_change)) +
    ggplot2::facet_wrap(~ year) +
    ggplot2::scale_fill_gradient2(
      midpoint = 0,
      low = "blue",
      high = "red",
      limits = c(-10, 10)
    ) +
    ggplot2::labs(
      title = paste(
        "Annualised percentage change by",
        if (region == "england") "English" else tools::toTitleCase(region),
        "local authority, 2015-22"
      ),
      subtitle = as.character(category),
      fill = "Percentage change\nfrom previous year"
    ) +
    cowplot::theme_map()

  if (!is.null(bounds)) {
    p <- p + ggplot2::coord_sf(
      xlim = bounds$xlim,
      ylim = bounds$ylim,
      expand = FALSE
    )
  }
  p
}

categories <- setdiff(
  stats::na.omit(unique(sen_year$sen_indicator)),
  "No SEND"
)

for (category in categories) {
  for (region in names(region_bounds)) {
    suffix <- if (region == "england") "" else paste0("_", region)
    plot <- make_change_map(
      sen_year,
      category,
      region,
      region_bounds[[region]]
    )
    ggplot2::ggsave(
      filename = file.path(
        "outputs/maps/annual_change",
        paste0(slugify(category), suffix, ".png")
      ),
      plot = plot,
      width = 16,
      height = 9,
      dpi = 450
    )
  }
}
