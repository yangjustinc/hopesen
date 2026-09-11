# Optional spatial analysis -------------------------------------------------
#
# Calculates global and local spatial autocorrelation of SEND-profile
# prevalence by school local authority. Geography files are not distributed
# with the repository and must be supplied inside the SRS. This stage is
# intentionally excluded from the default runner.

## Load shapefile =====
dir.create("outputs/maps/lisa", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

Spine <- read_parquet_labeled("data/Spine")
Education <- read_parquet_labeled("data/Education")

England_CT_2011 <-
  st_read("geography/england_ct_2011.shp", quiet = TRUE) |>
  st_as_sf() |>
  janitor::clean_names() |>
  filter(str_detect(code, "^E")) |>
  setDT()
gc()

## Spatial spine ====
Spatial_Spine <- setDT(Education)[!is.na(year) &
                                    !is.na(school_local_authority_9code),
                                  .(pupil_matching_ref_anonymous,
                                    year,
                                    code = school_local_authority_9code)]

gc()

# Maps
## Build continguity weights
England_CT_2011 <- st_as_sf(England_CT_2011)
la_sf <-
  England_CT_2011[!duplicated(England_CT_2011$code), c("code", "geometry")]
nb <- poly2nb(la_sf, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
la_index <- la_sf$code

## Calculate n and N per year and sen indicator
la_year_total <-
  setDT(Spatial_Spine)[, .(n_total = .N), by = .(code, year)]
la_year_sen_counts <-
  setDT(Spatial_Spine)[setDT(Spine)[, .(pupil_matching_ref_anonymous, year, sen_indicator)], , on = c("pupil_matching_ref_anonymous", "year")][sen_indicator != "No SEND", .(n_sen = .N), by = .(code, year, sen_indicator)]
all_years <- sort(unique(la_year_total$year))
all_sen <-  sort(unique(la_year_sen_counts$sen_indicator))
grid <-
  CJ(
    code = la_index,
    year = all_years,
    sen_indicator = all_sen,
    unique = TRUE
  )
la_year_complete <- grid[la_year_sen_counts,
                         on = .(code, year, sen_indicator)][la_year_total,
                                                            on = .(code, year)]
la_year_complete[is.na(n_sen), n_sen := 0L]
la_year_complete[, prevalence := n_sen / n_total]

## Calculate Moran's I
results <- la_year_complete[, {
  x <- prevalence[match(la_index, code)]
  keep <- is.finite(x)
  x_sub <- x[keep]
  lw_sub <- spdep::subset.listw(lw, keep, zero.policy = TRUE)

  if (length(x_sub) < 3L || isTRUE(all(x_sub == x_sub[1]))) {
    .(
      moran_I = NA_real_,
      p_value = NA_real_,
      n_LA = length(x_sub),
      var_x = var(x_sub)
    )
  } else {
    mor <- spdep::moran.test(x_sub, lw_sub, zero.policy = TRUE)
    .(
      moran_I = unname(mor$estimate["Moran I statistic"]),
      p_value = mor$p.value,
      n_LA = length(x_sub),
      var_x = var(x_sub)
    )
  }
},
by = .(year, sen_indicator)]

### Moran's I by year
moran_i <-
  ggplot(results, aes(x = year, y = moran_I, colour = sen_indicator)) +
  geom_line() +
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  geom_point() +
  labs(
    title = "Global Moran's I by Year and SEND Type, 2015-22",
    subtitle = "",
    colour = "SEND type",
    x = "Year",
    y = "Global Moran's I"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/moran.svg",
  plot = moran_i,
  width = 16,
  height = 9,
  device = "svg"
)

### LISA plots
all_sen <- sort(unique(la_year_complete$sen_indicator))
all_sen <- setdiff(all_sen, "No SEND")
all_years <- sort(unique(la_year_complete$year))
grid <- CJ(year = all_years, sen_indicator = all_sen, unique = TRUE)

lisa <- function(y, s){
  dt <- la_year_complete[year == y & sen_indicator == s, .(code, prevalence)]

  x <- dt$prevalence[match(la_index, dt$code)]
  x[is.na(x)] <- 0

  keep <- is.finite(x)
  x_sub <- x[keep]

  if (length(x_sub) <3L || all(x_sub == x_sub[1])) {
    out <- data.table(code = la_index, year = y, sen_indicator = s,
                      cluster = "Not significant", p_value = NA_real_, local_I = NA_real_)
    return(out)
  }

  lw_sub <- subset.listw(lw, keep, zero.policy = TRUE)
  lm <- localmoran(x_sub, lw_sub, zero.policy = TRUE)

  p <- lm[, 5]
  z <- as.numeric(scale(x_sub))
  lz <- lag.listw(lw_sub, z, zero.policy = TRUE)

  sig <- p <= 0.05
  cl <- ifelse(!sig, "Not significant",
               ifelse(z >=0 & lz >= 0, "High-High",
                      ifelse(z <= 0 & lz <= 0, "Low-Low",
                             ifelse(z >= 0 & lz <= 0, "High-Low", "Low-High"))))

  out <- data.table(code = la_index, year = y, sen_indicator = s,
                    cluster = NA_character_, p_value = NA_real_, local_I = NA_real_)
  idx <- which(keep)
  out$cluster[idx] <- cl
  out$p_value[idx] <- p
  out$local_I[idx] <- lm[, 1]

  out[]
}

lisa_dt <- data.table::rbindlist(
  lapply(seq_len(nrow(grid)), function(i) lisa(grid$year[i], grid$sen_indicator[i])),
  use.names = TRUE
)

lisa_sf <- merge(la_sf, lisa_dt, by = "code", all.y = TRUE)

lisa_sf$cluster <- factor(
  lisa_sf$cluster,
  levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not significant")
)

slugify <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

#### England
for (s in all_sen) {
  p <- ggplot(lisa_sf[lisa_sf$sen_indicator == s, ]) +
    geom_sf(aes(fill = cluster), colour = "black", size = 0.05) +
    facet_wrap(~ year) +
    scale_fill_manual(
      values = c(
        "High-High" = "darkred",
        "Low-Low" = "darkblue",
        "High-Low" = "pink",
        "Low-High" = "lightblue",
        "Not significant" = "white"
      ),
      na.value = "grey80",
      drop = FALSE
    ) +
    labs(
      title = paste("Local Indicators of Spatial Autocorrelation:", s),
      fill = "Cluster"
    ) +
    cowplot::theme_map()

  slug <- slugify(s)
  outfile <- file.path("outputs", "maps", "lisa", paste0("lisa_", slug, "_england.png"))

  ggsave(
    filename = outfile,
    plot = p,
    width = 16,
    height = 9,
    device = png, dpi = 450
  )
}

### Metropolitan Areas
city_bounds <- list(
  london = list(
    xlim = c(500000, 570000),
    ylim = c(155000, 205000)
  ),
  manchester = list(
    xlim = c(360000, 405000),
    ylim = c(380000, 420000)
  ),
  birmingham = list(
    xlim = c(385000, 440000),
    ylim = c(265000, 310000)
  )
)

make_bbox_sf <- function(xlim, ylim, crs) {
  bb <- st_polygon(list(rbind(
    c(xlim[1], ylim[1]),
    c(xlim[2], ylim[1]),
    c(xlim[2], ylim[2]),
    c(xlim[1], ylim[2]),
    c(xlim[1], ylim[1])
  )))
  st_sf(geometry = st_sfc(bb, crs = crs))
}

for (s in all_sen) {
  s_slug <- slugify(s)
  d <- lisa_sf[lisa_sf$sen_indicator == s,]

  for (city in names(city_bounds)) {
    bb <- city_bounds[[city]]

    bbox_sf <- make_bbox_sf(
      xlim = bb$xlim,
      ylim = bb$ylim,
      crs = st_crs(lisa_sf)
    )

    p <- ggplot(d) +
      geom_sf(aes(fill = cluster), colour = "black", size = 0.05) +
      geom_sf(data = bbox_sf, fill = NA, colour = "black", size = 0.01) +
      facet_wrap( ~ year) +
      scale_fill_manual(
        values = c(
          "High-High" = "darkred",
          "Low-Low" = "darkblue",
          "High-Low" = "pink",
          "Low-High" = "lightblue",
          "Not significant" = "white"
        ),
        na.value = "grey80",
        drop = FALSE
      ) +
      coord_sf(xlim = bb$xlim,
               ylim = bb$ylim,
               expand = FALSE) +
      labs(
        title = paste("Local Indicators of Spatial Autocorrelation:", s),
        subtitle = paste("Area:", tools::toTitleCase(city)),
        fill = "Cluster"
      ) +
      cowplot::theme_map()

    ggsave(
      filename = file.path("outputs", "maps", "lisa", paste0("lisa_", s_slug, "_", city, ".png")),
      plot = p,
      width = 16,
      height = 9,
      device = png, dpi = 450
    )
  }
}
