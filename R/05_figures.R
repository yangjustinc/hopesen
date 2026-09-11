# Descriptive figures ------------------------------------------------------
#
# Generates descriptive visualisations from the prepared source datasets.
# The small helpers below keep repeated summarise/plot/save operations visible
# without duplicating several dozen lines for each characteristic. Figures
# produced inside the SRS remain subject to disclosure-control rules.

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

source_objects <- c(
  Spine = "Spine",
  Demographics = "Demographics",
  Education = "Education",
  Absences = "Absences",
  Exclusions = "Exclusions",
  Chronic_Health_Conditions = "Chronic_Health_Conditions",
  Adversity_Related_Injury = "Adversity_Related_Injury",
  Stress_Related_Presentation = "Stress_Related_Presentation"
)
for (object_name in names(source_objects)) {
  load_if_missing(object_name, file.path("data", source_objects[[object_name]]))
}

save_figure <- function(plot, filename, width = 16, height = 9) {
  ggplot2::ggsave(
    filename = file.path("outputs/figures", filename),
    plot = plot,
    width = width,
    height = height
  )
  invisible(plot)
}

profile_percentage_plot <- function(
    data,
    facet_var,
    denominator_vars = c("year", facet_var),
    y_label = "Percentage of pupils",
    exclude_no_send = TRUE) {

  grouping_vars <- c("year", facet_var, "sen_indicator")

  plot_data <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
    dplyr::summarise(
      n = dplyr::n_distinct(pupil_matching_ref_anonymous),
      .groups = "drop"
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(denominator_vars))) |>
    dplyr::mutate(
      n_total = sum(n),
      pct = 100 * n / n_total
    ) |>
    dplyr::ungroup()

  if (exclude_no_send) {
    plot_data <- dplyr::filter(plot_data, sen_indicator != "No SEND")
  }

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = year,
      y = pct,
      colour = sen_indicator,
      group = sen_indicator
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var))) +
    ggplot2::labs(
      x = "Spring term",
      y = y_label,
      colour = "SEND type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_colour_brewer(palette = "Set3")
}

phenotype_percentage_plot <- function(
    data,
    phenotype_vars,
    phenotype_label,
    y_label) {

  data |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(phenotype_vars),
      names_to = phenotype_label,
      values_to = "phenotype_flag"
    ) |>
    dplyr::group_by(
      year,
      .data[[phenotype_label]],
      sen_indicator
    ) |>
    dplyr::summarise(n = sum(phenotype_flag, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(year) |>
    dplyr::mutate(n_total = sum(n), pct = 100 * n / n_total) |>
    dplyr::ungroup() |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = year,
        y = pct,
        colour = sen_indicator,
        group = sen_indicator
      )
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(stats::as.formula(paste("~", phenotype_label))) +
    ggplot2::labs(
      x = "Spring term",
      y = y_label,
      colour = "SEND type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_colour_brewer(palette = "Set3")
}

# Pupil-year spine ---------------------------------------------------------
spine_figure <- profile_percentage_plot(
  Spine,
  facet_var = "nc_year_actual",
  denominator_vars = "year"
)
save_figure(spine_figure, "spine.svg")

# Demographics -------------------------------------------------------------
gender_figure <- Demographics |>
  dplyr::filter(gender %in% c("Boys", "Girls")) |>
  profile_percentage_plot("gender")
save_figure(gender_figure, "gender.svg")

ethnicity_figure <- Demographics |>
  dplyr::filter(ethnic_group_major != "Unclassified") |>
  profile_percentage_plot("ethnic_group_major")
save_figure(ethnicity_figure, "ethnicity.svg")

fsm_figure <- profile_percentage_plot(Demographics, "fsm_eligible")
save_figure(fsm_figure, "fsm.svg")

language_figure <- Demographics |>
  dplyr::filter(language_group != "Unclassified") |>
  profile_percentage_plot("language_group")
save_figure(language_figure, "language.svg")

# Education provision -----------------------------------------------------
sen_figure <- Education |>
  dplyr::filter(
    sen_indicator != "No SEND",
    sen_provision %in% c(
      "SEN Statement (up to 2017/18)",
      "SEN support (since 2014/15)",
      "Education, health, and care plan (since 2014/15)"
    )
  ) |>
  profile_percentage_plot(
    "sen_provision",
    y_label = "Percentage of pupils with SEND"
  )
save_figure(sen_figure, "sen_provision.svg")

sen_unit_figure <- Education |>
  dplyr::filter(sen_indicator != "No SEND", !is.na(sen_unit)) |>
  profile_percentage_plot(
    "sen_unit",
    y_label = "Percentage of pupils with SEND"
  )
save_figure(sen_unit_figure, "sen_unit.svg")

resourced_provision_figure <- Education |>
  dplyr::filter(
    sen_indicator != "No SEND",
    !is.na(resourced_provision)
  ) |>
  profile_percentage_plot(
    "resourced_provision",
    y_label = "Percentage of pupils with SEND"
  )
save_figure(resourced_provision_figure, "resourced_provision.svg")

phase_figure <- Education |>
  dplyr::filter(
    sen_indicator != "No SEND",
    !is.na(phase),
    !phase %in% c("Primary", "Middle (Deemed Primary)")
  ) |>
  profile_percentage_plot(
    "phase",
    y_label = "Percentage of pupils with SEND"
  )
save_figure(phase_figure, "phase.svg")

# Absence -----------------------------------------------------------------
absence_figure <- Absences |>
  dplyr::group_by(year, sen_indicator) |>
  dplyr::summarise(
    Autumn = median(absence_rate_autumn, na.rm = TRUE),
    Spring = median(absence_rate_spring, na.rm = TRUE),
    Summer = median(absence_rate_summer, na.rm = TRUE),
    Overall = median(absence_rate_6halfterms, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(
    cols = c(Autumn, Spring, Summer, Overall),
    names_to = "term",
    values_to = "absence_rate"
  ) |>
  dplyr::mutate(
    term = factor(term, levels = c("Autumn", "Spring", "Summer", "Overall"))
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = year,
      y = absence_rate,
      colour = sen_indicator,
      group = sen_indicator
    )
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~ term) +
  ggplot2::labs(
    x = "Spring term",
    y = "Median absence rate (%)",
    colour = "SEND type"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::scale_colour_brewer(palette = "Set3")
save_figure(absence_figure, "absences.svg")

# Exclusions ---------------------------------------------------------------
exclusions_figure <- Exclusions |>
  dplyr::group_by(year, sen_indicator) |>
  dplyr::summarise(
    sessions_excluded = median(sessions_excluded, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = year,
      y = sessions_excluded,
      colour = sen_indicator,
      group = sen_indicator
    )
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(
    x = "Spring term",
    y = "Median sessions excluded",
    colour = "SEND type"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::scale_colour_brewer(palette = "Set3")
save_figure(exclusions_figure, "exclusions.svg")

# Health phenotypes --------------------------------------------------------
chronic_vars <- intersect(
  c(
    "musculoskeletal_skin",
    "mental_health_behavioural",
    "metabolic_endocrine_digestive_renal_genitourinary",
    "cardiovascular",
    "neurological",
    "cancer_blood",
    "nonspecific",
    "respiratory",
    "chronic_infections"
  ),
  names(Chronic_Health_Conditions)
)
chronic_figure <- phenotype_percentage_plot(
  Chronic_Health_Conditions,
  chronic_vars,
  phenotype_label = "chronic_condition",
  y_label = "Percentage of hospital admissions"
)
save_figure(chronic_figure, "chronic_health_conditions.svg")

ari_vars <- intersect(
  c("self_harm", "drug_alc", "violence"),
  names(Adversity_Related_Injury)
)
ari_figure <- phenotype_percentage_plot(
  Adversity_Related_Injury,
  ari_vars,
  phenotype_label = "adversity_related_injury",
  y_label = "Percentage of emergency hospital admissions"
)
save_figure(ari_figure, "adversity_related_injury.svg")

srp_vars <- intersect(
  c(
    "self_harm",
    "internalising",
    "thought_disorder",
    "potentially_psych",
    "externalising"
  ),
  names(Stress_Related_Presentation)
)
srp_figure <- phenotype_percentage_plot(
  Stress_Related_Presentation,
  srp_vars,
  phenotype_label = "stress_related_presentation",
  y_label = "Percentage of emergency hospital admissions"
)
save_figure(srp_figure, "stress_related_presentation.svg")
