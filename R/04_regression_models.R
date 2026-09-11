# Final manuscript regression models --------------------------------------
#
# This script implements the final (June 2026) modelling specification. The
# repetitive model-fitting code from the working analysis has been
# functionalised for clarity; model formulae, covariates, fixed effects,
# clustering and outcome definitions are unchanged.

source("R/functions_models.R")

Composite_Data <- read_parquet_labeled("data/Composite_Data")
data.table::setDT(Composite_Data)
assert_unique_pupil_year(Composite_Data, "Composite_Data")

# Derived binary outcomes --------------------------------------------------
Composite_Data[, `:=`(
  any_hosp = as.integer(!is.na(apc_record_n) & apc_record_n > 0),
  any_emerg_hosp = as.integer(!is.na(apc_emergency_n) & apc_emergency_n > 0),
  self_injury_any = as.integer(
    ari_self_harm %in% 1L | srp_self_harm %in% 1L | sui_suicide %in% 1L
  ),
  exclusion_or_ap = as.integer(
    any_exclusion %in% 1L | any_alternative_provision %in% 1L
  )
)]

# Any chronic-health category recorded in the prepared HES phenotype data.
chc_cols <- setdiff(grep("^chc_", names(Composite_Data), value = TRUE), "chc_any")
Composite_Data[, chc_any := as.integer(
  Reduce(`|`, lapply(.SD, function(x) x %in% 1L))
), .SDcols = chc_cols]

# Common complete-case population for outcomes that do not require an
# additional denominator/offset.
model_vars <- c(
  "sen_indicator", main_covariates, "year", "code",
  "pupil_matching_ref_anonymous"
)
idx_main <- Composite_Data[, complete.cases(.SD), .SDcols = model_vars]

binary_outcomes <- c(
  any_hosp = "Any inpatient",
  any_emerg_hosp = "Emergency inpatient",
  self_injury_any = "Self-injury/suicide",
  exclusion_or_ap = "Exclusion/AP",
  any_exclusion = "Exclusion",
  any_alternative_provision = "Alternative provision"
)

binary_models <- lapply(names(binary_outcomes), function(outcome) {
  fit_rr_model(Composite_Data, outcome = outcome, subset = idx_main)
})
names(binary_models) <- names(binary_outcomes)

# Absence outcomes ---------------------------------------------------------
idx_abs <- Composite_Data[, idx_main &
  !is.na(overall_absence_6halfterms) &
  !is.na(sessions_possible_6halfterms) &
  sessions_possible_6halfterms > 0]

absence_outcomes <- c(
  overall_absence_6halfterms = "Overall absence",
  authorised_absence_6halfterms = "Authorised absence",
  unauthorised_absence_6halfterms = "Unauthorised absence"
)

absence_models <- lapply(names(absence_outcomes), function(outcome) {
  fit_rr_model(
    Composite_Data,
    outcome = outcome,
    subset = idx_abs,
    offset = "sessions_possible_6halfterms"
  )
})
names(absence_models) <- names(absence_outcomes)

# Extract machine-readable effect estimates as well as formatted Word tables.
rr_results <- data.table::rbindlist(
  c(
    lapply(names(binary_models), function(nm) {
      cbind(outcome = nm, extract_send_rr(binary_models[[nm]]))
    }),
    lapply(names(absence_models), function(nm) {
      cbind(outcome = nm, extract_send_rr(absence_models[[nm]]))
    })
  ),
  use.names = TRUE,
  fill = TRUE
)

# No record-level information is written here: this CSV contains model
# coefficients only and remains subject to the project's SRS output process.
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(rr_results, "outputs/models/send_effect_estimates.csv")

# Manuscript table ---------------------------------------------------------
main_models <- list(
  "Absence rate" = absence_models[["overall_absence_6halfterms"]],
  "Any inpatient" = binary_models[["any_hosp"]],
  "Emergency inpatient" = binary_models[["any_emerg_hosp"]],
  "Self-injury/suicide" = binary_models[["self_injury_any"]],
  "Exclusion/AP" = binary_models[["exclusion_or_ap"]]
)

gof_keep <- data.frame(raw = "nobs", clean = "Pupil-years", fmt = 0)

tab_main <- modelsummary::modelsummary(
  main_models,
  exponentiate = TRUE,
  coef_map = send_coef_map,
  statistic = "({conf.low}, {conf.high})",
  conf_level = 0.95,
  fmt = 2,
  gof_map = gof_keep,
  output = "flextable"
)

tab_main <- tab_main |>
  flextable::theme_booktabs() |>
  flextable::fontsize(size = 9, part = "all") |>
  flextable::bold(part = "header") |>
  flextable::align(align = "center", part = "all") |>
  flextable::align(j = 1, align = "left", part = "all") |>
  flextable::autofit() |>
  flextable::add_footer_lines(
    paste(
      "Adjusted risk ratios or rate ratios with 95% confidence intervals.",
      "Models adjust for gender, age, ethnicity, FSM eligibility, language",
      "group, IDACI, and chronic health, with academic year and school local",
      "authority fixed effects. Standard errors clustered by pupil."
    )
  )

landscape_section <- officer::prop_section(
  page_size = officer::page_size(orient = "landscape"),
  page_margins = officer::page_mar(
    top = 0.4, bottom = 0.4, left = 0.4, right = 0.4
  )
)

flextable::save_as_docx(
  tab_main,
  path = "outputs/models/main_models.docx",
  pr_section = landscape_section
)

# Supplementary absence decomposition ------------------------------------
modelsummary::modelsummary(
  list(
    "Overall absence" = absence_models[["overall_absence_6halfterms"]],
    "Authorised absence" = absence_models[["authorised_absence_6halfterms"]],
    "Unauthorised absence" = absence_models[["unauthorised_absence_6halfterms"]]
  ),
  exponentiate = TRUE,
  coef_map = send_coef_map,
  statistic = "({conf.low}, {conf.high})",
  conf_level = 0.95,
  fmt = 2,
  gof_map = gof_keep,
  output = "outputs/models/absence_models.docx"
)

# Supplementary exclusion/AP decomposition -------------------------------
modelsummary::modelsummary(
  list(
    "Exclusion/AP" = binary_models[["exclusion_or_ap"]],
    "Exclusion" = binary_models[["any_exclusion"]],
    "Alternative provision" = binary_models[["any_alternative_provision"]]
  ),
  exponentiate = TRUE,
  coef_map = send_coef_map,
  statistic = "({conf.low}, {conf.high})",
  conf_level = 0.95,
  fmt = 2,
  gof_map = gof_keep,
  output = "outputs/models/exclusion_ap_models.docx"
)
