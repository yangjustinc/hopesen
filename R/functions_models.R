# Reusable modelling helpers ----------------------------------------------
#
# The final analysis used Poisson regression with log link to estimate risk
# ratios for binary outcomes and rate ratios for absence counts. School local
# authority and year are absorbed as fixed effects; standard errors are
# clustered by pupil. These helpers remove repetitive boilerplate without
# changing that specification.

main_covariates <- c(
  "gender", "age_at_start_of_academic_year", "ethnic_group_major",
  "fsm_eligible", "language_group", "idaci", "chc_any"
)

model_formula <- function(outcome, offset = NULL) {
  rhs <- paste(
    c(
      "sen_indicator", "gender", "age_at_start_of_academic_year",
      "ethnic_group_major", "fsm_eligible", "language_group",
      "i(idaci)", "chc_any",
      if (!is.null(offset)) sprintf("offset(log(%s))", offset)
    ),
    collapse = " + "
  )
  stats::as.formula(sprintf("%s ~ %s | year + code", outcome, rhs))
}

fit_rr_model <- function(data, outcome, subset, offset = NULL) {
  fixest::fepois(
    model_formula(outcome, offset = offset),
    vcov = ~ pupil_matching_ref_anonymous,
    data = data,
    subset = subset
  )
}

extract_send_rr <- function(model) {
  terms <- grep("^sen_indicator", names(stats::coef(model)), value = TRUE)
  ci <- stats::confint(model)[terms, , drop = FALSE]
  data.table::data.table(
    term = terms,
    estimate = exp(stats::coef(model)[terms]),
    conf_low = exp(ci[, 1]),
    conf_high = exp(ci[, 2])
  )
}

send_coef_map <- c(
  "sen_indicatorNeurodivergent SEN only" = "Neurodivergent SEN only",
  "sen_indicatorSocial, emotional, and/or mental health problem(s) only" = "Social, emotional, and/or mental health problem(s) only",
  "sen_indicatorSensory impairment and/or physical disability only" = "Sensory impairment and/or physical disability only",
  "sen_indicatorOther SEN(D) only" = "Other SEN(D) only",
  "sen_indicatorNeurodivergent + social, emotional and/or mental health" = "Neurodivergent + social, emotional and/or mental health",
  "sen_indicatorNeurodivergent + sensory/physical" = "Neurodivergent + sensory/physical",
  "sen_indicatorNeurodivergent + other" = "Neurodivergent + other",
  "sen_indicatorAny other combination" = "Any other combination"
)
