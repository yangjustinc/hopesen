# Data-free synthetic QA tests ---------------------------------------------
#
# These tests use base R only so they can run in public CI without installing
# the analytical dependency stack or accessing restricted ECHILD data.

source(file.path("R", "functions_send.R"))
source(file.path("R", "functions_models.R"))

expect_error <- function(expr) {
  inherits(try(force(expr), silent = TRUE), "try-error")
}

# SEND profile classification ---------------------------------------------
synthetic_send <- data.frame(
  neurodivergent = c(0, 1, 0, 0, 0, 1, 1, 1, 0),
  socioemotional = c(0, 0, 1, 0, 0, 1, 0, 0, 1),
  impairment = c(0, 0, 0, 1, 0, 0, 1, 0, 1),
  other = c(0, 0, 0, 0, 1, 0, 0, 1, 0)
)

observed_profiles <- classify_send_profile(
  synthetic_send$neurodivergent,
  synthetic_send$socioemotional,
  synthetic_send$impairment,
  synthetic_send$other
)

stopifnot(
  identical(observed_profiles, 0:8),
  identical(names(SEND_PROFILE_LABELS), as.character(0:8)),
  identical(
    unname(SEND_PROFILE_LABELS[observed_profiles + 1L]),
    unname(SEND_PROFILE_LABELS)
  )
)

residual_profiles <- classify_send_profile(
  neurodivergent = c(0, 1, 1),
  socioemotional = c(1, 1, 1),
  impairment = c(1, 1, 0),
  other = c(0, 0, 1)
)
stopifnot(identical(residual_profiles, c(8L, 8L, 8L)))

stopifnot(
  expect_error(classify_send_profile(c(0, 1), 0, c(0, 0), c(0, 0))),
  expect_error(classify_send_profile(2, 0, 0, 0)),
  expect_error(classify_send_profile(NA, 0, 0, 0))
)

# Locked model specification ----------------------------------------------
binary_formula <- paste(deparse(model_formula("any_hosp")), collapse = " ")
required_terms <- c(
  "sen_indicator", "gender", "age_at_start_of_academic_year",
  "ethnic_group_major", "fsm_eligible", "language_group",
  "i(idaci)", "chc_any", "year + code"
)
stopifnot(all(vapply(
  required_terms,
  function(term) grepl(term, binary_formula, fixed = TRUE),
  logical(1)
)))
stopifnot(!grepl("offset", binary_formula, fixed = TRUE))

absence_formula <- paste(
  deparse(model_formula(
    "overall_absence_6halfterms",
    offset = "sessions_possible_6halfterms"
  )),
  collapse = " "
)
stopifnot(
  grepl(
    "offset(log(sessions_possible_6halfterms))",
    absence_formula,
    fixed = TRUE
  ),
  grepl("| year + code", absence_formula, fixed = TRUE)
)

stopifnot(
  length(send_coef_map) == 8L,
  all(startsWith(names(send_coef_map), "sen_indicator")),
  "sen_indicatorNeurodivergent + other" %in% names(send_coef_map),
  !"Neurodivergent + other" %in% names(send_coef_map)
)

message("All synthetic QA tests passed.")
