testthat::test_that("binary model formula contains the locked adjustment set and fixed effects", {
  f <- model_formula("any_hosp")
  txt <- paste(deparse(f), collapse = " ")

  required_terms <- c(
    "sen_indicator", "gender", "age_at_start_of_academic_year",
    "ethnic_group_major", "fsm_eligible", "language_group",
    "i(idaci)", "chc_any", "year + code"
  )

  for (term in required_terms) {
    testthat::expect_true(grepl(term, txt, fixed = TRUE), info = term)
  }
  testthat::expect_false(grepl("offset", txt, fixed = TRUE))
})

testthat::test_that("absence model formula contains the sessions offset", {
  f <- model_formula(
    "overall_absence_6halfterms",
    offset = "sessions_possible_6halfterms"
  )
  txt <- paste(deparse(f), collapse = " ")

  testthat::expect_true(
    grepl("offset(log(sessions_possible_6halfterms))", txt, fixed = TRUE)
  )
  testthat::expect_true(grepl("| year + code", txt, fixed = TRUE))
})

testthat::test_that("coefficient map contains every non-reference SEND profile", {
  testthat::expect_length(send_coef_map, 8L)
  testthat::expect_true(all(startsWith(names(send_coef_map), "sen_indicator")))
  testthat::expect_true("sen_indicatorNeurodivergent + other" %in% names(send_coef_map))
  testthat::expect_false("Neurodivergent + other" %in% names(send_coef_map))
})
