testthat::test_that("SEND profile classifier reproduces the nine analysis groups", {
  synthetic <- data.frame(
    neurodivergent = c(0, 1, 0, 0, 0, 1, 1, 1, 0),
    socioemotional = c(0, 0, 1, 0, 0, 1, 0, 0, 1),
    impairment     = c(0, 0, 0, 1, 0, 0, 1, 0, 1),
    other          = c(0, 0, 0, 0, 1, 0, 0, 1, 0)
  )

  observed <- classify_send_profile(
    synthetic$neurodivergent,
    synthetic$socioemotional,
    synthetic$impairment,
    synthetic$other
  )

  testthat::expect_identical(observed, 0:8)
  testthat::expect_identical(names(SEND_PROFILE_LABELS), as.character(0:8))
  testthat::expect_identical(unname(SEND_PROFILE_LABELS[observed + 1L]), unname(SEND_PROFILE_LABELS))
})

testthat::test_that("other multi-domain combinations fall into the residual group", {
  observed <- classify_send_profile(
    neurodivergent = c(0, 1, 1),
    socioemotional = c(1, 1, 1),
    impairment = c(1, 1, 0),
    other = c(0, 0, 1)
  )

  testthat::expect_identical(observed, c(8L, 8L, 8L))
})

testthat::test_that("SEND classifier rejects malformed inputs", {
  testthat::expect_error(
    classify_send_profile(c(0, 1), 0, c(0, 0), c(0, 0)),
    "equal length"
  )
  testthat::expect_error(
    classify_send_profile(2, 0, 0, 0),
    "only 0/1"
  )
  testthat::expect_error(
    classify_send_profile(NA, 0, 0, 0),
    "only 0/1"
  )
})
