# SEND profile helpers ------------------------------------------------------
#
# Pure functions and labels used to derive the mutually exclusive SEND
# provision profiles. Keeping this logic outside the database-extraction code
# makes the classification transparent, reusable, and testable with synthetic
# records that contain no ECHILD data.

SEND_PROFILE_LABELS <- c(
  `0` = "No SEND",
  `1` = "Neurodivergent SEN only",
  `2` = "Social, emotional, and/or mental health problem(s) only",
  `3` = "Sensory impairment and/or physical disability only",
  `4` = "Other SEN(D) only",
  `5` = "Neurodivergent + social, emotional and/or mental health",
  `6` = "Neurodivergent + sensory/physical",
  `7` = "Neurodivergent + other",
  `8` = "Any other combination"
)

classify_send_profile <- function(
    neurodivergent,
    socioemotional,
    impairment,
    other) {
  inputs <- list(neurodivergent, socioemotional, impairment, other)
  lengths <- vapply(inputs, length, integer(1))

  if (length(unique(lengths)) != 1L) {
    stop("SEND profile inputs must have equal length.", call. = FALSE)
  }

  invalid <- vapply(
    inputs,
    function(x) any(is.na(x) | !x %in% c(0L, 1L)),
    logical(1)
  )
  if (any(invalid)) {
    stop("SEND profile inputs must contain only 0/1 values.", call. = FALSE)
  }

  # Start with category 8: any combination not represented by one of the
  # explicitly defined single-domain or neurodivergent-plus-one-domain groups.
  profile <- rep.int(8L, lengths[[1]])

  profile[
    neurodivergent == 0L & socioemotional == 0L &
      impairment == 0L & other == 0L
  ] <- 0L
  profile[
    neurodivergent == 1L & socioemotional == 0L &
      impairment == 0L & other == 0L
  ] <- 1L
  profile[
    neurodivergent == 0L & socioemotional == 1L &
      impairment == 0L & other == 0L
  ] <- 2L
  profile[
    neurodivergent == 0L & socioemotional == 0L &
      impairment == 1L & other == 0L
  ] <- 3L
  profile[
    neurodivergent == 0L & socioemotional == 0L &
      impairment == 0L & other == 1L
  ] <- 4L
  profile[
    neurodivergent == 1L & socioemotional == 1L &
      impairment == 0L & other == 0L
  ] <- 5L
  profile[
    neurodivergent == 1L & socioemotional == 0L &
      impairment == 1L & other == 0L
  ] <- 6L
  profile[
    neurodivergent == 1L & socioemotional == 0L &
      impairment == 0L & other == 1L
  ] <- 7L

  profile
}
