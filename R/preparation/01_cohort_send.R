# Cohort spine and SEND profiles -------------------------------------------
# Extracts KS3/KS4 census records, links the ECHILD bridge, and derives the
# mutually exclusive annual SEND profiles used throughout the analysis.

# Cohort Definition ====
get_cohort <- function(year, term) {
  term <- tools::toTitleCase(tolower(term))
  years <- as.integer(year)

  term_map <- c(Spring = "SPR",
                Summer = "SUM",
                Autumn = "AUT")
  term_short <- term_map[term]

  combos <-
    expand.grid(years = years,
                term = term,
                stringsAsFactors = FALSE)
  results <-
    future.apply::future_lapply(seq_len(nrow(combos)), function(i) {
      yr <- combos$year[i]
      tm <- combos$term[i]
      suffix <- substr(yr, 3, 4)
      short <- term_map[tm]
      table_name <-
        paste0(tm, "_Census_", yr)

      col_ref <-
        paste0("PupilMatchingRefAnonymous_", short, suffix)
      col_ncy <-
        paste0("NCyearActual_", short, suffix)
      col_pri <-
        paste0("PrimarySENtype_", short, suffix)
      col_sec <-
        paste0("SecondarySENtype_", short, suffix)

      con <- connect_to_database()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      tryCatch({
        if (!table_name %in% dbListTables(con)) {
          message("Table not found: ", table_name)
          return(data.table())
        }

        tbl(con, table_name) %>%
          filter(
            !!sym(col_ncy) %like% "7" |
              !!sym(col_ncy) %like% "8" |
              !!sym(col_ncy) %like% "9" |
              !!sym(col_ncy) %like% "10" |
              !!sym(col_ncy) %like% "11"
          ) %>%
          dplyr::select(all_of(c(col_ref, col_ncy, col_pri, col_sec))) %>%
          collect() %>%
          rename(
            pupil_matching_ref_anonymous = !!col_ref,
            nc_year_actual = !!col_ncy,
            primary_sen_type = !!col_pri,
            secondary_sen_type = !!col_sec
          ) %>%
          mutate(year = yr, term = tm) %>%
          as.data.table()
      }, error = function(e) {
        message("Error in ", table_name, ": ", e$message)
        data.table()
      })
    }, future.seed = TRUE)

  rbindlist(results, fill = TRUE)
}

Pupils <- get_cohort(Year, Term) %>%
  data.table::merge.data.table(Bridge, by = "pupil_matching_ref_anonymous", all.x = TRUE) %>%
  dplyr::distinct() %>%
  dplyr::select(
    pupil_matching_ref_anonymous,
    pseudo,
    year,
    term,
    nc_year_actual,
    primary_sen_type,
    secondary_sen_type
  ) %>%
  dplyr::mutate(nc_year_actual = as.numeric(nc_year_actual)) %>%
  tidyr::pivot_longer(cols = c(primary_sen_type, secondary_sen_type)) %>%
  dplyr::select(-name) %>%
  distinct() %>%
  dplyr::mutate(value = tolower(value)) %>%
  tidyr::pivot_wider(
    id_cols = c(
      pupil_matching_ref_anonymous,
      pseudo,
      year,
      term,
      nc_year_actual
    ),
    names_from = value
  ) %>%
  dplyr::select(
    pupil_matching_ref_anonymous,
    pseudo,
    year,
    term,
    nc_year_actual,
    spld,
    mld,
    sld,
    pmld,
    slcn,
    hi,
    vi,
    msi,
    pd,
    asd,
    oth,
    semh,
    nsa
  ) %>%
  dplyr::mutate(spld = case_when(is.na(spld) ~ 0,!is.na(spld) ~ 1)) %>%
  dplyr::mutate(mld = case_when(is.na(mld) ~ 0,!is.na(mld) ~ 1)) %>%
  dplyr::mutate(sld = case_when(is.na(sld) ~ 0,!is.na(sld) ~ 1)) %>%
  dplyr::mutate(pmld = case_when(is.na(pmld) ~ 0,!is.na(pmld) ~ 1)) %>%
  dplyr::mutate(slcn = case_when(is.na(slcn) ~ 0,!is.na(slcn) ~ 1)) %>%
  dplyr::mutate(hi = case_when(is.na(hi) ~ 0,!is.na(hi) ~ 1)) %>%
  dplyr::mutate(vi = case_when(is.na(vi) ~ 0,!is.na(vi) ~ 1)) %>%
  dplyr::mutate(msi = case_when(is.na(msi) ~ 0,!is.na(msi) ~ 1)) %>%
  dplyr::mutate(pd = case_when(is.na(pd) ~ 0,!is.na(pd) ~ 1)) %>%
  dplyr::mutate(asd = case_when(is.na(asd) ~ 0,!is.na(asd) ~ 1)) %>%
  dplyr::mutate(oth = case_when(is.na(oth) ~ 0,!is.na(oth) ~ 1)) %>%
  dplyr::mutate(semh = case_when(is.na(semh) ~ 0,!is.na(semh) ~ 1)) %>%
  dplyr::mutate(nsa = case_when(is.na(nsa) ~ 0,!is.na(nsa) ~ 1)) %>%
  dplyr::distinct() %>%
  dplyr::arrange(pupil_matching_ref_anonymous, year, term)

gc()

rm(get_cohort)

rm(Bridge) # Remove Bridge table

## Define SEND variables ====

Spine <- setDT(Pupils)

nd_cols <- c("spld", "mld", "sld", "pmld", "slcn", "asd")
Spine[, neurodivergent := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = nd_cols]

semh_cols <- c("semh")
Spine[, socioemotional := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = semh_cols]

spi_cols <- c("hi", "vi", "msi", "pd")
Spine[, impairment := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = spi_cols]

oth_cols <- c("oth", "nsa")
Spine[, other := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = oth_cols]

send_cols <- c(nd_cols, semh_cols, spi_cols, oth_cols)
Spine[, no_send := as.integer(rowSums(.SD, na.rm = TRUE) == 0L), .SDcols = send_cols]

Spine[, sen_indicator := fifelse(
  no_send == 1L,
  0L,
  fifelse(
    neurodivergent == 1L &
      socioemotional == 0L &
      impairment == 0L & other == 0L,
    1L,
    # Neurodivergent only
    fifelse(
      neurodivergent == 0L &
        socioemotional == 1L &
        impairment == 0L & other == 0L,
      2L,
      # Socioemotional only
      fifelse(
        neurodivergent == 0L &
          socioemotional == 0L &
          impairment == 1L &
          other == 0L,
        3L,
        # Sensory or physical impairment only
        fifelse(
          neurodivergent == 0L &
            socioemotional == 0L &
            impairment == 0L & other == 1L,
          4L,
          # Other SEND only
          fifelse(
            neurodivergent == 1L &
              socioemotional == 1L &
              impairment == 0L &
              other == 0L,
            5L,
            # Neurodivergent + socioemotional
            fifelse(
              neurodivergent == 1L &
                socioemotional == 0L &
                impairment == 1L &
                other == 0L,
              6L,
              # Neurodivergent + sensory/physical impairment
              fifelse(
                neurodivergent == 1L &
                  socioemotional == 0L &
                  impairment == 0L & other == 1L,
                7L,
                # Neurodivergent + other SEND
                8L,
                # Any combination of two or more of: socioemotional, sensory/physical, and other SEND
              )
            )
          )
        )
      )
    )
  )
)]

nc_year_labels <- c(
  `7` = "Year 7",
  `8` = "Year 8",
  `9` = "Year 9",
  `10` = "Year 10",
  `11` = "Year 11"
)

sen_labels <- c(
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

keys <- c("pupil_matching_ref_anonymous", "year")

dup_keys <- unique(Spine[duplicated(Spine, by = keys), ..keys])

if (nrow(dup_keys) > 0L) {
  send_fix <- Spine[dup_keys, on = keys,
                     .(no_send = as.integer(all(no_send == 1L)),
                       sen_indicator = max(sen_indicator, na.rm = TRUE)),
                     by = .EACHI][, c(keys, "no_send", "sen_indicator"), with = FALSE]

  Spine[send_fix, on = keys, `:=`(no_send = i.no_send,
                                  sen_indicator = i.sen_indicator)]

  Spine <-
    Spine[!duplicated(Spine, by = keys)]
}

stopifnot(nrow(Spine) == uniqueN(Spine, by = keys))

Spine <- Spine |>
  mutate(
    nc_year_actual = factor(
      nc_year_actual,
      levels = names(nc_year_labels),
      labels = unname(nc_year_labels)
    ),
    sen_indicator = factor(
      sen_indicator,
      levels = names(sen_labels),
      labels = unname(sen_labels)
    )
  )

gc()

rm (Pupils) # Remove Pupils table
rm(nd_cols,
   oth_cols,
   semh_cols,
   sen_labels,
   nc_year_labels,
   send_cols,
   spi_cols)

spine_labels <-
  c(
    year = "School Year",
    term = "Term",
    nc_year_actual = "National Curriculum Year",
    spld = "Specific Learning Difficulty",
    mld = "Moderate Learning Difficulty",
    sld = "Severe Learning Difficulty",
    pmld = "Profound & Multiple Learning Difficulty",
    besd = "Behaviour, Emotional, and Social Difficulties",
    slcn = "Speech, Language, and Communication Needs",
    hi = "Hearing Impairment",
    vi = "Visual Impairment",
    msi = "Multi-Sensory Impairment",
    pd = "Physical Disability",
    asd = "Autism Spectrum Disorder",
    oth = "Other Difficulty/Disability",
    semh = "Social, Emotional, and Mental Health",
    nsa = "SEN support but no specialist assessment of type of need",
    ds = "Down Syndrome",
    neurodivergent = "Neurodivergent SEN",
    socioemotional = "Social, Emotional, and/or Mental Health",
    impairment = "Sensory Impairment(s) and/or Physical Disability",
    other = "Other SEN(D)",
    no_send = "No SEND",
    sen_indicator = "Type of SEN(D)s"
  )

label(Spine) <-
  as.list(spine_labels[match(names(Spine), names(spine_labels))])
rm(spine_labels)

write_parquet_labeled(Spine, "data/Spine")
