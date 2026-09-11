# School absence -----------------------------------------------------------
# Extracts annual absence counts and denominators. 2020 and 2021 are absent
# from the source used by this project and remain missing, not zero.

# Absences (2020 and 2021 are missing) ====
get_absences <- function(year) {
  years <- as.integer(year)
  valid_year <- setdiff(as.integer(years), c(2020, 2021))

  results <-
    future.apply::future_lapply(valid_year, function(valid_year) {
      yr <- valid_year
      suffix <- substr(yr, 3, 4)
      table_name <- paste0("Absence_", yr, "_3Term")

      col_ref <- paste0("PupilMatchingRefAnonymous_ab", suffix)
      col_ncy <- paste0("NCyearActual_ab", suffix)
      col_aau <- paste0("AuthorisedAbsence_Autumn_ab", suffix)
      col_uau <- paste0("UnauthorisedAbsence_Autumn_ab", suffix)
      col_oau <- paste0("OverallAbsence_Autumn_ab", suffix)
      col_spu <- paste0("SessionsPossible_Autumn_ab", suffix)
      col_aap <- paste0("AuthorisedAbsence_Spring_ab", suffix)
      col_uap <- paste0("UnauthorisedAbsence_Spring_ab", suffix)
      col_oap <- paste0("OverallAbsence_Spring_ab", suffix)
      col_spp <- paste0("SessionsPossible_Spring_ab", suffix)
      col_aam <- paste0("AuthorisedAbsence_Summer_ab", suffix)
      col_uam <- paste0("UnauthorisedAbsence_Summer_ab", suffix)
      col_oam <- paste0("OverallAbsence_Summer_ab", suffix)
      col_spm <- paste0("SessionsPossible_Summer_ab", suffix)
      col_aa6 <- paste0("AuthorisedAbsence_6HalfTerms_ab", suffix)
      col_ua6 <- paste0("UnauthorisedAbsence_6HalfTerms_ab", suffix)
      col_oa6 <- paste0("OverallAbsence_6HalfTerms_ab", suffix)
      col_sp6 <- paste0("SessionsPossible_6HalfTerms_ab", suffix)

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
          dplyr::select(all_of(
            c(
              col_ref,
              col_aau, col_uau, col_oau, col_spu,
              col_aap, col_uap, col_oap, col_spp,
              col_aam, col_uam, col_oam, col_spm,
              col_aa6, col_ua6, col_oa6, col_sp6
            )
          )) %>%
          collect() %>%
          rename(
            pupil_matching_ref_anonymous = !!col_ref,
            authorised_absence_autumn = !!col_aau,
            unauthorised_absence_autumn = !!col_uau,
            overall_absence_autumn = !!col_oau,
            sessions_possible_autumn = !!col_spu,
            authorised_absence_spring = !!col_aap,
            unauthorised_absence_spring = !!col_uap,
            overall_absence_spring = !!col_oap,
            sessions_possible_spring = !!col_spp,
            authorised_absence_summer = !!col_aam,
            unauthorised_absence_summer = !!col_uam,
            overall_absence_summer = !!col_oam,
            sessions_possible_summer = !!col_spm,
            authorised_absence_6halfterms = !!col_aa6,
            unauthorised_absence_6halfterms = !!col_ua6,
            overall_absence_6halfterms = !!col_oa6,
            sessions_possible_6halfterms = !!col_sp6
          ) %>%
          mutate(year = yr) %>%
          as.data.table()
      }, error = function(e) {
        message("Error in ", table_name, ": ", e$message)
        data.table()
      })
    }, future.seed = TRUE)

  rbindlist(results, fill = TRUE)
}

Absences <- get_absences(Year)
Absences <- Spine[Absences, on = .(pupil_matching_ref_anonymous, year), nomatch = NULL]

Absences <- Absences |>
  drop_na(year) |>
  mutate(
    absence_rate_autumn = 100 * overall_absence_autumn / sessions_possible_autumn,
    absence_rate_spring = 100 * overall_absence_spring / sessions_possible_spring,
    absence_rate_summer = 100 * overall_absence_summer / sessions_possible_summer,
    absence_rate_6halfterms = 100 * overall_absence_6halfterms / sessions_possible_6halfterms
  )

rm(get_absences)
gc()

absences_labels <- c(
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
  sen_indicator = "Type of SEN(D)s",
  authorised_absence_autumn = "Number of sessions missed due to authorised absence in Autumn Term",
  unauthorised_absence_autumn = "Number of sessions missed due to unauthorised absence in Autumn Term",
  overall_absence_autumn = "Number of sessions missed due to absence in Autumn Term",
  sessions_possible_autumn = "Number of sessions possible in Autumn Term",
  absence_rate_autumn = "Percentage of sessions missed due to absence in Autumn Term",
  authorised_absence_spring = "Number of sessions missed due to authorised absence in Spring Term",
  unauthorised_absence_spring = "Number of sessions missed due to unauthorised absence in Spring Term",
  overall_absence_spring = "Number of sessions missed due to absence in Spring Term",
  sessions_possible_spring = "Number of sessions possible in Spring Term",
  absence_rate_spring = "Percentage of sessions missed due to absence in Spring Term",
  authorised_absence_summer = "Number of sessions missed due to authorised absence in Summer Term",
  unauthorised_absence_summer = "Number of sessions missed due to unauthorised absence in Summer Term",
  overall_absence_summer = "Number of sessions missed due to absence in Summer Term",
  sessions_possible_summer = "Number of sessions possible in Summer Term",
  absence_rate_summer = "Percentage of sessions missed due to absence in Summer Term",
  authorised_absence_6halfterms = "Number of sessions missed due to authorised absence during the academic year",
  unauthorised_absence_6halfterms = "Number of sessions missed due to unauthorised absence during the academic year",
  overall_absence_6halfterms = "Number of sessions missed due to absence during the academic year",
  sessions_possible_6halfterms = "Number of sessions possible during the academic year",
  absence_rate_6halfterms = "Percentage of sessions missed due to absence during the academic year"
)

label(Absences) <-
  as.list(absences_labels[match(names(Absences), names(absences_labels))])
rm(absences_labels)

write_parquet_labeled(Absences, "data/Absences")
