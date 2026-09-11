# Demographic covariates ---------------------------------------------------
# Extracts and labels pupil-year demographic and deprivation covariates.

# Demographics ====
get_demographics <- function(year, term) {
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
      col_gen <-
        paste0("Gender_", short, suffix)
      col_age <-
        paste0("AgeAtStartOfAcademicYear_", short, suffix)
      col_ema <-
        paste0("EthnicGroupMajor_", short, suffix)
      col_emi <-
        paste0("EthnicGroupMinor_", short, suffix)
      col_fsm <-
        paste0("FSMeligible_", short, suffix)
      col_lan <-
        paste0("Language_", short, suffix)
      col_lag <-
        paste0("LanguageGroupMajor_", short, suffix)
      col_lau <-
        paste0("HomeLA_9Code_", short, suffix)
      col_imd <-
        paste0("overall_imd_2010_rank")
      col_ida <-
        paste0("idaci_2010_rank")

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
              col_gen,
              col_age,
              col_ema,
              col_emi,
              col_fsm,
              col_lan,
              col_lag,
              col_lau,
              col_imd,
              col_ida
            )
          )) %>%
          collect() %>%
          rename(
            pupil_matching_ref_anonymous = !!col_ref,
            gender = !!col_gen,
            age_at_start_of_academic_year = !!col_age,
            ethnic_group_major = !!col_ema,
            ethnic_group_minor = !!col_emi,
            fsm_eligible = !!col_fsm,
            language = !!col_lan,
            language_group = !!col_lag,
            home_local_authority = !!col_lau,
            imd = !!col_imd,
            idaci = !!col_ida
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

Demographics <- get_demographics(Year, Term)

Demographics <-
  Spine[Demographics, on = .(pupil_matching_ref_anonymous, year), nomatch = NULL]

gender_labels <- c("M" = "Boys",
                   "F" = "Girls")

ethnic_group_major_labels <- c(
  "WHIT" = "White",
  "BLAC" = "Black",
  "ASIA" = "Asian",
  "CHIN" = "Chinese",
  "AOEG" = "Any other ethnic group",
  "MIXD" = "Mixed",
  "UNCL" = "Unclassified"
)

fsm_eligible_labels <- c(`0` = "Not eligible for FSM",
                         `1` = "Eligible for FSM")

language_group_labels <- c("1_ENG" = "English",
                           "2_OTH" = "Other than English",
                           "3_UNCL" = "Unclassified")

Demographics <- Demographics |>
  dplyr::mutate(
    gender = factor(
      gender,
      levels = names(gender_labels),
      labels = unname(gender_labels)
    ),
    ethnic_group_major = factor(
      ethnic_group_major,
      levels = names(ethnic_group_major_labels),
      labels = unname(ethnic_group_major_labels)
    ),
    fsm_eligible = factor(
      fsm_eligible,
      levels = names(fsm_eligible_labels),
      labels = unname(fsm_eligible_labels)
    ),
    language_group = factor(
      language_group,
      levels = names(language_group_labels),
      labels = unname(language_group_labels)
    )
  )

rm(
  get_demographics,
  gender_labels,
  ethnic_group_major_labels,
  fsm_eligible_labels,
  language_group_labels
)

gc()

demographics_labels <-
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
    sen_indicator = "Type of SEN(D)s",
    gender = "Sex",
    age_at_start_of_academic_year = "Age at Start of Academic Year",
    ethnic_group_major = "Ethnic Group (Major)",
    ethnic_group_minor = "Ethnic Group (Minor)",
    fsm_eligible = "Eligibility for Free School Meals",
    language = "Language",
    language_group = "Language Group",
    home_local_authority = "Home Local Authority",
    imd = "Index of Multiple Deprivation Rank",
    idaci = "Income Deprivation Affecting Children Index Rank"
  )

label(Demographics) <-
  as.list(demographics_labels[match(names(Demographics), names(demographics_labels))])
rm(demographics_labels)

write_parquet_labeled(Demographics, "data/Demographics")
