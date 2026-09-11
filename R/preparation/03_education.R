# Education characteristics -----------------------------------------------
# Extracts enrolment, SEN provision, school phase, and school-LA variables.

# Education ====
get_education <- function(year, term) {
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
      col_enr <-
        paste0("EnrolStatus_", short, suffix)
      col_par <-
        paste0("PartTime_", short, suffix)
      col_sen <-
        paste0("SENprovision_", short, suffix)
      col_sma <-
        paste0("SENprovisionMajor_", short, suffix)
      col_sun <-
        paste0("SENUnitIndicator_", short, suffix)
      col_rep <-
        paste0("ResourcedProvisionIndicator_", short, suffix)
      col_pha <-
        paste0("Phase_", short, suffix)
      col_lac <-
        paste0("LA_", short, suffix)
      col_lan <-
        paste0("LA_9Code_", short, suffix)

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
              col_enr,
              col_par,
              col_sen,
              col_sma,
              col_sun,
              col_rep,
              col_pha,
              col_lac,
              col_lan
            )
          )) %>%
          collect() %>%
          rename(
            pupil_matching_ref_anonymous = !!col_ref,
            enrolment_status = !!col_enr,
            part_time = !!col_par,
            sen_provision = !!col_sen,
            sen_provision_major = !!col_sma,
            sen_unit = !!col_sun,
            resourced_provision = !!col_rep,
            phase = !!col_pha,
            school_local_authority_code = !!col_lac,
            school_local_authority_9code = !!col_lan
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

Education <- get_education(Year, Term)

Education <-
  Spine[Education, on = .(pupil_matching_ref_anonymous, year), nomatch = NULL]

enrolment_status_labels <- c(
  "C" = "Current",
  "G" = "Guest",
  "M" = "Current Main",
  "S" = "Current Subsidiary",
  "F" = "FE College",
  "O" = "Other Provider"
)

part_time_labels <- c(`0` = "False",
                      `2` = "True")

sen_provision_labels <- c(
  "N" = "No special educational need",
  "A" = "School Action (up to 2014/15)",
  "P" = "School Action Plus (up to 2014/15)",
  "S" = "SEN Statement (up to 2017/18)",
  "K" = "SEN support (since 2014/15)",
  "E" = "Education, health, and care plan (since 2014/15)"
)

sen_provision_major_labels <- c(
  "1_NON" = "No identified SEN",
  "2_SNS" = "SEN without a Statement (up to 2017/18) or SEN support (since 2014/15)",
  "3_SS" = "SEN with a Statement (up to 2017/18) or an Education, health, and care plan (since 2014/15)",
  "4_UNCL" = "Unclassified (includes information refused or not obtained)"
)

resourced_provision_labels <- c(`0` = "Not in Resourced Provision",
                                `1` = "In Resourced Provision")

sen_unit_labels <- c(`0` = "Not in a SEN Unit",
                     `1` = "In a SEN Unit")

phase_labels <- c(
  "NS" = "Nursery",
  "PS" = "Primary",
  "MP" = "Middle (Deemed Primary)",
  "MS" = "Middle (Deemed Secondary)",
  "SS" = "Secondary (including CTC and Academies)",
  "SP" = "Special",
  "EY" = "Early Years Settings",
  "PR" = "Pupil Referral Unit (PRU)",
  "AT" = "All Through",
  "XX" = "Multiple Phases (not Middle, Special, or PRU)"
)

Education <- Education |>
  dplyr::mutate(
    enrolment_status = factor(
      enrolment_status,
      levels = names(enrolment_status_labels),
      labels = unname(enrolment_status_labels)
    ),
    part_time = factor(
      part_time,
      levels = names(part_time_labels),
      labels = unname(part_time_labels)
    ),
    sen_provision = factor(
      sen_provision,
      levels = names(sen_provision_labels),
      labels = unname(sen_provision_labels)
    ),
    sen_provision_major = factor(
      sen_provision_major,
      levels = names(sen_provision_major_labels),
      labels = unname(sen_provision_major_labels)
    ),
    sen_unit = factor(
      sen_unit,
      levels = names(sen_unit_labels),
      labels = unname(sen_unit_labels)
    ),
    resourced_provision = factor(
      resourced_provision,
      levels = names(resourced_provision_labels),
      labels = unname(resourced_provision_labels)
    ),
    phase = factor(
      phase,
      levels = names(phase_labels),
      labels = unname(phase_labels)
    )
  )

rm(
  get_education,
  enrolment_status_labels,
  part_time_labels,
  phase_labels,
  sen_provision_labels,
  sen_provision_major_labels,
  sen_unit_labels,
  resourced_provision_labels
)

gc()

education_labels <-
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
    enrolment_status = "Enrolment Status",
    part_time = "Part-Time Indicator",
    sen_provision = "SEN Provision Type",
    sen_provision_major = "Major SEN Provision Type",
    sen_unit = "Membership of a SEN Unit or Special Class",
    resourced_provision = "Membership of a Resourced Provision",
    phase = "Phase of Education",
    school_local_authority_code = "School Local Authority Code",
    school_local_authority_9code = "School Local Authority 9 Code"
  )

label(Education) <-
  as.list(education_labels[match(names(Education), names(education_labels))])
rm(education_labels)

write_parquet_labeled(Education, "data/Education")
