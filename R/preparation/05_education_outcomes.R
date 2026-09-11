# Education outcomes -------------------------------------------------------
# Constructs registered suicide death, alternative provision, and exclusion
# source datasets used by the downstream pupil-year analysis.

# Suicides ====
Civil_Reg_Deaths_Table <- source_tables$civil_reg_deaths

deaths_con <- connect_to_database()
Civil_Reg_Deaths <- deaths_con %>%
  tbl(Civil_Reg_Deaths_Table) %>%
  collect() %>%
  janitor::clean_names() %>%
  as.data.table() %>%
  data.table::setnames("token_person_id", "pseudo") %>%
  semi_join(Spine) %>%
  dplyr::select(
    pseudo,
    dec_agec,
    reg_date,
    reg_date_of_death,
    reg_district_name,
    starts_with("s_cod_code_"),
    s_injury_external,
    s_underlying_cod_icd10
  ) %>%
  mutate(
    reg_date = ymd(reg_date),
    reg_date_of_death = ym(reg_date_of_death)
  ) %>%
  pivot_longer(
    cols = starts_with("s_cod_code_"),
    names_to = "line",
    values_to = "code"
  ) |>
  dplyr::filter(code != "")
DBI::dbDisconnect(deaths_con)

rm(Civil_Reg_Deaths_Table, deaths_con)

# Using the Ann John 2023 codelist from HDR UK phenotype library.
Suicide_Deaths <- Civil_Reg_Deaths |>
  mutate(suicide = case_when(
    str_detect(
      code,
      "^(X6[0-9]|X7[0-9]|X8[0-4]|Y1[0-9]|Y2[0-9]|Y3[0-4]|W7[5-6]|X4[0-9]|Y872|Y899|R99X)"
    ) ~ 1,
    TRUE ~ 0
  )) |>
  dplyr::filter(suicide == 1) |>
  dplyr::distinct() |>
  mutate(year = year(reg_date_of_death)) |>
  semi_join(Spine) |>
  dplyr::select(pseudo, year, suicide, reg_date_of_death) |>
  distinct()

Suicide <- Spine |>
  left_join(Suicide_Deaths) |>
  mutate(suicide = coalesce(suicide, 0))

rm(Suicide_Deaths)
write_parquet_labeled(Suicide, "data/Suicide")

# Alternative Provision ====
AP_Census_Table <- source_tables$alternative_provision

ap_con <- connect_to_database()
AlternativeProvision <- ap_con %>%
  tbl(AP_Census_Table) %>%
  collect() %>%
  janitor::clean_names() %>%
  as.data.table() %>%
  data.table::setnames("ap_pupil_matching_ref_anonymous",
                       "pupil_matching_ref_anonymous") %>%
  data.table::setnames("ap_year", "year") %>%
  mutate(year = as.integer(year)) %>%
  semi_join(Spine) %>%
  dplyr::select(
    pupil_matching_ref_anonymous,
    year,
    ap_ap_type_description
  ) %>%
  mutate(alternative_provision = 1) |>
  dplyr::distinct()
DBI::dbDisconnect(ap_con)

rm(AP_Census_Table, ap_con)

AlternativeProvision <- AlternativeProvision[
  ,
  .(alternative_provision = max(alternative_provision, na.rm = TRUE)),
  by = .(pupil_matching_ref_anonymous, year)
]

Alternative_Provision <- Spine |>
  left_join(AlternativeProvision) |>
  mutate(alternative_provision = coalesce(alternative_provision, 0))

rm(AlternativeProvision)
write_parquet_labeled(Alternative_Provision, "data/Alternative_Provision")

# Exclusions (2006 onwards only) ====
get_exclusions <- function(year) {
  years <- as.integer(year)
  combos <- data.frame(years = years)

  results <-
    future.apply::future_lapply(seq_len(nrow(combos)), function(i) {
      yr <- combos$years[i]
      suffix <- substr(yr, 3, 4)
      table_name <- paste0("Exclusions_", yr, "_Table1")

      col_ref <- paste0("PupilMatchingRefAnonymous_ex", suffix)
      col_ncy <- paste0("NCYear_ex", suffix)
      col_cat <- paste0("Category_ex", suffix)
      col_sta <- paste0("StartDate_ex", suffix)
      col_ses <- paste0("Sessions_ex", suffix)
      col_ped <- paste0("Perm_Duplicate_ex", suffix)

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
          dplyr::select(all_of(c(
            col_ref,
            col_cat,
            col_sta,
            col_ses,
            col_ped
          ))) %>%
          collect() %>%
          rename(
            pupil_matching_ref_anonymous = !!col_ref,
            exclusion_type = !!col_cat,
            exclusion_start_date = !!col_sta,
            sessions_excluded = !!col_ses,
            multiple_permanent_exclusions = !!col_ped
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

Exclusions <- get_exclusions(Year) %>%
  drop_na()

Exclusions <-
  Spine[Exclusions, on = .(pupil_matching_ref_anonymous, year), nomatch = NULL]

exclusion_type_labels <- c(
  "FIXD" = "Fixed Period",
  "PERM" = "Permanent",
  "LNCH" = "Lunchtime (up to 19/20)",
  "SUSP" = "Suspension (from 20/21)"
)

Exclusions <- Exclusions |>
  mutate(exclusion_type = factor(
    exclusion_type,
    levels = names(exclusion_type_labels),
    labels = unname(exclusion_type_labels)
  ))

rm(get_exclusions, exclusion_type_labels)
gc()

exclusions_labels <- c(
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
  exclusion_type = "Exclusion Type",
  exclusion_start_date = "Exclusion Start Date",
  sessions_excluded = "Sessions Excluded",
  multiple_permanent_exclusions = "Multiple Permanent Exclusions at the Same School"
)

label(Exclusions) <-
  as.list(exclusions_labels[match(names(Exclusions), names(exclusions_labels))])
rm(exclusions_labels)

Exclusions <- Exclusions[
  ,
  c(
    list(sessions_excluded = sum(sessions_excluded, na.rm = TRUE)),
    lapply(.SD, first)
  ),
  by = .(pupil_matching_ref_anonymous, year),
  .SDcols = !c(
    "sessions_excluded",
    "exclusion_type",
    "exclusion_start_date",
    "multiple_permanent_exclusions"
  )
]

write_parquet_labeled(Exclusions, "data/Exclusions")
