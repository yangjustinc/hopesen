# HES admitted patient care ------------------------------------------------
# Extracts and harmonises annual APC episodes, diagnoses, procedures,
# admission method, discharge method, emergency status, and length of stay.

# HES APC ====
get_apc <- function(years) {
  years <- as.integer(years)

  empty_apc <- function() {
    diag_cols <- paste0("diag_", sprintf("%02d", 1:20))
    opertn_cols <- paste0("opertn_", sprintf("%02d", 1:24))
    opdate_cols <- paste0("opdate_", sprintf("%02d", 1:24))

    col_template <- data.table(matrix(ncol = 0, nrow = 0))
    col_template[, `:=`(
      pseudo = character(),
      epi_key = character(),
      epi_start = as.Date(character()),
      epi_end = as.Date(character()),
      admi_date = as.Date(character()),
      dis_date = as.Date(character()),
      admi_meth = character(),
      dis_meth = character(),
      year = integer()
    )]

    for (col in c(diag_cols, opertn_cols, opdate_cols)) {
      col_template[[col]] <-
        if (grepl("opdate", col)) as.Date(character()) else character()
    }

    col_template
  }

  results <- future_lapply(years, function(yr) {
    con <- connect_to_database()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    suffix <- as.character(yr)
    all_tables <- dbListTables(con)
    table_pattern <- paste0(".*_HES_APC_", suffix, "$")
    matched_table <- grep(table_pattern, all_tables, value = TRUE)

    if (length(matched_table) == 0) {
      message("No table found for year ", yr)
      return(empty_apc())
    }

    table_name <- matched_table[1]

    diag_vars <- paste0("DIAG_", sprintf("%02d", 1:20))
    opertn_vars <- paste0("OPERTN_", sprintf("%02d", 1:24))
    opdate_vars <- paste0("OPDATE_", sprintf("%02d", 1:24))

    base_vars <- c(
      "TOKEN_PERSON_ID",
      "EPIKEY",
      "EPISTART",
      "EPIEND",
      "ADMIDATE",
      "DISDATE",
      "ADMIMETH",
      "DISMETH"
    )

    select_vars <- c(base_vars, diag_vars, opertn_vars, opdate_vars)
    actual_cols <- dbListFields(con, table_name)
    valid_vars <- intersect(select_vars, actual_cols)

    if (length(valid_vars) == 0) {
      message("No expected columns found in ", table_name)
      return(empty_apc())
    }

    tryCatch({
      data <- as.data.table(collect(dplyr::select(
        tbl(con, table_name), all_of(valid_vars)
      )))
      data <- copy(data)

      rename_map <- c(
        "TOKEN_PERSON_ID" = "pseudo",
        "EPIKEY" = "epi_key",
        "EPISTART" = "epi_start",
        "EPIEND" = "epi_end",
        "ADMIDATE" = "admi_date",
        "DISDATE" = "dis_date",
        "ADMIMETH" = "admi_meth",
        "DISMETH" = "dis_meth"
      )

      intersecting_vars <- intersect(names(data), names(rename_map))
      setnames(data, intersecting_vars, rename_map[intersecting_vars])

      for (i in 1:20) {
        old <- paste0("DIAG_", sprintf("%02d", i))
        new <- paste0("diag_", sprintf("%02d", i))
        if (old %in% names(data)) setnames(data, old, new)
      }

      for (i in 1:24) {
        op <- paste0("OPERTN_", sprintf("%02d", i))
        dt <- paste0("OPDATE_", sprintf("%02d", i))
        if (op %in% names(data)) {
          setnames(data, op, paste0("opertn_", sprintf("%02d", i)))
        }
        if (dt %in% names(data)) {
          setnames(data, dt, paste0("opdate_", sprintf("%02d", i)))
        }
      }

      date_cols <- c(
        "admi_date",
        "dis_date",
        "epi_start",
        "epi_end",
        grep("^opdate_", names(data), value = TRUE)
      )
      for (col in date_cols) {
        if (col %in% names(data)) data[[col]] <- as.Date(data[[col]])
      }

      data[, year := yr]

      if ("admi_date" %in% names(data) && "dis_date" %in% names(data)) {
        data[, length_of_stay := as.integer(dis_date - admi_date)]
      }
      if ("admi_meth" %in% names(data)) {
        data[, emergency_admission := admi_meth %in% c("21", "22", "23", "24", "25", "2A")]
      }
      if ("dis_meth" %in% names(data)) {
        data[, died_in_hospital := dis_meth == "4"]
      }

      data
    }, error = function(e) {
      message("Error in ", table_name, ": ", e$message)
      empty_apc()
    })
  }, future.seed = TRUE)

  rbindlist(results, fill = TRUE)
}

Admitted_Patient_Care <- get_apc(Year)
Admitted_Patient_Care <-
  Spine[Admitted_Patient_Care, on = .(pseudo, year), nomatch = NULL]

admi_meth_labels <- c(
  "11" = "Elective admission - waiting list",
  "12" = "Elective admission - booked",
  "13" = "Elective admission - planned",
  "21" = "Emergency admission - Emergency care department or acute or emergency dental service",
  "22" = "Emergency admission - General practitioner",
  "23" = "Emergency admission - Bed bureau",
  "24" = "Emergency admission - Consultant clinic",
  "25" = "Emergency admission - Mental health crisis resolution team",
  "2A" = "Emergency admission - Emergency care department of another provider",
  "2B" = "Emergency admission - Transfer of an admitted patient from another hospital provider",
  "2C" = "Emergency admission - Baby born at home as intended",
  "2D" = "Emergency admission - Other emergency admission",
  "28" = "Emergency admission - Other means",
  "31" = "Maternity admission - Admitted ante-partum",
  "32" = "Maternity admission - Admitted post-partum",
  "82" = "Other admission - The birth of a baby in this provider",
  "83" = "Other admission - Baby born outside of the provider except when born at home as intended",
  "81" = "Other admission - Transfer of any admitted patient from hospital provider other than in an emergency",
  "84" = "Other admission - Admission by admissions panel of a high security psychiatric hospital",
  "89" = "Other admission - HSPH admissions waiting list",
  "98" = "Other admission - Not applicable",
  "99" = "Other admission - Method not known"
)

dis_meth_labels <- c(
  "1" = "Patient discharged on clinical advice or with clinical consent",
  "2" = "Patient discharged him/herself or was discharged by a relative or advocate",
  "3" = "Patient discharged by a mental health review tribunal, Home Secretary, or court",
  "4" = "Patient died",
  "5" = "Stillbirth",
  "6" = "Patient discharged himself/herself",
  "7" = "Patient discharged by a relative or advocate",
  "8" = "Not applicable - hospital provider spell not finished at episode end or current episode unfinished",
  "9" = "Method of discharge not known"
)

Admitted_Patient_Care <- Admitted_Patient_Care |>
  mutate(
    admi_meth = factor(
      admi_meth,
      levels = names(admi_meth_labels),
      labels = unname(admi_meth_labels)
    ),
    dis_meth = factor(
      dis_meth,
      levels = names(dis_meth_labels),
      labels = unname(dis_meth_labels)
    )
  )

rm(get_apc, admi_meth_labels, dis_meth_labels)
gc()

write_parquet_labeled(Admitted_Patient_Care, "data/Admitted_Patient_Care")
