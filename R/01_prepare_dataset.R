# Construct source datasets ------------------------------------------------
#
# Main runner for the computationally expensive preparation stage. The
# substantive transformations are split into ordered modules so that readers
# can follow each linked-data source separately. All generated parquet files
# contain restricted ECHILD data and must remain inside the SRS.

Year <- analysis_years
Term <- analysis_terms
Bridge_Table <- db_config$bridge_table

# Load the pupil bridge once. Individual preparation modules reuse this object
# until the cohort/SEND stage has finished with it.
bridge_con <- connect_to_database()
Bridge <- bridge_con |>
  dplyr::tbl(Bridge_Table) |>
  dplyr::collect() |>
  data.table::as.data.table()
DBI::dbDisconnect(bridge_con)
rm(bridge_con)

if ("ECHILD_PUPIL_REF_MATCHING_ANONYMOUS" %in% names(Bridge)) {
  data.table::setnames(
    Bridge,
    "ECHILD_PUPIL_REF_MATCHING_ANONYMOUS",
    "pupil_matching_ref_anonymous"
  )
}

preparation_scripts <- c(
  "R/preparation/01_cohort_send.R",
  "R/preparation/02_demographics.R",
  "R/preparation/03_education.R",
  "R/preparation/04_absence.R",
  "R/preparation/05_education_outcomes.R",
  "R/preparation/06_apc.R",
  "R/preparation/07_chronic_health.R",
  "R/preparation/08_adversity_related_injury.R",
  "R/preparation/09_stress_related_presentations.R"
)

for (script in preparation_scripts) {
  message("Running preparation module: ", script)
  source(script)
}

rm(preparation_scripts, Year, Term, Bridge_Table)
gc()
