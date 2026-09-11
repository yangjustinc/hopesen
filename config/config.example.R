# Project-specific configuration -------------------------------------------
# Copy this file to config/config.R inside the ONS Secure Research Service
# and edit the values there. config/config.R is ignored by Git so that
# infrastructure details are not published accidentally.

analysis_years <- 2015:2022
analysis_terms <- "Spring"

# The names below are deliberately placeholders. ECHILD table/database names
# and connection details depend on the SRS release and project workspace.
db_config <- list(
  driver = "<ODBC driver>",
  server = "<server>",
  database = "<database>",
  trusted_connection = "<trusted connection setting>",
  bridge_table = "<ECHILD pupil bridge table>"
)

# Local project directories. Relative paths are preferred because they make
# the public code portable while still working inside an air-gapped SRS.
project_paths <- list(
  data = "data",
  outputs = "outputs",
  geography = "geography"
)
