# Main analysis runner -----------------------------------------------------
#
# Run from the project root in the ONS Secure Research Service. The stages are
# deliberately explicit: the linked-data preparation is expensive and many
# researchers will rerun only downstream stages after the restricted parquet
# intermediates have been created.

source("R/00_setup.R")

# 1. Extract and construct pupil-year analytical source datasets.
source("R/01_prepare_dataset.R")

# 2. Join the source datasets into the final pupil-year analysis dataset.
source("R/02_build_analysis_dataset.R")

# 3. Descriptive tables used for checking and manuscript reporting.
source("R/03_descriptive_tables.R")

# 4. Final fixed-effects Poisson models and manuscript regression tables.
source("R/04_regression_models.R")

# 5. Descriptive figures.
source("R/05_figures.R")

# Optional analyses are intentionally not run by default:
# source("R/06_send_change.R")
# source("R/07_spatial_analysis.R")
