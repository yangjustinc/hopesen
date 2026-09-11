# Project setup ------------------------------------------------------------
#
# Loads the packages used by the released analysis, defines lightweight I/O
# helpers, and reads the project-specific SRS configuration. Package
# installation is intentionally NOT automated here: the ONS SRS is air-gapped
# and package availability/install procedures vary by workspace and date.
# See docs/SRS_ENVIRONMENT.md.

required_packages <- c(
  "arrow", "data.table", "DBI", "dbplyr", "dplyr", "fixest",
  "flextable", "future", "future.apply", "ggplot2", "labelled",
  "lubridate", "modelsummary", "odbc", "officer", "purrr", "sf",
  "spdep", "stringr", "table1", "tidyr", "tmap", "janitor",
  "Hmisc", "cowplot"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required packages: ", paste(missing_packages, collapse = ", "),
    ". Install/transfer these packages using your approved SRS procedure."
  )
}

suppressPackageStartupMessages({
  invisible(lapply(required_packages, library, character.only = TRUE))
})

if (!file.exists("config/config.R")) {
  stop(
    "config/config.R was not found. Copy config/config.example.R to ",
    "config/config.R and edit the SRS-specific values before running."
  )
}
source("config/config.R")

# The original analysis was run sequentially in the SRS. Keeping the default
# conservative avoids spawning more database connections than intended.
future::plan(future::sequential)

write_parquet_labeled <- function(df, path, compression = "zstd") {
  stopifnot(is.data.frame(df), is.character(path), length(path) == 1L)

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  parquet_path <- paste0(path, ".parquet")
  labels_path <- paste0(path, ".labels.rds")

  var_labels <- lapply(df, function(x) attr(x, "label", exact = TRUE))
  factor_info <- lapply(df, function(x) {
    if (is.factor(x)) list(levels = levels(x), ordered = is.ordered(x)) else NULL
  })

  meta <- list(
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    nrow = nrow(df), ncol = ncol(df), names = names(df),
    var_labels = var_labels, factor_info = factor_info
  )

  arrow::write_parquet(df, parquet_path, compression = compression)
  saveRDS(meta, labels_path, compress = TRUE)
  invisible(list(parquet = parquet_path, labels = labels_path))
}

read_parquet_labeled <- function(path) {
  stopifnot(is.character(path), length(path) == 1L)
  df <- arrow::read_parquet(paste0(path, ".parquet"))
  labels_path <- paste0(path, ".labels.rds")

  if (!file.exists(labels_path)) return(df)
  meta <- readRDS(labels_path)

  for (nm in intersect(names(meta$factor_info), names(df))) {
    fi <- meta$factor_info[[nm]]
    if (!is.null(fi)) {
      values <- as.character(df[[nm]])
      df[[nm]] <- if (isTRUE(fi$ordered)) {
        ordered(values, levels = fi$levels)
      } else {
        factor(values, levels = fi$levels)
      }
    }
  }

  for (nm in intersect(names(meta$var_labels), names(df))) {
    lab <- meta$var_labels[[nm]]
    if (!is.null(lab) && length(lab) == 1L && !is.na(lab)) {
      attr(df[[nm]], "label") <- lab
    }
  }
  df
}

connect_to_database <- function(config = db_config) {
  DBI::dbConnect(
    odbc::odbc(),
    Driver = config$driver,
    Server = config$server,
    Database = config$database,
    Trusted_connection = config$trusted_connection
  )
}

load_if_missing <- function(object_name, path) {
  if (!exists(object_name, envir = .GlobalEnv, inherits = FALSE)) {
    assign(object_name, read_parquet_labeled(path), envir = .GlobalEnv)
  }
  invisible(get(object_name, envir = .GlobalEnv, inherits = FALSE))
}

assert_unique_pupil_year <- function(x, name = deparse(substitute(x))) {
  x <- data.table::as.data.table(x)
  n_dup <- data.table::anyDuplicated(
    x, by = c("pupil_matching_ref_anonymous", "year")
  )
  if (n_dup != 0L) stop(name, " is not unique by pupil-year.")
  invisible(TRUE)
}
