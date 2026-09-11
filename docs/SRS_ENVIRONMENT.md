# Running this analysis in the ONS Secure Research Service

This repository documents an analysis undertaken using the ECHILD Research
Database inside the ONS Secure Research Service (SRS). It is therefore not a
"clone and run" public-data repository.

## Air-gapped environment

The SRS does not provide ordinary internet access. In the working analysis,
R packages had to be obtained and installed using the package-transfer and
local-binary arrangements available in the project workspace at the time.
Those operational scripts were highly environment-specific and are not a
stable or useful dependency manager for a public repository.

The released `R/00_setup.R` instead checks that the required packages are
available and fails with an informative message. Researchers should install
packages using the current ONS-approved procedure for their workspace.

## Database configuration

Database server, driver, table and workspace details are not published. Copy
`config/config.example.R` to `config/config.R` inside the SRS and substitute
the identifiers appropriate to the ECHILD release available to the project.
The local configuration file is ignored by Git.

## Intermediate data

The preparation stage writes labelled parquet intermediates. This was useful
inside the SRS because repeated extraction from large linked administrative
tables is slow and unnecessary. These files contain restricted record-level
data and must never be committed to GitHub.

## Reproducibility outside the SRS

The public code is intended to make the cohort definitions, transformations,
models and output construction transparent and reusable. Exact numerical
reproduction requires approved access to the relevant ECHILD release and the
same source tables within a trusted research environment.
