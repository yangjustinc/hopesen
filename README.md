# SEND provision profiles and secondary-school outcomes in ECHILD

R code for population-based analyses of recorded special educational needs and
disabilities (SEND) provision profiles and education and health outcomes among
secondary-school pupils in England using the ECHILD Research Database.

> **Status:** analysis complete; version 1.0.0 prepared to support manuscript
> submissions and reuse by other ECHILD researchers.

## Project overview

The project uses linked education, hospital and mortality records to construct
annual pupil-level SEND provision profiles and examine heterogeneity in school
absence, inpatient hospitalisation, exclusions, alternative provision, and
self-injury/suicide outcomes.

The public repository is a **cleaned and pedagogical implementation of the
completed analysis**. It preserves the final analytical specification while
functionalising repetitive code and separating project logic from the
ONS Secure Research Service (SRS) infrastructure details.

## Data source

The analyses use the [ECHILD Research Database](https://www.echild.ac.uk/),
which links administrative education and health data for children and young
people in England. Record-level data cannot be distributed with this
repository and are accessed only within an approved trusted research
environment.

ECHILD Research Database release citation:

> University College London. *Education and Child Health Insights from Linked
> Data - England*. ONS Secure Research Service Metadata Catalogue.
> https://doi.org/10.57906/j1gr-gm30

## Analysis population and exposure

The analysis creates a pupil-year cohort of pupils in National Curriculum
Years 7-11 and classifies recorded SEND needs into mutually exclusive annual
profiles:

1. No SEND
2. Neurodivergent SEN only
3. Social, emotional and/or mental health (SEMH) only
4. Sensory impairment and/or physical disability only
5. Other SEN(D) only
6. Neurodivergent + SEMH
7. Neurodivergent + sensory/physical
8. Neurodivergent + other
9. Any other combination

These are administrative provision/need profiles and should not be interpreted
as clinical diagnoses.

## Main outcomes

The released workflow constructs and analyses:

- overall, authorised and unauthorised school absence;
- any inpatient and emergency inpatient admission;
- exclusion, alternative provision, and a combined exclusion/AP indicator;
- a composite self-injury/suicide indicator derived from inpatient self-harm
  phenotypes and registered suicide deaths.

## Repository structure

```text
.
├── .github/workflows/r-syntax.yml  # syntax parsing + synthetic QA
├── _main.R                         # explicit analysis runner
├── codelists/
│   └── README.md                   # required public phenotype definitions
├── config/
│   └── config.example.R            # copy to config.R inside the SRS
├── R/
│   ├── 00_setup.R                  # packages, config and I/O helpers
│   ├── 01_prepare_dataset.R        # preparation-stage runner
│   ├── preparation/                # source-specific cohort/outcome modules
│   ├── 02_build_analysis_dataset.R # construct final pupil-year dataset
│   ├── 03_descriptive_tables.R     # descriptive outputs
│   ├── 04_regression_models.R      # final manuscript models
│   ├── 05_figures.R                # descriptive figures
│   ├── 06_send_change.R            # optional SEND-change analysis
│   ├── 07_spatial_analysis.R       # optional spatial analyses
│   ├── functions_models.R          # reusable modelling helpers
│   └── functions_send.R            # testable SEND classification helper
├── tests/
│   └── run_synthetic_tests.R       # data-free risk-based QA
├── docs/
│   ├── ANALYSIS_SPECIFICATION.md   # concise analytical map
│   ├── QUALITY_ASSURANCE.md        # public/SRS QA boundary
│   └── SRS_ENVIRONMENT.md          # air-gap and reproducibility notes
├── ANALYSIS_HISTORY.md
├── CHANGELOG.md
├── CITATION.cff
├── DESCRIPTION                     # dependency and project metadata
├── hopesen.Rproj
└── LICENSE
```

## Running the analysis

Inside an approved ECHILD SRS project workspace:

1. Copy `config/config.example.R` to `config/config.R` and enter the relevant
   database/table details for the ECHILD release available to the project.
2. Transfer the three public phenotype codelist CSVs listed in
   `codelists/README.md` into the project.
3. Make the required R packages available using the current ONS-approved
   package installation/transfer procedure.
4. Run `_main.R` from the project root, or run numbered stages individually.

The preparation stage is deliberately separate from downstream analysis. It
creates restricted parquet intermediates so that expensive SQL extraction does
not need to be repeated every time a table or model is regenerated.

## Statistical analysis

Final models are Poisson regressions with log link. For binary outcomes,
exponentiated coefficients are interpreted as risk ratios; for absence counts,
they are rate ratios with the log of possible school sessions as an offset.
Models include academic year and school local authority fixed effects and use
standard errors clustered by pupil.

The adjusted model covariates in the completed analysis are gender, age,
ethnic group, free school meal eligibility, language group, IDACI and a
chronic-health indicator.

See `docs/ANALYSIS_SPECIFICATION.md` for a concise analytical map and
`ANALYSIS_HISTORY.md` for the distinction between the final specification and
superseded exploratory modelling.

## Quality assurance

The repository uses a dependency-light GitHub Actions workflow to parse all R
files and run synthetic, data-free checks of the highest-risk reusable logic.
These tests cover the nine SEND profiles and the locked manuscript model
specification without exposing or simulating record-level ECHILD data.

Full end-to-end validation against database schemas, linked records, model
sample sizes and disclosure-controlled outputs necessarily remains within the
SRS. See `docs/QUALITY_ASSURANCE.md` for the public/SRS QA boundary and the
proportionate approach informed by the UK Government Analysis Function's
Quality Assurance of Code guidance.

## Reuse

The code is intended to be useful beyond the accompanying manuscripts. In
particular, the SEND-profile construction, labelled parquet helpers and
functionalised fixed-effects Poisson workflow may be adapted for other ECHILD
projects. Researchers should check variable/table names against the ECHILD
release available in their own workspace rather than assuming that SRS schemas
remain unchanged.

## Disclosure and reproducibility

No record-level ECHILD data, identifiers, SRS connection details or analytical
outputs requiring disclosure control are included. Exact numerical
reproduction requires authorised access to the relevant ECHILD data in a
trusted research environment.

The ONS providing access to the SRS does not imply its acceptance of the
validity of the code, methods or conclusions.

## Licence and citation

Code is released under the MIT License. Citation metadata, including author
ORCID and affiliation, are provided in `CITATION.cff`. Tagged releases are
intended to be archived through Zenodo for persistent citation.
