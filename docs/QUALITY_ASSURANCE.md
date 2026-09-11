# Quality assurance

This repository follows a proportionate quality-assurance approach informed by
the UK Government Analysis Function **Quality Assurance of Code for Analysis
and Research** guidance (the "Duck Book"). The project is analytical code rather
than a deployable software service, and it operates against restricted ECHILD
data inside the ONS Secure Research Service (SRS), so some forms of public
end-to-end testing are neither possible nor appropriate.

## What is automated publicly

A single dependency-light GitHub Actions quality-assurance workflow runs on
every push and pull request and performs two checks:

1. **R syntax parsing** parses every `.R` file in the repository. This catches
   syntax errors without requiring restricted data or SRS-only dependencies.
2. **R synthetic tests** exercise high-risk reusable logic using entirely
   synthetic inputs and base R. Current tests cover:
   - assignment of the nine mutually exclusive SEND profiles, including
     residual multi-domain combinations and invalid input handling;
   - the locked fixed-effects Poisson model formula and adjustment set;
   - the sessions-possible offset used for absence rate models; and
   - complete mapping of all eight non-reference SEND coefficients, including
     the `Neurodivergent + other` category.

The test suite intentionally does **not** connect to ECHILD or contain any
record-level, aggregate-disclosive, or derived confidential data.

## Structural QA

The public implementation has been refactored from the working SRS scripts to
make the analytical workflow easier to review and reuse. In particular:

- `_main.R` provides an explicit execution order;
- expensive source extraction is separated from downstream analysis;
- linked-data preparation is divided into ordered source/phenotype modules;
- reusable model logic is functionalised rather than duplicated;
- project-specific database/table settings live in `config/config.R`, which is
  excluded from version control;
- raw/restricted data and generated outputs are excluded from the repository;
- outputs are intended to be disposable and regenerated from the pipeline;
- filenames are descriptive, machine-readable, and ordered where sequence
  matters; and
- non-obvious analytical and SRS-specific decisions are documented close to
  the code and in `docs/`.

## QA that remains inside the SRS

Public continuous integration cannot validate database schemas, linkage,
record counts, disclosure controls, or full end-to-end execution against ECHILD.
Within the SRS, analysts should therefore additionally verify:

- source table and column names against the ECHILD release available to the
  project;
- expected row counts and uniqueness at major pupil/pupil-year checkpoints;
- successful creation and re-reading of labelled parquet intermediates;
- model sample sizes and reference categories;
- agreement of regenerated tables/figures with the assured analytical outputs;
  and
- disclosure-control requirements before exporting any analytical result.

## Scope of the public tests

The synthetic tests are deliberately risk-based rather than an attempt to test
every line. They prioritise transformations where a small coding error could
silently change exposure classification or the manuscript model specification.
This is consistent with the Duck Book principle that testing effort should be
proportionate to analytical risk.

## Release QA

Before a tagged release, the intended checks are:

- the GitHub Actions quality-assurance workflow passes on the release commit;
- `CITATION.cff`, `DESCRIPTION`, and release version agree;
- no `config/config.R`, restricted data, analytical output, credentials, or SRS
  connection details are tracked;
- the README and analysis specification describe the current execution order;
  and
- the release is archived through Zenodo to provide a persistent DOI.
