# Changelog

All notable changes to the public analytical code are documented here.

## Unreleased

## 1.0.0 - 2026-09-11

### Added

- ECHILD-style public repository structure with explicit `_main.R` runner.
- Modular linked-data preparation scripts for cohort/SEND, demographics,
  education, absence, exclusions/AP, admitted patient care, chronic health,
  adversity-related injury, and stress-related presentations.
- Functionalised fixed-effects Poisson modelling helpers.
- Data-free R syntax parsing and synthetic QA tests on every push and pull
  request.
- Synthetic tests for SEND classification and the locked model specification.
- Documentation for the analytical specification, SRS/air-gapped environment,
  analysis history, phenotype codelists, and quality-assurance approach.
- `CITATION.cff` metadata for GitHub/Zenodo citation and archiving.

### Changed

- Refactored the completed working analysis into a pedagogical and reusable
  implementation while preserving the final June 2026 analytical
  specification.
- Removed superseded exploratory negative-binomial modelling from the public
  execution path.
- Corrected non-substantive table/export issues identified during audit,
  including the omitted `Neurodivergent + other` coefficient-map key.
- Made the completed adversity-related-injury behaviour explicit rather than
  retaining a misleading no-op emergency-admission filter.

## 0.1.0 - 2026-09-11

Initial public repository scaffold and audited analytical workflow.
