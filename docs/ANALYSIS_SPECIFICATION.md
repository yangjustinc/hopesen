# Analysis specification represented by the released code

This document is a concise map from the public scripts to the completed
analysis. It is intended to help readers distinguish the substantive analysis
from SRS infrastructure code.

## Unit of analysis

The main analytical dataset is one row per pupil and analysis year. The cohort
is drawn from pupils in National Curriculum Years 7-11 identified through the
school census data used by the project.

## SEND exposure

Primary and secondary recorded SEN types are reduced to four broad domains:

- neurodivergent needs;
- social, emotional and/or mental health (SEMH) needs;
- sensory impairment and/or physical disability;
- other SEN(D).

These domains are then combined into nine mutually exclusive annual profiles,
with **No SEND** as the regression reference group. The profiles represent
recorded educational needs/provision, not clinical diagnoses.

## Outcomes

### Education

- overall sessions absent;
- authorised sessions absent;
- unauthorised sessions absent;
- any exclusion;
- any alternative-provision record;
- exclusion or alternative provision.

Absence data are unavailable for 2020 and 2021 in the source used by this
analysis and remain missing rather than being recoded to zero.

### Health

- any HES APC inpatient record;
- any emergency inpatient record;
- composite self-injury/suicide indicator derived from the two inpatient
  self-harm phenotypes constructed in `01_prepare_dataset.R` and registered
  suicide death.

## Covariates and model specification

The completed adjusted models include:

- gender;
- age at the start of the academic year;
- major ethnic group;
- free school meal eligibility;
- language group;
- IDACI;
- chronic-health indicator;
- analysis year fixed effects;
- school local authority fixed effects.

Standard errors are clustered by pupil.

Binary outcomes are fitted using Poisson regression with log link and reported
as exponentiated coefficients (risk ratios). Absence outcomes use the same
model family with `log(sessions_possible_6halfterms)` as an offset and are
reported as rate ratios.

## Time indexing

The public code retains the source-specific time indexing used in the completed
analysis. Education is organised around the school-census year variable;
health and mortality records are assigned using the annual/date fields
available in their respective source datasets. Users adapting the code should
not assume that a shared `year` variable implies identical source-period
boundaries without checking the ECHILD release documentation.

## Relationship to earlier working code

Earlier exploratory negative-binomial specifications are not part of the
released execution path. See `ANALYSIS_HISTORY.md`.
