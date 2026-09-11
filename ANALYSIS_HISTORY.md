# Analysis history

This repository is a cleaned, documented implementation of the final analysis,
not a chronological dump of every exploratory script used during the project.

The analysis developed in three broad phases:

1. **Dataset construction and descriptive exploration.** Pupil-year source
   datasets were extracted and combined across education, absence, exclusions,
   alternative provision, HES APC, self-harm phenotypes, chronic health and
   registered deaths.
2. **Earlier modelling.** Exploratory analyses included negative-binomial
   specifications for some count outcomes. Those models were subsequently
   superseded and are deliberately not included in the public execution path.
3. **Final analysis.** The manuscript analyses use fixed-effects Poisson models
   with pupil-clustered standard errors. Binary outcomes are reported as risk
   ratios and absence counts as rate ratios using the number of possible
   sessions as an offset.

The public implementation has been refactored for readability and reuse:
repeated modelling/export operations are functionalised, environment-specific
SRS details are moved into configuration/documentation, and obsolete scripts
are omitted. These changes are organisational; they are not intended to alter
the completed analytical specification.
