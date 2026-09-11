# Build final pupil-year analysis dataset ----------------------------------
#
# Joins the restricted source datasets created in 01_prepare_dataset.R onto
# the SEND pupil-year spine. Where source tables contain multiple records per
# pupil-year, the aggregation/selection rules below mirror the completed
# analysis. The final object is written as data/Composite_Data.parquet.

Spine <- read_parquet_labeled("data/Spine")
setDT(Spine)
stopifnot(anyDuplicated(Spine, by = c("pupil_matching_ref_anonymous", "year")) == 0)
Composite_Data <-
  Spine[, .(pupil_matching_ref_anonymous, year, sen_indicator)]
rm(Spine)
gc()

Demographics <- read_parquet_labeled("data/Demographics")
setDT(Demographics)
anyDuplicated(Demographics, by = c("pupil_matching_ref_anonymous", "year"))
demo_keep <-
  c(
    "pupil_matching_ref_anonymous",
    "year",
    "gender",
    "age_at_start_of_academic_year",
    "ethnic_group_major",
    "ethnic_group_minor",
    "fsm_eligible",
    "language_group",
    "home_local_authority",
    "imd",
    "idaci",
    "i.term"
  )
demo_keep <- intersect(demo_keep, names(Demographics))
Demographics <- Demographics[, ..demo_keep]
setorderv(
  Demographics,
  c("pupil_matching_ref_anonymous", "year", "i.term"),
  order = c(1, 1, -1),
  na.last = TRUE
)
Demographics <-
  Demographics[!duplicated(Demographics, by = c("pupil_matching_ref_anonymous", "year"))]

key <- c("pupil_matching_ref_anonymous", "year")
dem_cols <- setdiff(names(Demographics), c(key, "i.term"))
Composite_Data[Demographics, (dem_cols) := mget(paste0("i.", dem_cols)), on = key]

rm(Demographics, demo_keep)
gc()

Education <- read_parquet_labeled("data/Education")
setDT(Education)
edu_keep <- c(
  "pupil_matching_ref_anonymous",
  "year",
  "i.term",
  "nc_year_actual",
  "enrolment_status",
  "part_time",
  "phase",
  "sen_indicator",
  "sen_provision",
  "no_send",
  "neurodivergent",
  "socioemotional",
  "impairment",
  "other",
  "slcn",
  "hi",
  "vi",
  "msi",
  "asd",
  "oth",
  "semh",
  "nsa",
  "sen_unit",
  "resourced_provision",
  "school_local_authority_code"
)
edu_keep <- intersect(edu_keep, names(Education))
Education <- Education[, ..edu_keep]

Education[, term_rank := fifelse(`i.term` == "Spring",
                                 1L,
                                 fifelse(`i.term` == "Summer", 2L,
                                         fifelse(`i.term` == "Autumn", 3L, 4L)))]
setorderv(Education,
          c("pupil_matching_ref_anonymous", "year", "term_rank"))
Education <-
  Education[!duplicated(Education, by = c("pupil_matching_ref_anonymous", "year"))]

key <- c("pupil_matching_ref_anonymous", "year")
edu_cols <- setdiff(names(Education), c(key, "i.term", "term_rank"))
Composite_Data[Education, (edu_cols) := mget(paste0("i.", edu_cols)), on = key]

rm(Education, edu_keep, key, edu_cols)
gc()

Composite_Data[, code := school_local_authority_code]

Absences <- read_parquet_labeled("data/Absences")
setDT(Absences)

key <- c("pupil_matching_ref_anonymous", "year")
abs_keep <- c(
  key,
  "i.term",
  "authorised_absence_6halfterms",
  "unauthorised_absence_6halfterms",
  "overall_absence_6halfterms",
  "sessions_possible_6halfterms",
  "absence_rate_6halfterms"
)
abs_keep <- intersect(abs_keep, names(Absences))
Absences <- Absences[, ..abs_keep]

Absences <- Absences[!duplicated(Absences, by = key)]

abs_cols <- setdiff(names(Absences), key)
Composite_Data[Absences, (abs_cols) := mget(paste0("i.", abs_cols)), on = key]

rm(Absences, abs_keep, key, abs_cols)
gc()

Admitted_Patient_Care <-
  read_parquet_labeled("data/Admitted_Patient_Care")
setDT(Admitted_Patient_Care)

key <- c("pupil_matching_ref_anonymous", "year")
apc_keep <-
  intersect(
    c(
      key,
      "emergency_admission",
      "died_in_hospital",
      "length_of_stay"
    ),
    names(Admitted_Patient_Care)
  )
Admitted_Patient_Care <- Admitted_Patient_Care[, ..apc_keep]
str(Admitted_Patient_Care)

Admitted_Patient_Care <- Admitted_Patient_Care[,
                                               .(
                                                 apc_record_n = .N,
                                                 apc_emergency_n = sum(emergency_admission == TRUE, na.rm = TRUE),
                                                 apc_any_emergency = as.integer(any(emergency_admission == TRUE, na.rm = TRUE)),
                                                 apc_any_died_in_hospital = as.integer(any(died_in_hospital == TRUE, na.rm = TRUE)),
                                                 apc_total_los = sum(length_of_stay, na.rm = TRUE)
                                               ),
                                               by = key]

apc_cols <- setdiff(names(Admitted_Patient_Care), key)

Composite_Data[Admitted_Patient_Care,
               (apc_cols) := mget(paste0("i.", apc_cols)),
               on = key]

for (j in apc_cols) {
  set(Composite_Data, which(is.na(Composite_Data[[j]])), j, 0L)
}

rm(Admitted_Patient_Care, apc_keep, key, apc_cols)
gc()

Chronic_Health_Conditions <-
  read_parquet_labeled("data/Chronic_Health_Conditions")
setDT(Chronic_Health_Conditions)

key <- c("pupil_matching_ref_anonymous", "year")
chc_cols <- intersect(
  c(
    "musculoskeletal_skin",
    "mental_health_behavioural",
    "metabolic_endocrine_digestive_renal_genitourinary",
    "cardiovascular",
    "neurological",
    "cancer_blood",
    "nonspecific",
    "respiratory",
    "chronic_infections"
  ),
  names(Chronic_Health_Conditions)
)

Chronic_Health_Conditions <- Chronic_Health_Conditions[,
                                                       lapply(.SD, function(x)
                                                         as.integer(any(x == 1, na.rm = TRUE))),
                                                       by = key,
                                                       .SDcols = chc_cols]
setnames(Chronic_Health_Conditions, chc_cols, paste0("chc_", chc_cols))

chc_out <- setdiff(names(Chronic_Health_Conditions), key)
Composite_Data[Chronic_Health_Conditions,
               (chc_out) := mget(paste0("i.", chc_out)),
               on = key]

for (j in chc_out) {
  set(Composite_Data, which(is.na(Composite_Data[[j]])), j, 0L)
}

rm(Chronic_Health_Conditions, chc_out, key, chc_cols)
gc()

Adversity_Related_Injury <-
  read_parquet_labeled("data/Adversity_Related_Injury")
setDT(Adversity_Related_Injury)

key <- c("pupil_matching_ref_anonymous", "year")
ari_cols <- intersect(c("self_harm",
                        "drug_alc",
                        "violence"),
                      names(Adversity_Related_Injury))

Adversity_Related_Injury <- Adversity_Related_Injury[,
                                                     lapply(.SD, function(x)
                                                       as.integer(any(x == 1, na.rm = TRUE))),
                                                     by = key,
                                                     .SDcols = ari_cols]
setnames(Adversity_Related_Injury, ari_cols, paste0("ari_", ari_cols))

ari_out <- setdiff(names(Adversity_Related_Injury), key)

Composite_Data[Adversity_Related_Injury,
               (ari_out) := mget(paste0("i.", ari_out)),
               on = key]

for (j in ari_out) {
  set(Composite_Data, which(is.na(Composite_Data[[j]])), j, 0L)
}

rm(Adversity_Related_Injury, ari_out, key, ari_cols)
gc()

Stress_Related_Presentation <-
  read_parquet_labeled("data/Stress_Related_Presentation")
setDT(Stress_Related_Presentation)

key <- c("pupil_matching_ref_anonymous", "year")
srp_cols <- intersect(
  c(
    "self_harm",
    "internalising",
    "thought_disorder",
    "potentially_psych",
    "externalising"
  ),
  names(Stress_Related_Presentation)
)

Stress_Related_Presentation <- Stress_Related_Presentation[,
                                                           lapply(.SD, function(x)
                                                             as.integer(any(x == 1, na.rm = TRUE))),
                                                           by = key,
                                                           .SDcols = srp_cols]
setnames(Stress_Related_Presentation,
         srp_cols,
         paste0("srp_", srp_cols))

srp_out <- setdiff(names(Stress_Related_Presentation), key)

Composite_Data[Stress_Related_Presentation,
               (srp_out) := mget(paste0("i.", srp_out)),
               on = key]

for (j in srp_out) {
  set(Composite_Data, which(is.na(Composite_Data[[j]])), j, 0L)
}

rm(Stress_Related_Presentation, srp_out, key, srp_cols)
gc()

Suicide <-
  read_parquet_labeled("data/Suicide")
setDT(Suicide)

key <- c("pupil_matching_ref_anonymous", "year")
sui_cols <- intersect(c("suicide"),
                      names(Suicide))

Suicide <- Suicide[,
                   lapply(.SD, function(x)
                     as.integer(any(x == 1, na.rm = TRUE))),
                   by = key,
                   .SDcols = sui_cols]
setnames(Suicide, sui_cols, paste0("sui_", sui_cols))

sui_out <- setdiff(names(Suicide), key)

Composite_Data[Suicide,
               (sui_out) := mget(paste0("i.", sui_out)),
               on = key]

for (j in sui_out) {
  set(Composite_Data, which(is.na(Composite_Data[[j]])), j, 0L)
}

rm(Suicide, sui_out, key, sui_cols)
gc()

Exclusions <-
  read_parquet_labeled("data/Exclusions")
setDT(Exclusions)

key <- c("pupil_matching_ref_anonymous", "year")
str(Exclusions[, .(sessions_excluded)])
anyDuplicated(Exclusions, by = key)

Exclusions <- Exclusions[,
                         .(
                           exclusion_record_n = .N,
                           exclusions_sessions = sum(sessions_excluded, na.rm = TRUE),
                           any_exclusion = as.integer(any(sessions_excluded > 0, na.rm = TRUE))
                         ),
                         by = key]

exc_out <- setdiff(names(Exclusions), key)

Composite_Data[Exclusions,
               (exc_out) := mget(paste0("i.", exc_out)),
               on = key]

for (j in exc_out) {
  set(Composite_Data, which(is.na(Composite_Data[[j]])), j, 0L)
}

rm(Exclusions, exc_out, key)
gc()

Alternative_Provision <-
  read_parquet_labeled("data/Alternative_Provision")
setDT(Alternative_Provision)

key <- c("pupil_matching_ref_anonymous", "year")
anyDuplicated(Alternative_Provision, by = key)

Alternative_Provision <- Alternative_Provision[,
                                               .(
                                                 pupil_matching_ref_anonymous,
                                                 year,
                                                 any_alternative_provision = as.integer(alternative_provision == 1)
                                               )]

Composite_Data[Alternative_Provision,
               any_alternative_provision := i.any_alternative_provision,
               on = key]

Composite_Data[is.na(any_alternative_provision), any_alternative_provision := 0L]

rm(Alternative_Provision, key)
gc()

nrow(Composite_Data)
anyDuplicated(Composite_Data, by = c("pupil_matching_ref_anonymous", "year"))
ncol(Composite_Data)

write_parquet_labeled(Composite_Data, "data/Composite_Data")
