# Chronic-health phenotype -------------------------------------------------
# Applies the published Hardelid chronic-condition codelist to prepared APC
# diagnoses and retains the phenotype domains used in the completed analysis.

CHC_Codelist <- read_csv("codelists/chc_hardelid_v1.csv") |>
  dplyr::mutate(code = str_replace(code, "[[:punct:]]", ""))

Chronic_Health_Conditions <- Admitted_Patient_Care |>
  dplyr::select(
    pseudo,
    pupil_matching_ref_anonymous,
    epi_key,
    year,
    diag_01, diag_02, diag_03, diag_04, diag_05,
    diag_06, diag_07, diag_08, diag_09, diag_10,
    diag_11, diag_12, diag_13, diag_14, diag_15,
    diag_16, diag_17, diag_18, diag_19, diag_20,
    length_of_stay
  ) |>
  tidyr::drop_na() |>
  tidyr::pivot_longer(cols = starts_with("diag_"), values_to = "code") |>
  dplyr::select(-name) |>
  dplyr::filter(str_detect(code, "^[[:alpha:]].+")) |>
  dplyr::mutate(code = str_replace(code, "[[:punct:]]", "")) |>
  inner_join(CHC_Codelist) |>
  dplyr::mutate(
    valid = case_when(
      str_detect(flag, "los3") & length_of_stay < 3 ~ 0,
      TRUE ~ 1
    )
  ) |>
  dplyr::filter(valid == 1) |>
  dplyr::select(
    pseudo,
    pupil_matching_ref_anonymous,
    epi_key,
    year,
    group,
    valid
  ) |>
  dplyr::distinct() |>
  pivot_wider(names_from = group, values_from = valid, values_fill = 0) |>
  dplyr::rename(
    musculoskeletal_skin = "musculoskeletal/skin",
    mental_health_behavioural = "mental health/behavioural",
    metabolic_endocrine_digestive_renal_genitourinary = "metabolic/endocrine/digestive/renal/genitourinary",
    cancer_blood = "cancer/blood",
    nonspecific = "codes indicating non-specific chronic condition",
    chronic_infections = "chronic infections"
  ) |>
  group_by(pseudo, pupil_matching_ref_anonymous, year, epi_key) |>
  summarise(
    musculoskeletal_skin = max(musculoskeletal_skin),
    mental_health_behavioural = max(mental_health_behavioural),
    metabolic_endocrine_digestive_renal_genitourinary = max(metabolic_endocrine_digestive_renal_genitourinary),
    cardiovascular = max(cardiovascular),
    neurological = max(neurological),
    cancer_blood = max(cancer_blood),
    nonspecific = max(nonspecific),
    respiratory = max(respiratory),
    chronic_infections = max(chronic_infections)
  ) |>
  ungroup() |>
  left_join(Spine)

rm(CHC_Codelist)

chronic_health_conditions_labels <- c(
  year = "School Year",
  term = "Term",
  nc_year_actual = "National Curriculum Year",
  spld = "Specific Learning Difficulty",
  mld = "Moderate Learning Difficulty",
  sld = "Severe Learning Difficulty",
  pmld = "Profound & Multiple Learning Difficulty",
  besd = "Behaviour, Emotional, and Social Difficulties",
  slcn = "Speech, Language, and Communication Needs",
  hi = "Hearing Impairment",
  vi = "Visual Impairment",
  msi = "Multi-Sensory Impairment",
  pd = "Physical Disability",
  asd = "Autism Spectrum Disorder",
  oth = "Other Difficulty/Disability",
  semh = "Social, Emotional, and Mental Health",
  nsa = "SEN support but no specialist assessment of type of need",
  ds = "Down Syndrome",
  neurodivergent = "Neurodivergent SEN",
  socioemotional = "Social, Emotional, and/or Mental Health",
  impairment = "Sensory Impairment(s) and/or Physical Disability",
  other = "Other SEN(D)",
  no_send = "No SEND",
  sen_indicator = "Type of SEN(D)s",
  musculoskeletal_skin = "Musc/Skin",
  mental_health_behavioural = "Mental/Behavioural",
  metabolic_endocrine_digestive_renal_genitourinary = "Met/Ren/Dig/End/GU",
  cardiovascular = "Cardiac",
  neurological = "Neuro/sens",
  cancer_blood = "Cancer/blood",
  nonspecific = "Non-specific",
  respiratory = "Resp",
  chronic_infections = "Chronic infection"
)

label(Chronic_Health_Conditions) <-
  as.list(chronic_health_conditions_labels[match(
    names(Chronic_Health_Conditions), names(chronic_health_conditions_labels)
  )])
rm(chronic_health_conditions_labels)

write_parquet_labeled(
  Chronic_Health_Conditions,
  "data/Chronic_Health_Conditions"
)
