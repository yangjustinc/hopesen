# Adversity-related injury phenotype --------------------------------------
# Applies the published Herbert phenotype. The completed analysis used
# qualifying APC episodes without an additional emergency-only restriction.

ARI_Codelist <- read_csv("codelists/ari_herbert_v1.csv") |>
  dplyr::mutate(code = str_replace(code, "[[:punct:]]", ""))

Adversity_Related_Injury <- Admitted_Patient_Care |>
  dplyr::select(
    pseudo,
    pupil_matching_ref_anonymous,
    epi_key,
    admi_meth,
    year,
    diag_01, diag_02, diag_03, diag_04, diag_05,
    diag_06, diag_07, diag_08, diag_09, diag_10,
    diag_11, diag_12, diag_13, diag_14, diag_15,
    diag_16, diag_17, diag_18, diag_19, diag_20,
    length_of_stay
  ) |>
  # The completed analysis applied the ARI phenotype across qualifying APC
  # episodes; it did not impose an emergency-admission restriction here.
  tidyr::drop_na() |>
  tidyr::pivot_longer(cols = starts_with("diag_"), values_to = "code") |>
  dplyr::select(-name) |>
  dplyr::filter(str_detect(code, "^[[:alpha:]].+")) |>
  dplyr::mutate(code = str_replace(code, "[[:punct:]]", "")) |>
  inner_join(ARI_Codelist) |>
  dplyr::select(
    pseudo,
    pupil_matching_ref_anonymous,
    epi_key,
    year,
    group
  ) |>
  dplyr::distinct() |>
  dplyr::mutate(value = 1) |>
  pivot_wider(names_from = group, values_from = value) |>
  dplyr::rename(self_harm = "self-harm") |>
  mutate(across(everything(), ~ replace_na(., 0))) |>
  group_by(pseudo, pupil_matching_ref_anonymous, year, epi_key) |>
  summarise(
    self_harm = max(self_harm),
    drug_alc = max(drug_alc),
    violence = max(violence)
  ) |>
  ungroup() |>
  left_join(Spine)

rm(ARI_Codelist)

adversity_related_injury_labels <- c(
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
  self_harm = "Self-Harm",
  drug_alc = "Drug or Alcohol Misuse",
  violence = "Violence"
)

label(Adversity_Related_Injury) <-
  as.list(adversity_related_injury_labels[match(
    names(Adversity_Related_Injury), names(adversity_related_injury_labels)
  )])
rm(adversity_related_injury_labels)

write_parquet_labeled(
  Adversity_Related_Injury,
  "data/Adversity_Related_Injury"
)
