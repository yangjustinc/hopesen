# Stress-related presentation phenotype ----------------------------------
# Applies the published Ni Chobhthaigh phenotype to emergency APC episodes,
# including its diagnosis-position and medical/surgical exclusion logic.

SRP_Codelist <- read_csv("codelists/srp_nichobhthaigh_v2.csv") |>
  dplyr::mutate(code = str_replace(code, "[[:punct:]]", ""))

Stress_Related_Presentation_identifiers <-
  Admitted_Patient_Care |>
  dplyr::select(
    pseudo,
    pupil_matching_ref_anonymous,
    epi_key,
    emergency_admission,
    year,
    length_of_stay
  ) |>
  dplyr::filter(emergency_admission == TRUE) |>
  distinct()

Stress_Related_Presentation_diag <-
  Admitted_Patient_Care |>
  dplyr::select(
    pseudo,
    pupil_matching_ref_anonymous,
    epi_key,
    diag_01, diag_02, diag_03, diag_04, diag_05,
    diag_06, diag_07, diag_08, diag_09, diag_10,
    diag_11, diag_12, diag_13, diag_14, diag_15,
    diag_16, diag_17, diag_18, diag_19, diag_20
  ) |>
  tidyr::pivot_longer(
    cols = starts_with("diag_"),
    names_to = "diag_name",
    values_to = "code",
    values_drop_na = TRUE
  ) |>
  dplyr::filter(str_detect(code, "^[[:alpha:]].+")) |>
  dplyr::mutate(code = str_replace(code, "[[:punct:]]", ""))

Stress_Related_Presentation_opertn <-
  Admitted_Patient_Care |>
  dplyr::select(
    pseudo,
    pupil_matching_ref_anonymous,
    epi_key,
    opertn_01, opertn_02, opertn_03, opertn_04, opertn_05,
    opertn_06, opertn_07, opertn_08, opertn_09, opertn_10,
    opertn_11, opertn_12, opertn_13, opertn_14, opertn_15,
    opertn_16, opertn_17, opertn_18, opertn_19, opertn_20,
    opertn_21, opertn_22, opertn_23, opertn_24
  ) |>
  tidyr::pivot_longer(
    cols = starts_with("opertn_"),
    names_to = "opertn_name",
    values_to = "opertn",
    values_drop_na = TRUE
  ) |>
  dplyr::filter(str_detect(opertn, "^[[:alpha:]].+")) |>
  dplyr::mutate(opertn = str_replace(opertn, "[[:punct:]]", ""))

Stress_Related_Presentation <-
  Stress_Related_Presentation_identifiers |>
  left_join(Stress_Related_Presentation_diag) |>
  left_join(Stress_Related_Presentation_opertn) |>
  inner_join(SRP_Codelist) |>
  dplyr::select(-dataset, -field, -code_type, -flag1) |>
  dplyr::filter(
    diag_position == "any" |
      (diag_position == "first" & diag_name == "diag_01")
  ) |>
  dplyr::mutate(
    med_surg_flag = case_when(
      str_detect(code, "A0[0123456789]") ~ 1,
      str_detect(code, "C1[56789]") ~ 1,
      str_detect(code, "C2[0123456]") ~ 1,
      str_detect(code, "C3[01234789]") ~ 1,
      str_detect(code, "C4[0156789]") ~ 1,
      str_detect(code, "C5[12345678]") ~ 1,
      str_detect(code, "C6[456789]") ~ 1,
      str_detect(code, "C7[012345]") ~ 1,
      str_detect(code, "E282") ~ 1,
      str_detect(code, "G4[0156]") ~ 1,
      str_detect(code, "I6[0123456789]") ~ 1,
      str_detect(code, "J1[0345678]") ~ 1,
      str_detect(code, "K3[5678]") ~ 1,
      str_detect(code, "K52[019]") ~ 1,
      str_detect(code, "K5[56]") ~ 1,
      str_detect(code, "N131") ~ 1,
      str_detect(code, "N390") ~ 1,
      str_detect(code, "N8[1356789]") ~ 1,
      str_detect(code, "N90") ~ 1,
      str_detect(opertn, "H01[1239]") ~ 1,
      str_detect(opertn, "H02[13489]") ~ 1,
      str_detect(opertn, "Y752") ~ 1,
      TRUE ~ 0
    )
  ) |>
  group_by(epi_key) |>
  mutate(med_surg_flag_max = max(med_surg_flag)) |>
  ungroup() |>
  dplyr::filter(!(flag2 == "med_surg" & med_surg_flag_max == 1)) |>
  dplyr::select(-opertn_name, -opertn, -med_surg_flag, -med_surg_flag_max) |>
  dplyr::mutate(
    selfharmxz_flag = case_when(
      str_detect(code, "X6[0123456789]") ~ 1,
      str_detect(code, "X7[0123456789]") ~ 1,
      str_detect(code, "X8[1234]") ~ 1,
      str_detect(code, "Z040") ~ 1,
      str_detect(code, "Z50[23]") ~ 1,
      str_detect(code, "Z56[34]") ~ 1,
      str_detect(code, "Z642") ~ 1,
      str_detect(code, "Z71[145]") ~ 1,
      str_detect(code, "Z72[123]") ~ 1,
      str_detect(code, "Z915") ~ 1,
      TRUE ~ 0
    )
  ) |>
  group_by(epi_key) |>
  mutate(selfharmxz_flag_sum = sum(selfharmxz_flag)) |>
  mutate(
    selfharmxz_remove_flag = case_when(
      flag2 == "selfharm_xz_codes" & selfharmxz_flag_sum < 2 ~ 1,
      TRUE ~ 0
    )
  ) |>
  ungroup() |>
  dplyr::filter(selfharmxz_remove_flag != 1) |>
  dplyr::select(
    -selfharmxz_flag,
    -selfharmxz_flag_sum,
    -selfharmxz_remove_flag
  ) |>
  dplyr::select(
    pseudo,
    pupil_matching_ref_anonymous,
    epi_key,
    year,
    group
  ) |>
  dplyr::distinct() |>
  dplyr::mutate(value = 1) |>
  pivot_wider(names_from = group, values_from = value, values_fill = 0) |>
  dplyr::rename(self_harm = "selfharm") |>
  mutate(across(everything(), ~ replace_na(., 0))) |>
  group_by(pseudo, pupil_matching_ref_anonymous, year, epi_key) |>
  summarise(
    self_harm = max(self_harm),
    internalising = max(internalising),
    thought_disorder = max(thought_disorder),
    potentially_psych = max(potentially_psych),
    externalising = max(externalising)
  ) |>
  ungroup() |>
  left_join(Spine)

gc()

rm(
  Stress_Related_Presentation_diag,
  Stress_Related_Presentation_identifiers,
  Stress_Related_Presentation_opertn,
  SRP_Codelist
)

stress_related_presentation_labels <- c(
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
  potentially_psych = "Potentially Psychosomatic",
  internalising = "Internalising",
  externalising = "Externalising",
  thought_disorder = "Thought Disorders",
  self_harm = "Self-Harm"
)

label(Stress_Related_Presentation) <-
  as.list(stress_related_presentation_labels[match(
    names(Stress_Related_Presentation),
    names(stress_related_presentation_labels)
  )])
rm(stress_related_presentation_labels)

write_parquet_labeled(
  Stress_Related_Presentation,
  "data/Stress_Related_Presentation"
)
