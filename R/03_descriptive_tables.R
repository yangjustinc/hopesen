# Descriptive tables --------------------------------------------------------
#
# Generates descriptive summaries used for analytical checking and manuscript
# reporting. Disclosure control must be applied to any outputs leaving the SRS.

## Set table parameters ====
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

source_objects <- c(
  Spine = "Spine", Demographics = "Demographics", Education = "Education",
  Absences = "Absences", Exclusions = "Exclusions",
  Chronic_Health_Conditions = "Chronic_Health_Conditions",
  Adversity_Related_Injury = "Adversity_Related_Injury",
  Stress_Related_Presentation = "Stress_Related_Presentation",
  Suicide = "Suicide", Alternative_Provision = "Alternative_Provision"
)
for (object_name in names(source_objects)) {
  load_if_missing(object_name, file.path("data", source_objects[[object_name]]))
}

sect_a4_land_narrow <- prop_section(
  page_size = page_size(
    orient = "landscape",
    width = 11.69,
    height = 8.27
  ),
  page_margins = page_mar(
    top = 0.3,
    bottom = 0.3,
    left = 0.3,
    right = 0.3,
    header = 0.2,
    footer = 0.2,
    gutter = 0
  ),
  type = "continuous"
)

## Spine ====
Spine_Table <- table1(
  ~ sen_indicator |
    factor(year) * factor(nc_year_actual),
  data = Spine,
  overall = FALSE
) |>
  t1flex() |>
  fontsize(size = 4, part = "all") |>
  autofit() |>
  set_table_properties(width = 1, layout = "autofit")

save_as_docx(Spine_Table, path = "outputs/tables/spine.docx", pr_section = sect_a4_land_narrow)

gc()

## Demographics ====
Demographics_Table <- Demographics %>%
  dplyr::filter(gender == "Boys" |
                  gender == "Girls") %>%
  table1(
    ~ factor(gender) + factor(ethnic_group_major) + factor(fsm_eligible) + factor(language_group) |
      factor(sen_indicator),
    data = .,
    overall = FALSE
  ) |>
  t1flex() |>
  autofit() |>
  set_table_properties(width = 1, layout = "autofit")

save_as_docx(Demographics_Table, path = "outputs/tables/demographics.docx", pr_section = sect_a4_land_narrow)

gc()

## Education ====
Education_Table <- Education %>%
  dplyr::filter(sen_indicator != "No SEND") %>%
  dplyr::filter(
    sen_provision != "No special educational need" &
      sen_provision != "School Action (up to 2014/15)" &
      sen_provision != "School Action Plus (up to 2014/15)" &
      sen_provision != "Missing" &
      sen_unit != "Missing" &
      resourced_provision != "Missing" &
      phase != "Primary" &
      phase != "Middle (Deemed Primary)"
  ) %>%
  table1(
    ~ factor(sen_provision) + factor(sen_unit) + factor(resourced_provision) + factor(phase) |
      factor(sen_indicator),
    data = .,
    overall = FALSE
  ) |>
  t1flex() |>
  autofit() |>
  set_table_properties(width = 1, layout = "autofit")

save_as_docx(Education_Table, path = "outputs/tables/education.docx", pr_section = sect_a4_land_narrow)

gc()

## Absences ====
Absences_Table <- Absences %>%
  table1(
    ~ authorised_absence_autumn + unauthorised_absence_autumn + overall_absence_autumn + absence_rate_autumn + authorised_absence_spring + unauthorised_absence_spring + overall_absence_spring + absence_rate_spring + authorised_absence_summer + unauthorised_absence_summer + overall_absence_summer + absence_rate_summer + authorised_absence_6halfterms + unauthorised_absence_6halfterms + overall_absence_6halfterms + absence_rate_6halfterms |
      factor(sen_indicator),
    data = .,
    render.continuous = c("Median" = "MEDIAN",
                          "Interquartile Range" = "IQR"),
    overall = FALSE
  ) |>
  t1flex() |>
  autofit() |>
  set_table_properties(width = 1, layout = "autofit")

save_as_docx(Absences_Table, path = "outputs/tables/absences.docx", pr_section = sect_a4_land_narrow)

gc()

## Exclusions ====
Exclusions_Table <- Exclusions %>%
  group_by(pupil_matching_ref_anonymous,
           year,
           nc_year_actual,
           sen_indicator) %>%
  summarise(sessions_excluded = sum(sessions_excluded)) %>%
  table1(
    ~ sessions_excluded |
      factor(sen_indicator),
    data = .,
    render.continuous = c("Median" = "MEDIAN",
                          "Interquartile Range" = "IQR"),
    overall = FALSE
  ) |>
  t1flex() |>
  autofit() |>
  set_table_properties(width = 1, layout = "autofit")

save_as_docx(Exclusions_Table, path = "outputs/tables/exclusions.docx", pr_section = sect_a4_land_narrow)

gc()

## Chronic Health Conditions ====
Chronic_Health_Conditions_Table <- Chronic_Health_Conditions %>%
  full_join(Spine) %>%
  mutate(
    mental_health_behavioural = ifelse(
      is.na(mental_health_behavioural),
      0,
      mental_health_behavioural
    ),
    cancer_blood = ifelse(is.na(cancer_blood), 0, cancer_blood),
    respiratory = ifelse(is.na(respiratory), 0, respiratory),
    metabolic_endocrine_digestive_renal_genitourinary = ifelse(
      is.na(metabolic_endocrine_digestive_renal_genitourinary),
      0,
      metabolic_endocrine_digestive_renal_genitourinary
    ),
    musculoskeletal_skin = ifelse(is.na(musculoskeletal_skin), 0, musculoskeletal_skin),
    neurological = ifelse(is.na(neurological), 0, neurological),
    cardiovascular = ifelse(is.na(cardiovascular), 0, cardiovascular)
  ) %>%
  table1(
    ~ factor(mental_health_behavioural) + factor(cancer_blood) + factor(respiratory) + factor(metabolic_endocrine_digestive_renal_genitourinary) + factor(musculoskeletal_skin) + factor(neurological) + factor(cardiovascular) |
      factor(sen_indicator),
    data = .,
    overall = FALSE
  ) |>
  t1flex() |>
  autofit() |>
  set_table_properties(width = 1, layout = "autofit")

save_as_docx(Chronic_Health_Conditions_Table,
             path = "outputs/tables/chronic_health_conditions.docx",
             pr_section = sect_a4_land_narrow)

gc()

## Adversity Related Injuries ====
Adversity_Related_Injury_Table <- Adversity_Related_Injury %>%
  full_join(Spine) %>%
  mutate(
    self_harm = ifelse(is.na(self_harm), 0, self_harm),
    drug_alc = ifelse(is.na(drug_alc), 0, drug_alc),
    violence = ifelse(is.na(violence), 0, violence)
  ) %>%
  table1(
    ~ factor(self_harm) + factor(drug_alc) + factor(violence) |
      factor(sen_indicator),
    data = .,
    overall = FALSE
  ) |>
  t1flex() |>
  autofit() |>
  set_table_properties(width = 1, layout = "autofit")

save_as_docx(Adversity_Related_Injury_Table,
             path = "outputs/tables/adversity_related_injury.docx",
             pr_section = sect_a4_land_narrow)

gc()

## Stress Related Presentations ====
Stress_Related_Presentation_Table <- Stress_Related_Presentation %>%
  full_join(Spine) %>%
  mutate(
    self_harm = ifelse(is.na(self_harm), 0, self_harm),
    internalising = ifelse(is.na(internalising), 0, internalising),
    thought_disorder = ifelse(is.na(thought_disorder), 0, thought_disorder),
    potentially_psych = ifelse(is.na(potentially_psych), 0, potentially_psych),
    externalising = ifelse(is.na(externalising), 0, externalising)
  ) %>%
  table1(
    ~ factor(self_harm) + factor(internalising) + factor(thought_disorder) + factor(potentially_psych) + factor(externalising) |
      factor(sen_indicator),
    data = .,
    overall = FALSE
  ) |>
  t1flex() |>
  autofit() |>
  set_table_properties(width = 1, layout = "autofit")

save_as_docx(Stress_Related_Presentation_Table,
             path = "outputs/tables/stress_related_presentation.docx",
             pr_section = sect_a4_land_narrow)

gc()

## Suicide Deaths ====
Suicide_Table <- Suicide %>%
  table1(~ factor(suicide) |
           factor(sen_indicator),
         data = .,
         overall = FALSE) |>
  t1flex() |>
  autofit() |>
  set_table_properties(width = 1, layout = "autofit")

save_as_docx(Suicide_Table,
             path = "outputs/tables/suicide.docx",
             pr_section = sect_a4_land_narrow)

gc()

## Alternative Provision ====
AlternativeProvision_Table <- Alternative_Provision %>%
  table1(~ factor(alternative_provision) |
           factor(sen_indicator),
         data = .,
         overall = FALSE) |>
  t1flex() |>
  autofit() |>
  set_table_properties(width = 1, layout = "autofit")

save_as_docx(AlternativeProvision_Table,
             path = "outputs/tables/alternative_provision.docx",
             pr_section = sect_a4_land_narrow)

gc()
