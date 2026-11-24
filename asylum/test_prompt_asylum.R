library(dplyr)
library(AIscreenR)
library(future)
prompt <- "Evaluate the following study based on the selection criteria
 for a systematic review of detention of asylum seekers and the impact on their mental health. The review
 looks for studies on asylum seekers and refugees in which detention, or the asylum-process conditions that
 go together with detention, are studied in relation to health. Detention means the deprivation of liberty of
 asylum seekers in an immigration or asylum context, for example in detention centers, to check identity, to
 process the asylum claim or to carry out a deportation. Health outcomes mean mental health (for example PTSD,
 depression, anxiety, psychological distress), physical health or social functioning. You only have the title
 and the abstract, and the full text is checked later, so if you are unsure, include the study. Studies are
 often not about detention alone: detention is often one of several post-migration stressors that are measured
 or compared. Go through the exclusion rules first. If ANY one of them applies, exclude the study. If none
 applies, go through the inclusion rules and include the study if both are met. Say which rule decided.

 EXCLUDE the study if any one of these applies:
 X1) The people are clearly not asylum seekers, refugees or immigration detainees: for example prisoners in
 criminal detention, general immigrants or migrant workers with no asylum or refugee background, or the
 general population. Studies of immigrants in general, in which refugees or asylum seekers are only
 mentioned as one of several groups, are also excluded. Asylum seekers whose claim was rejected, of all ages
 and nationalities, and refugees (the two words are often used for the same people) all count.
 X2) The abstract clearly shows that the study has nothing to do with detention or with the asylum process and
 post-migration conditions of asylum seekers (for example temporary protection or insecure residency status,
 delayed decisions, restricted access to work, housing or health care), or that it is only about
 pre-migration trauma, torture or treatment programs, with no mention of detention or post-migration
 conditions. Also exclude studies that are only about the general living conditions in a refugee camp or
 settlement (for example housing, crowding, food, nutrition, infections, or psychological work with the camp
 group), unless the abstract says that the people are held there against their will. Also exclude studies of
 the deportation or policing of immigrants in general, or of the forced return of refugees, that do not
 concern the detention of asylum seekers.
 X3) The abstract clearly shows that no health, mental health or social functioning is addressed: the study
 is only about law, politics, economics or integration, with no health information.
 X4) It is a news item, letter to the editor, book review or opinion piece, even when it describes harm to
 refugees or asylum seekers, unless it is about the detention of asylum seekers. Do NOT exclude reports,
 briefs, expert submissions or reviews that present or summarize evidence on the health effects of detention
 or of asylum policies.
 X5) The abstract positively states that there is no comparison of any kind and the study only describes one
 case. Do NOT exclude because the abstract does not mention a comparison group, and do NOT exclude
 descriptive or cross-sectional studies of detained asylum seekers.

 INCLUDE the study if both are shown or likely from the title or abstract:
 I1) The main group studied is asylum seekers, refugees or immigration detainees seeking protection.
 I2) Detention, or the asylum-process conditions that go with it, are part of the study and are connected to
 health, mental health or wellbeing. This includes: detained compared with not detained; groups that differ in
 whether, for how long or under what conditions they were detained; groups that differ in asylum policy or
 status (for example temporary versus permanent protection visas, or asylum seekers compared with refugees
 and immigrants) when the abstract mentions detention or post-migration stressors as a measured factor or as
 an explanation; and descriptions of the mental or physical health of detained asylum seekers. Detention does
 not have to be the main exposure. It is enough that detention, or the asylum-process conditions that go with
 it, are measured, mentioned as an explanation, or likely to be measured in the full text. Living conditions in
 a refugee camp do not count on their own.
 If the abstract shows that I1 is met and I2 is met or likely, include the study."


#--------------------------------------------------------------------------
# Data: 200 studies = 4 edge cases (2 human included, 2 human excluded) + 98 human included + 98 human excluded
#--------------------------------------------------------------------------
edge_cases <- c("10231285", "10234026", "10232556", "10233566")

raw <- readRDS("asylum/data/all_studies_with_labels.rds") |>
  transmute(
    # 5 records have no eppi_id (empty), so they get their own id
    id         = ifelse(is.na(eppi_id) | eppi_id == "", paste0("no_eppi_id_", row_number()), as.character(eppi_id)),
    title      = title,
    abstract   = abstract,
    human_code = as.numeric(human_code),
    included_final = as.integer(grepl("benchmark", cite_label))
  ) |>
  # records without an abstract cannot be screened (they are also removed in run_screening_asylum.r)
  filter(!is.na(abstract), trimws(abstract) != "")

set.seed(123)
data <- bind_rows(
  raw |> filter(id %in% edge_cases),
  raw |> filter(!id %in% edge_cases, human_code == 1) |> slice_sample(n = 98),
  raw |> filter(!id %in% edge_cases, human_code == 0) |> slice_sample(n = 98)
) |>
  as_tibble()

stopifnot(nrow(data) == 200, sum(data$id %in% edge_cases) == length(edge_cases))
table(human_code = data$human_code)

# Print the edge cases for reference
for (i in seq_along(edge_cases)) {
  cat("Edge case", i, "ID:", edge_cases[i], "\n")
  cat("Title:", data$title[data$id == edge_cases[i]], "\n")
  cat("Abstract:", data$abstract[data$id == edge_cases[i]], "\n\n")
  cat("Human code:", data$human_code[data$id == edge_cases[i]], "\n")
  cat("Included final:", data$included_final[data$id == edge_cases[i]], "\n\n")
}

#--------------------------------------------------------------------------
# Screening
#--------------------------------------------------------------------------
plan(multisession)

res_test <- AIscreenR::tabscreen_gpt(
  data = data,
  prompt = prompt,
  studyid = id,
  title = title,
  abstract = abstract,
  model = "gpt-6-luna",
  reps = 1,
  reasoning_effort = "high",
  verbosity = "low",
  overinclusive = FALSE,
  decision_description = TRUE
)

plan(sequential)

res_test |> screen_analyzer()


#--------------------------------------------------------------------------
# Check if edge cases were caught
#--------------------------------------------------------------------------
# The human code of the edge cases is probably wrong, so the AI should give the opposite answer
edge <- res_test$answer_data |> filter(id %in% edge_cases)

edge |> select(id, human_code, decision_binary, detailed_description)
stopifnot("The AI missed an edge case" = all(edge$decision_binary != edge$human_code))
