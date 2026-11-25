library(dplyr)
library(AIscreenR)
library(future)
prompt <- "Assess whether this record provides evidence on the health effects of immigration
detention or an asylum policy that exposes protection seekers to detention or
prolonged insecure status. Use only the title and abstract. Identify the exposure
contrast, outcome and type of evidence before deciding.

Include primary comparative studies when asylum seekers or refugees are compared on
measured mental or physical health outcomes across different immigration detention
histories, durations of immigration detention, or protection regimes. For a policy
contrast, the study must actually compare groups facing different asylum procedures,
legal security or temporary versus permanent protection and report psychiatric
symptoms, psychological distress or health-related functioning. Comparisons of people
applying after arrival with already authorized refugees or other immigrants qualify if
postmigration difficulties attributable to insecure status are studied alongside
psychiatric symptoms. Mixed methods qualify when a comparative health analysis is
reported.

Also include formal expert evidence submissions that specifically assess the combined
mental health effects of mandatory immigration detention and prolonged temporary legal
status. Exclude general advocacy, letters, news, accounts of individual cases,
literature commentary and summaries of another study. A descriptive survey of one
group of detainees, comparison with population norms, or follow-up of a single exposed
cohort without a policy or detention comparison group is insufficient. Before/after
symptom changes alone do not establish an eligible comparison. Distinguish immigration
detention from open reception centres, residential moves, general refugee camp stays,
and premigration imprisonment. Generic trauma, service use or acculturation studies
need a relevant exposure contrast. Explain the decision briefly.
For detention duration, require a comparison between defined exposure groups or with
non-detained protection seekers. Exclude a correlation between symptoms and time served
in a single detained sample. Time awaiting an asylum decision or living in reception
facilities does not establish detention. For status comparisons, require independently
recruited groups or groups defined by policy at baseline. A within-person change in visa
status is insufficient when the comparison concerns symptom change as status resolves.
Require the report to present its own methods and findings. A news summary saying that a
study found an effect is secondary reporting even when it gives sample sizes and scales.
A prevalence comparison of asylum applicants versus settled refugees only qualifies when
the analysis specifically investigates detention, restrictive protection policy or
insecure residency as an exposure. A difference in legal labels or place of recruitment
alone is insufficient.
First establish that the sample actually experienced immigration detention or that the
study directly compared asylum protection regimes. An asylum centre, repeated relocation,
or long residence in a reception system is not evidence of detention. For mixed
immigration detainee samples, a health service audit comparing lengths of custody across
all detainees is insufficient unless a detention effect is reported specifically for
protection seekers."







#--------------------------------------------------------------------------
# Data: 4 edge cases, 40 additional human inclusions and 98 additional human exclusions
#--------------------------------------------------------------------------
edge_cases <- c(include = "10231285", include = "10234026", exclude = "10232556", exclude = "10233566")

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
  raw |> filter(!id %in% edge_cases, human_code == 1) |> slice_sample(n = 40),
  raw |> filter(!id %in% edge_cases, human_code == 0) |> slice_sample(n = 98)
) |>
  as_tibble()

stopifnot(nrow(data) == 142, sum(data$id %in% edge_cases) == length(edge_cases))
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

res_test |> screen_analyzer(human_decision = included_final)


#--------------------------------------------------------------------------
# Check if edge cases were caught
#--------------------------------------------------------------------------
# The human code of the edge cases is probably wrong, so the AI should give the opposite answer
edge <- res_test$answer_data |> filter(id %in% edge_cases)

edge |> select(id, human_code, decision_binary, detailed_description)
stopifnot("The AI missed an edge case" = all(edge$decision_binary != edge$human_code))
