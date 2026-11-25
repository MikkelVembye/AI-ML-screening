library(dplyr)
library(AIscreenR)
library(future)

#--------------------------------------------------------------------------
# Prompt
#--------------------------------------------------------------------------
prompt <- "
Select studies for full-text review. The review is about group-based community
interventions for adults with mental illness who also have personal or social
problems.

Include a study if it meets these criteria, or if the abstract suggests that it
likely does:

1. Population: Adults (18 or older) with a psychiatric diagnosis AND personal or
   social problems, such as substance abuse, self-harm, criminal behaviour,
   homelessness, unemployment, hospital admissions, low well-being, social
   isolation or loneliness.
2. Intervention: Delivered in a group (more than one participant at the same
   time and place), in a community or out-patient setting. It must aim at more
   than symptom reduction, for example social reintegration, social skills,
   coping or well-being.
3. Design: A randomised trial, quasi-experimental study, or a comparison of two
   or more groups (for example treatment as usual, waiting list or another
   intervention).

Exclude a study if any of these is clearly true:
- The participants are under 18.
- The study is outside OECD countries.
- The intervention is delivered to one person at a time.
- The participants are in hospital with around-the-clock care.
- The intervention is medication only.
- There is only one group (pre-post, no comparison group).
- The intervention only aims to reduce symptoms in people without other personal
  or social problems.

If the abstract does not give enough information about a criterion, do not
exclude the study for that reason alone.
"


#--------------------------------------------------------------------------
# Data: about 200 studies = 5 edge cases + 98 human included + 98 human excluded
#--------------------------------------------------------------------------
# Edge cases (eppi_id): the AI must code these in the same way as the humans did
# 3 human included and finally included: the AI must find them (added_5 Somers, added_3 Schafer, 73519573 Crawford)
# 2 human excluded, but they could be included according to the protocol (73491295, 73519561)
edge_cases <- c("added_5", "added_3", "73519573", "73491295", "73519561")

raw <- readRDS("group_based/data/group_based_data.rds") |>
  transmute(
    id             = eppi_id,
    title          = title,
    abstract       = abstract,
    human_code     = as.numeric(human_code),
    included_final = as.integer(included_final)
  )

stopifnot("Edge cases not found exactly once" = sum(raw$id %in% edge_cases) == length(edge_cases))

set.seed(123)
data <- bind_rows(
  raw |> filter(id %in% edge_cases),
  raw |> filter(!id %in% edge_cases, human_code == 1) |> slice_sample(n = 98),
  raw |> filter(!id %in% edge_cases, human_code == 0) |> slice_sample(n = 98)
) |>
  as_tibble()

table(human_code = data$human_code)
table(included_final = data$included_final)


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

table(AI = res_test$answer_data$decision_binary, human = res_test$answer_data$human_code)


#--------------------------------------------------------------------------
# Check the disagreements between the AI and the humans
#--------------------------------------------------------------------------
# False negatives: included by humans but excluded by AI
false_negatives <- res_test$answer_data |>
  filter(human_code == 1 & decision_binary == 0) |>
  select(id, title, human_code, decision_binary, included_final, detailed_description)

false_negatives

# False positives: excluded by humans but included by AI
false_positives <- res_test$answer_data |>
  filter(human_code == 0 & decision_binary == 1) |>
  select(id, title, human_code, decision_binary, included_final, detailed_description)

false_positives


#--------------------------------------------------------------------------
# Check if edge cases were caught
#--------------------------------------------------------------------------
# The AI should code all edge cases in the same way as the humans
edge <- res_test$answer_data |> filter(id %in% edge_cases)

edge |> select(id, human_code, decision_binary, detailed_description)
stopifnot(all(edge$decision_binary == edge$human_code))
