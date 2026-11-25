library(dplyr)
library(AIscreenR)
library(future)

prompt <- "Screen original quantitative pupil-level research from preschool through secondary
school for a review of the association between affective teacher-pupil relationships
and the pupil's own relationships with classroom peers. Include when a within-sample
association can plausibly be obtained. Both domains may be secondary, covariates,
moderators, or measured by different informants. Reporting a correlation in the
abstract is unnecessary. Inspect the entire title and abstract, including subordinate
findings.

Identify a separate personal relational measure on each side. Teacher affection,
support, perceived equity, positive or negative engagement, closeness, conflict,
liking, bullying by the teacher, and relatedness qualify. Peer acceptance, rejection,
peer ratings of liking, class belonging, community, cohesion, cooperation, social
efficacy, peer engagement, and being bullied by classmates qualify. Include both
domains as perpetrator-specific sources of relational victimization. Source-specific
psychosocial support for pupils, individual observation of interactions with teachers
and peers, social-motivational support and need satisfaction in school subjects, and
treatment studies with both parent-rated peer functioning and teacher-rated
student-teacher relationships qualify. Screen in studies that examine a teacher
relationship together with a separate peer context in standardized school-health
surveys even when only the strongest predictors are listed. A student-rated
teacher-relations climate factor predicting victimization, or perceived teacher
emotional support and sense of class community, qualify.

Apply these distinctions carefully. An institutional school-climate assessment of how
students generally relate, leadership or peer-network management, classroom justice,
and generic school support are insufficient unless personal relationships are
separately measured. Support specifically for learning, occupational planning or
subject motivation alone is insufficient without affective, social, relatedness or
relational-quality measures. Exclude a teacher's awareness of the classroom peer
network as the core teacher exposure with only generic auxiliary support. Exclude
qualitative analyses of talk. Distinguish personal school-peer relationships from close
friends of unknown setting, social capital among friends, friendship satisfaction
outside school, peer pressure and aggregated social-risk indices. Differentiate a
comparison of average support levels or endorsement frequencies across sources with no
analysis of individual associations from a study of individual source-specific social
support. Distinguish national means, group changes or respondent percentages alone from
evidence on between-child relationships. Separate intervention and control-group shifts
in happiness or skills cannot establish an individual association; a fictional target
is not an actual classmate. Classify using measures, units of analysis and setting;
give a brief reason."














#--------------------------------------------------------------------------
# Data: 200 studies = 4 edge cases (2 human included, 2 human excluded) + 98 human included + 98 human excluded
#--------------------------------------------------------------------------
edge_cases <- c(include = "3882", include = "3647", exclude = "4030", exclude = "4334")

raw <- read.csv("Endedijk/data/04_Endedijk_data.csv", stringsAsFactors = FALSE) |>
  mutate(
    id         = as.character(key),
    title      = title,
    abstract   = abstract,
    human_code = as.numeric(label_included)
  ) |> 
  select(id, title, abstract, human_code, included_final)

set.seed(123)
data <- bind_rows(
  raw |> filter(id %in% edge_cases),
  raw |> filter(!id %in% edge_cases, human_code == 1) |> slice_sample(n = 50),
  raw |> filter(!id %in% edge_cases, human_code == 0) |> slice_sample(n = 98)
) |> 
  as_tibble()

table(human_code = data$human_code)

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
edge <- res_test$answer_data |> filter(id %in% edge_cases)

edge |> select(id, human_code, decision_binary, detailed_description)
stopifnot("The AI missed an edge case" = all(edge$decision_binary != edge$human_code))
