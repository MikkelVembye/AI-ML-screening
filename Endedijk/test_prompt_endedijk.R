library(dplyr)
library(AIscreenR)
library(future)
prompt <- "Evaluate the following study based on the selection criteria
 for a systematic review and meta-analysis on the association between the affective quality of the
 teacher-student relationship and students' relationships with their classmates. The review only uses studies in
 which individual students have a score on both. You only have the title and the abstract, and the full text is
 checked later. Go through the exclusion rules first. If ANY one of them applies, exclude the study. If none
 applies, go through the inclusion rules and include the study if all three are met or likely. Say which rule decided.

 EXCLUDE the study if any one of these applies:
 X1) It is not an empirical study that reports data on students: purely qualitative studies, literature reviews,
 meta-analyses, theory papers, case studies, book reviews.
 X2) The sample is not from preschool, primary, middle or secondary education (roughly age 3-18): university
 students, adults, or only teachers.
 X3) Only one of the two relationships is studied: only the teacher-student relationship (for example with
 achievement, motivation or engagement), or only peers (for example bullying or friendships without a teacher
 variable).
 X4) The teacher side is not about the relationship: teaching methods, instructional quality, classroom
 management, teacher expectations, feedback, teacher stress, teacher training.
 X5) The peers are not classmates: peers or friends outside school, in free time or in sports clubs, peer
 pressure, peer influence, deviant peers, peer groups, peer-assisted learning or tutoring.
 X6) The abstract clearly says there are only class-level or school-level scores, or it studies general
 classroom climate, school climate, school belonging, connectedness, satisfaction, engagement or well-being
 without separate measures of the teacher and of the classmates. Do NOT exclude because the abstract also
 mentions classroom climate, if teacher and classmate measures are present for the students.
 X7) It evaluates an intervention, program or training, or develops a scale, and does not measure both
 relationships as variables. Do NOT exclude an intervention study if the abstract says that both relationships
 were measured.
 X8) Teacher support and general peer or friend support are only two of several sources of social support
 (parents, teachers, peers, friends) used to predict an unrelated outcome such as well-being, health,
 depression, suicide, achievement, engagement or aggression, AND the abstract does not mention classmates, does
 not name a student-teacher relationship (closeness, conflict, warmth, relatedness, attachment), and does not
 mention peer acceptance, rejection, popularity or friendship. Do NOT use X8 if the abstract mentions
 classmates, or if either relationship is described in more detail than as one kind of social support. If you
 are unsure whether X8 applies, do not use it.

 INCLUDE the study if all three are shown or likely from the title or abstract:
 I1) The affective side of the teacher-student relationship was measured: closeness, warmth, trust, care,
 affection, relatedness or attachment to the teacher; conflict, negativity, dependency, or hostile or abusive
 behavior by the teacher; teacher support or emotional support from teachers; the emotional quality of observed
 teacher-child interactions (sensitivity, warmth, conflict); or a named student-teacher or teacher-child
 relationship.
 I2) The relationships with classmates were measured: peer acceptance or rejection, being liked or disliked,
 social preference, popularity or social status, sociometric or peer nominations, friendships, relationships
 with classmates, or support from classmates. The peers of school students are their classmates unless the
 abstract says otherwise, so these count even when the word classmates is not used. General peer
 relationships, peer support or friends count as likely classmate measures. Peer victimization or bullying
 together with teacher support also counts.
 I3) Both were measured for the same students. They do not have to be analysed together. Either can be a
 predictor, an outcome, a mediator, a moderator or a control variable, and it does not matter when they were
 measured. A measure of child behavior is not required.
 If the abstract shows I1 and I2, or if they are likely to be measured in the full text, include the study.
 If the abstract does not mention one of the two relationships at all, exclude the study: do not assume it was
 measured. A general mention such as relationships with teachers and peers counts as likely measured."


#--------------------------------------------------------------------------
# Data: 200 studies = 4 edge cases (2 human included, 2 human excluded) + 98 human included + 98 human excluded
#--------------------------------------------------------------------------
edge_cases <- c("3882", "3647", "4030", "4334")

raw <- read.csv("Endedijk/data/04_Endedijk_data.csv", stringsAsFactors = FALSE) |>
  transmute(
    id         = as.character(key),
    title      = title,
    abstract   = abstract,
    human_code = as.numeric(label_included)
  )

set.seed(123)
data <- bind_rows(
  raw |> filter(id %in% edge_cases),
  raw |> filter(!id %in% edge_cases, human_code == 1) |> slice_sample(n = 98),
  raw |> filter(!id %in% edge_cases, human_code == 0) |> slice_sample(n = 98)
)

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
  model = "gpt-6.1-terra",
  reps = 1,
  reasoning_effort = "high",
  verbosity = "low",
  overinclusive = FALSE,
  decision_description = TRUE
)

plan(sequential)


#--------------------------------------------------------------------------
# Check if edge cases were caught
#--------------------------------------------------------------------------
edge <- res_test$answer_data |> filter(id %in% edge_cases)

edge |> select(id, human_code, decision_binary, detailed_description)
stopifnot("The AI missed an edge case" = all(edge$decision_binary != edge$human_code))
