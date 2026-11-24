library(dplyr)
library(AIscreenR)
library(future)

prompt <- "Select quantitative empirical studies of preschool to secondary pupils for
full-text review of how affective teacher-child relationships relate to children's
relationships with classroom peers. Read the complete abstract and title. They need not
report a correlation: both constructs can be secondary variables, reported by different
informants or measured at different times.

Apply these exclusions first. Exclude adult retrospective surveys. Exclude peer-network
management or teacher attunement/awareness studies even if general teacher support is an
ancillary predictor, unless a specific dyadic teacher-pupil relationship is also
assessed. Exclude social-risk surveys pairing generic teacher support with a peer risk
index (e.g. time with friends and rejection), without comparable relational support
scales. Exclude support for learning, academic help, achievement expectations, or
instrumental help as the only support on either side. Exclude scores describing general
school climate, academic environment, school social capital or family/community
environment if there is no separately interpretable measure of the child's own
relationships on both sides. Exclude when survey responses characterize a general
institutional climate (e.g. how teachers and peers treat students at this school), even
when several school climate dimensions are named. Relationships mentioned only as
components of such an environment are insufficient. A teacher acting solely as informant
about child behavior does not count.

Then look for two domains. The teacher domain includes a named relationship, liking as
perceived by the child, teacher, peers or observers, fairness, emotional support,
closeness, conflict, abuse and bullying by a teacher. The peer domain includes
classmates' liking, rejection, popularity, friendship, peer support, cooperation,
belonging to the class, peer bullying or victimization. Peer victimization IS a
relationship outcome when paired with an affective teacher measure. A child's personal
rating of both teacher and peer support is sufficient even if other sources or outcomes
are assessed. A generic peer or classmate support scale in school pupils can qualify. In
contrast, a measure of friends, close friends, friendship networks or social capital
among friends does not establish the classroom context: require school/class anchoring
for friends specifically. Exclude if the instrument explicitly mixes classmates with
partners or out-of-school friends. Cybervictimization alone cannot establish classroom
peer relations. Social efficacy or relatedness with both partners qualifies. A teacher's
positive or negative feedback about a target child in a social referencing experiment
counts as manipulated teacher liking, including a hypothetical target if peers judge its
acceptability.

In early childhood, separately observed engagement or interaction with teachers and peers
qualifies even when reported as behavior, activities or preschool classroom processes.
Exclude adjustment typologies that rate behavioral problems across situations with adults
or peers without assessing relational quality. Consider observational studies of
classroom relational quality and children's peer interactions for full text when
child-specific interactions are plausible. Separate teacher-relationship and
peer-relationship subscales of children's happiness or adjustment qualify even when the
abstract primarily describes an intervention. Child-reported source-specific support in
school psychosocial surveys qualifies even when the results summarize the sources
unevenly. Exclude when only one domain is present and there is no evidence for the other.
State which measures justified the decision."





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

res_test |> screen_analyzer()


#--------------------------------------------------------------------------
# Check if edge cases were caught
#--------------------------------------------------------------------------
edge <- res_test$answer_data |> filter(id %in% edge_cases)

edge |> select(id, human_code, decision_binary, detailed_description)
stopifnot("The AI missed an edge case" = all(edge$decision_binary != edge$human_code))
