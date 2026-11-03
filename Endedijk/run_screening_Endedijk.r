library(dplyr)
library(AIscreenR)
library(future)

#--------------------------------------------------------------------------
# Prompt
#--------------------------------------------------------------------------
prompt <- "Evaluate the following study based on the selection criteria
 for a systematic review and meta-analysis on the association between the quality of the
 teacher-student relationship and students' relationships with their peers in school.
 The teacher-student relationship is the relationship between a teacher and a student or a class. It covers
 positive aspects such as closeness, warmth, trust, care, support and feeling related to teachers, and negative
 aspects such as conflict, negativity, dependency, and bullying or verbal abuse by the teacher. A teacher's warm,
 caring, supportive or authoritative way of treating students, as the students perceive it, counts as well, also when
 the study calls it classroom climate. It can be reported by the
 student, by the teacher or by an observer. In many studies it is called teacher support, perceived support from
 teachers, teacher-child relationship or child-teacher interaction, and it often appears as one of several sources
 of social support (parents, teachers, classmates, friends). Peer relationships means how students get along with
 other children in school: peer acceptance and rejection (however it is measured, also with peer nominations),
 social preference, popularity, friendships, classmate relationships, support from classmates, feeling related to
 peers, peer victimization and bullying (as victim, bully or both). Cohesiveness or friendship between the students
 in the classroom, social integration in the class, sense of community among classmates and social status among
 classmates count as well. The studies are quantitative studies of children and adolescents from pre-school to the
 end of secondary school. You only have the title and the abstract and the full text is checked later, so be
 generous: it is much worse to exclude a study that belongs in the review than to include one that does not. Use the
 title as well as the abstract: if the title names teacher-student relations or peer relations as something that
 was studied, count it as measured.
 The teacher-student relationship and the peer relationships do not have to be analysed together, or related to
 each other, in the abstract. It is enough that both are measured in the same study, for example when support
 from teachers and support from classmates are both used to predict grades, well-being or behavior, or when
 bullying is studied together with teacher support. Each of them can be a predictor, an outcome, a mediator, a
 moderator or a control variable. Teaching methods, instructional quality, classroom management and teacher
 stress do not count by themselves, only if the study also looks at how teachers and students relate.
 For each study, I would like you to assess in this order:
 1) Are the participants children or adolescents in school (pre-school, primary or secondary school, roughly
 age 3-18)? Studies of university students, adults or only teachers do not count. If not, exclude study.
 2) Does the study measure or report something about the relationship between teachers and students? Teacher
 support, support from teachers as one of several support sources, closeness, conflict, warmth, relatedness to
 teachers and teacher behavior toward students such as verbal abuse all count. If not, exclude study.
 3) Does the study measure or report something about peer relationships? Classmate support, relationships with
 classmates, relatedness to peers, friendships, peer acceptance or rejection, victimization and bullying all count.
 If not, exclude study.
 4) Are both of them measured in the same study? A study that has both teacher support and classmate support, or
 bullying together with teacher support, counts even if the two are only used as separate predictors. If the abstract
 only has one of them, exclude study.
 5) Is it an original quantitative study? Surveys, questionnaires, observations, longitudinal studies and experiments
 count, and so do mixed methods studies with a quantitative part. Purely qualitative studies, literature reviews,
 meta-analyses, theory papers and book reviews are excluded. If not, exclude study.
 If all five questions are answered yes or probably yes, include the study. If you are unsure, include the study."


#--------------------------------------------------------------------------
# Data
#--------------------------------------------------------------------------
raw <- read.csv("Endedijk/data/04_Endedijk_data.csv", stringsAsFactors = FALSE)

endedijk_raw <- raw |>
  transmute(
    id        = as.character(key),
    title          = title,
    abstract       = abstract,
    author       = authors,
    year           = year,
    human_code     = as.numeric(label_included),
    included_final = as.integer(included_final)
  )

unique_citations <- CiteSource::dedup_citations(endedijk_raw, manual = TRUE)$unique

# If a record has included_final = 1, then human_code must be 1 (included). If not, set human_code to 1.
unique_citations <- unique_citations |> mutate(human_code = ifelse(included_final == 1, 1, human_code))

# All records without an abstract are removed before screening, including finally included studies
no_abstract <- is.na(unique_citations$abstract) | trimws(unique_citations$abstract) == "" | unique_citations$abstract == "No information"
removed_ids <- unique_citations$id[no_abstract]
included_final_ids <- unique_citations$id[no_abstract & unique_citations$included_final == 1] # finally included, removed too

cat("Records in data:                               ", nrow(endedijk_raw), "\n")
cat("Records without abstract (all removed):        ", sum(no_abstract), "\n")
cat("  of which finally included:                   ", length(included_final_ids), "\n")

screening_data <- unique_citations[!no_abstract, ]
cat("Records to screen:                             ", nrow(screening_data), "\n")

#--------------------------------------------------------------------------
# Small test set
#--------------------------------------------------------------------------
# 50 human in + 50 human out (none of them finally included) + all finally included studies
set.seed(123)
test_data <- bind_rows(
  screening_data |> filter(included_final == 1),
  screening_data |> filter(included_final == 0, human_code == 1) |> slice_sample(n = 50),
  screening_data |> filter(included_final == 0, human_code == 0) |> slice_sample(n = 50)
)
table(human_code = test_data$human_code, included_final = test_data$included_final)

plan(multisession)

res_test <- AIscreenR::tabscreen_gpt(
  data = test_data,
  prompt = prompt,
  studyid = id,
  title = title,
  abstract = abstract,
  model = "gpt-6-luna",
  reps = 1,
  reasoning_effort = "medium",
  verbosity = "low",
  decision_description = TRUE # explanations, to see why records were wrongly excluded/included
)

plan(sequential)

AIscreenR::screen_analyzer(res_test)
table(AI = res_test$answer_data$decision_binary, human = res_test$answer_data$human_code)
table(AI = res_test$answer_data$decision_binary, final = res_test$answer_data$included_final)

# Finally included studies the AI excluded, with its explanation
fn_final_included <- res_test$answer_data |>
  filter(decision_binary == 0, included_final == 1) |>
  select(id, title, abstract, detailed_description)

for (i in 1:nrow(fn_final_included)) {
  cat("ID:", fn_final_included$id[i], "\n")
  cat("Title:", fn_final_included$title[i], "\n")
  cat("Abstract:", fn_final_included$abstract[i], "\n")
  cat("AI explanation:", fn_final_included$detailed_description[i], "\n\n")
}

# Human excluded studies the AI included, with its explanation
fp_human_code <- res_test$answer_data |>
  filter(decision_binary == 1, human_code == 0) |>
  select(id, title,abstract, detailed_description)

for (i in 1:nrow(fp_human_code)) {
  cat("ID:", fp_human_code$id[i], "\n")
  cat("Title:", fp_human_code$title[i], "\n")
  cat("Abstract:", fp_human_code$abstract[i], "\n")
  cat("AI explanation:", fp_human_code$detailed_description[i], "\n\n")
}

#--------------------------------------------------------------------------
# Screening
#--------------------------------------------------------------------------
plan(multisession)

res_all <- AIscreenR::tabscreen_gpt(
  data = screening_data,
  prompt = prompt,
  studyid = id,
  title = title,
  abstract = abstract,
  model = "gpt-6-luna",
  reps = 1,
  reasoning_effort = "medium",
  verbosity = "low",
  decision_description = FALSE
)

plan(sequential)

AIscreenR::screen_analyzer(res_all)

table(res_all$answer_data$decision_binary, res_all$answer_data$human_code)

table(res_all$answer_data$decision_binary, res_all$answer_data$included_final)

saveRDS(res_all, "Endedijk/data/screening_Endedijk_ai.rds")

#--------------------------------------------------------------------------
# Prepare the data for the simulation
#--------------------------------------------------------------------------
endedijk_data <- res_all$answer_data |>
  mutate(
    id         = as.character(id),
    human_code      = as.numeric(human_code),
    decision_binary = as.numeric(decision_binary),
    included_final  = as.integer(included_final),
    human_and_ai_in = as.integer(decision_binary == 1 & human_code == 1) # included by both the AI and the humans
  )

cat("Prepared data:", nrow(endedijk_data), "records |",
    "AI included:", sum(endedijk_data$decision_binary == 1),
    "| human included:", sum(endedijk_data$human_code == 1),
    "| human and AI:", sum(endedijk_data$human_and_ai_in == 1),
    "| finally included:", sum(endedijk_data$included_final == 1), "\n")

attr(endedijk_data, "data_name") <- "endedijk_data"
saveRDS(endedijk_data, "Endedijk/data/endedijk_cleaned.rds")
