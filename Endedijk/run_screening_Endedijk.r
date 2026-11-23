library(dplyr)
library(AIscreenR)
library(future)

#--------------------------------------------------------------------------
# Prompt
#--------------------------------------------------------------------------
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
# Data
#--------------------------------------------------------------------------
raw <- read.csv("Endedijk/data/04_Endedijk_data.csv", stringsAsFactors = FALSE)

endedijk_raw <- raw |>
  transmute(
    id             = as.character(key),
    title          = title,
    abstract       = abstract,
    author         = authors,
    year           = year,
    human_code     = as.numeric(label_included),
    included_final = as.integer(included_final)
  )

endedijk_unique <- CiteSource::dedup_citations(endedijk_raw, manual = TRUE)$unique |>
  # CiteSource returns the labels as text ("0"/"1"), so they are converted back to numbers
  mutate(human_code = as.numeric(human_code), included_final = as.integer(included_final))

cat("Finally included before / after CiteSource:    ", sum(endedijk_raw$included_final), "/", sum(endedijk_unique$included_final), "\n")

# If a record has included_final = 1, then human_code must be 1 (included). If not, set human_code to 1.
endedijk_unique <- endedijk_unique |> mutate(human_code = ifelse(included_final == 1, 1, human_code))

# Duplicates that CiteSource missed: the same study with a different title format (case, punctuation, HTML
# escapes such as &apos;). One copy is kept per study, with the highest labels of its copies. All copies, with
# their original labels and a kept flag, are saved so the removal can be checked.
duplicate_check <- endedijk_unique |>
  mutate(
    title_key = gsub("[^a-z0-9]", "", gsub("&apos;|&amp;|&quot;|&#39;", "", tolower(title))),
    title_key = ifelse(nzchar(title_key) & !is.na(title_key), title_key, paste0("no_title_", id)),
    abstract_chars = nchar(coalesce(abstract, ""))
  ) |>
  group_by(title_key) |>
  mutate(
    n_copies = n(),
    human_code_original = human_code,
    included_final_original = included_final,
    human_code = max(human_code),
    included_final = max(included_final)
  ) |>
  arrange(desc(abstract_chars), as.numeric(id), .by_group = TRUE) |>
  mutate(kept = row_number() == 1) |>
  ungroup()

duplicate_copies <- duplicate_check |>
  filter(n_copies > 1) |>
  arrange(title_key, desc(kept)) |>
  select(title_key, kept, n_copies, id, title, year, abstract_chars,
         human_code_original, included_final_original, human_code, included_final)
saveRDS(duplicate_copies, "Endedijk/data/duplicates_Endedijk.rds")

cat("Duplicate studies (same title):                ", n_distinct(duplicate_copies$title_key), "\n")
cat("  extra copies removed:                        ", sum(!duplicate_copies$kept), "\n")
cat("  removed copies that were finally included:   ", sum(!duplicate_copies$kept & duplicate_copies$included_final_original == 1), "\n")
cat("  studies whose copies had different labels:   ",
    sum(tapply(duplicate_copies$human_code_original, duplicate_copies$title_key, function(x) length(unique(x))) > 1 |
        tapply(duplicate_copies$included_final_original, duplicate_copies$title_key, function(x) length(unique(x))) > 1), "\n")

unique_citations <- duplicate_check |>
  filter(kept) |>
  select(-title_key, -abstract_chars, -n_copies, -human_code_original, -included_final_original, -kept)


# All records without an abstract are removed before screening, including finally included studies
no_abstract <- is.na(unique_citations$abstract) | trimws(unique_citations$abstract) == "" | unique_citations$abstract == "No information"
removed_ids <- unique_citations$id[no_abstract]
included_final_ids <- unique_citations$id[no_abstract & unique_citations$included_final == 1] # finally included, removed too

cat("Records in data:                               ", nrow(endedijk_raw), "\n")
cat("Records without abstract (all removed):        ", sum(no_abstract), "\n")
cat("  of which human included:                     ", sum(no_abstract & unique_citations$human_code == 1), "\n")
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
  model = "gpt-6.1-terra",
  reps = 1,
  reasoning_effort = "high",
  verbosity = "low",
  overinclusive = FALSE, # default TRUE lets the AI answer "1.1" (unsure), which counts as include
  decision_description = TRUE # explanations, to see why records were wrongly excluded/included
)

plan(sequential)

AIscreenR::screen_analyzer(res_test)

#   promptid model        reps p_agreement recall specificity n_screened n_missing
#      <int> <chr>       <dbl>       <dbl>  <dbl>       <dbl>      <int>     <int>
# 1        1 gpt-6.1-te…     1       0.915  0.911        0.94        386         0

table(AI = res_test$answer_data$decision_binary, human = res_test$answer_data$human_code)
table(AI = res_test$answer_data$decision_binary, final = res_test$answer_data$included_final)

# Finally included studies the AI excluded, with its explanation
fn_final_included <- res_test$answer_data |>
  filter(decision_binary == 0, included_final == 1) |>
  select(id, title, abstract, detailed_description)

for (i in seq_len(nrow(fn_final_included))) {
  cat("ID:", fn_final_included$id[i], "\n")
  cat("Title:", fn_final_included$title[i], "\n")
  # Print only the first 200 characters of the abstract, to avoid cluttering the console with long abstracts
  #cat("Abstract:", substr(fn_final_included$abstract[i], 1, 200), ifelse(nchar(fn_final_included$abstract[i]) > 200, "...", ""), "\n")
  cat("Abstract:", fn_final_included$abstract[i], "\n")
  cat("AI explanation:", fn_final_included$detailed_description[i], "\n\n")
}

# Human excluded studies the AI included, with its explanation
fp_human_code <- res_test$answer_data |>
  filter(decision_binary == 1, human_code == 0) |>
  select(id, title, abstract, detailed_description)

for (i in seq_len(nrow(fp_human_code))) {
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
  model = "gpt-6.1-terra",
  reps = 1,
  reasoning_effort = "high",
  verbosity = "low",
  overinclusive = FALSE,
  decision_description = FALSE
)

plan(sequential)

AIscreenR::screen_analyzer(res_all)

table(AI = res_all$answer_data$decision_binary, human = res_all$answer_data$human_code)

table(AI = res_all$answer_data$decision_binary, final = res_all$answer_data$included_final)

# False negatives: included by humans but excluded by AI
false_negatives <- res_all$answer_data |>
  filter(human_code == 1 & decision_binary == 0) |>
  select(id, title, abstract, human_code, decision_binary, included_final)

cat("False negatives (human in, AI out):", nrow(false_negatives),
    "| of which finally included:", sum(false_negatives$included_final == 1), "\n")

false_positives <- res_all$answer_data |>
  filter(human_code == 0 & decision_binary == 1) |>
  select(id, title, abstract, human_code, decision_binary, included_final)

for (i in seq_len(nrow(false_positives))) {
  cat("ID:", false_positives$id[i], "\n")
  cat("Title:", false_positives$title[i], "\n")
  #cat("Abstract:", false_positives$abstract[i], "\n")
  # Print only the first 300 characters of the abstract, to avoid cluttering the console with long abstracts
  cat("Abstract:", substr(false_positives$abstract[i], 1, 300), ifelse(nchar(false_positives$abstract[i]) > 300, "...", ""), "\n")
  cat("Human code:", false_positives$human_code[i], "| AI code:", false_positives$decision_binary[i], "| Finally included:", false_positives$included_final[i], "\n\n")
}

saveRDS(res_all, "Endedijk/data/screening_Endedijk_ai.rds")

#--------------------------------------------------------------------------
# Prepare the data for the simulation
#--------------------------------------------------------------------------
endedijk_data <- res_all$answer_data |>
  mutate(
    id              = as.character(id),
    human_code      = as.numeric(human_code),
    decision_binary = as.numeric(decision_binary),
    included_final  = as.integer(included_final),
    human_and_ai_in = as.integer(decision_binary == 1 & human_code == 1) # included by both the AI and the humans
  )

stopifnot(!anyDuplicated(endedijk_data$id), !anyNA(endedijk_data$decision_binary))

cat("Prepared data:", nrow(endedijk_data), "records |",
    "AI included:", sum(endedijk_data$decision_binary == 1),
    "| human included:", sum(endedijk_data$human_code == 1),
    "| human and AI:", sum(endedijk_data$human_and_ai_in == 1),
    "| finally included:", sum(endedijk_data$included_final == 1), "\n")

attr(endedijk_data, "data_name") <- "endedijk_data"
saveRDS(endedijk_data, "Endedijk/data/endedijk_cleaned.rds")
