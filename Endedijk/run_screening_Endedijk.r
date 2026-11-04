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
 applies, go through the inclusion rules and include the study only if ALL three are met. Say which rule decided.

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
 without separate measures of the teacher and of the classmates.
 X7) Teacher support and general peer or friend support are the only two measures, listed among several sources
 of social support (parents, teachers, peers, friends) that predict an unrelated outcome such as well-being,
 health, depression, suicide, achievement, engagement or aggression.
 X8) It evaluates an intervention, program or training, or develops a scale, and does not measure both
 relationships as variables.

 INCLUDE the study only if all three are shown in the title or abstract:
 I1) The affective side of the teacher-student relationship was measured: closeness, warmth, trust, care,
 affection, relatedness or attachment to the teacher; conflict, negativity, dependency, or hostile or abusive
 behavior by the teacher; teacher support or emotional support from teachers; the emotional quality of observed
 teacher-child interactions (sensitivity, warmth, conflict); or a named student-teacher or teacher-child
 relationship.
 I2) The relationships with classmates were measured: peer acceptance or rejection, being liked or disliked,
 social preference, popularity or social status, sociometric or peer nominations, friendships, relationships
 with classmates, or support from classmates. The peers of school students are their classmates unless the
 abstract says otherwise, so these count even when the word classmates is not used. Peer victimization or
 bullying counts only when the teacher side is closeness, conflict, warmth, relatedness or a named
 student-teacher or teacher-child relationship, not teacher support alone. Only general peer support, peer
 relations, peer interactions or friends, without one of the measures above, does not count.
 I3) Both were measured for the same students. They do not have to be analysed together. Either can be a
 predictor, an outcome, a mediator, a moderator or a control variable, and it does not matter when they were
 measured. A measure of child behavior is not required.
 If the abstract does not clearly show I1 and I2, or if you are unsure about them, exclude the study."


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

# Duplicates that CiteSource missed: the same study with a different title format (case, punctuation, HTML
# escapes such as &apos;). One copy is kept per study, with the highest labels of its copies. All copies, with
# their original labels and a kept flag, are saved so the removal can be checked.
duplicate_check <- unique_citations |>
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
#saveRDS(duplicate_copies, "Endedijk/data/duplicates_Endedijk.rds")

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
cat("  of which finally included:                   ", length(included_final_ids), "\n")

screening_data <- unique_citations[!no_abstract, ]
cat("Records to screen:                             ", nrow(screening_data), "\n")

#--------------------------------------------------------------------------
# Small test set
#--------------------------------------------------------------------------
# 50 human in + 50 human out (none of them finally included) + all finally included studies
# + 100 human out studies that an earlier AI run wrongly included, to see if the prompt now excludes them
set.seed(123)
earlier_fp_ids <- if (file.exists("Endedijk/data/screening_Endedijk_ai.rds")) {
  earlier <- readRDS("Endedijk/data/screening_Endedijk_ai.rds")$answer_data
  as.character(earlier$id[earlier$decision_binary == 1 & earlier$human_code == 0])
} else character(0)

test_data <- bind_rows(
  screening_data |> filter(included_final == 1),
  screening_data |> filter(included_final == 0, human_code == 1) |> slice_sample(n = 50),
  screening_data |> filter(included_final == 0, human_code == 0, !id %in% earlier_fp_ids) |> slice_sample(n = 50),
  screening_data |> filter(id %in% earlier_fp_ids) |> slice_sample(n = min(100, length(earlier_fp_ids)))
) |>
  mutate(earlier_fp = id %in% earlier_fp_ids) # TRUE = was wrongly included by the earlier AI run

# The test set is not a random sample, so the share of studies the AI includes in it says nothing about the
# whole dataset. Each group is therefore weighted by its size in the whole dataset (see below).
add_stratum <- function(x) {
  x |> mutate(stratum = case_when(
    included_final == 1       ~ "final_included",
    id %in% earlier_fp_ids    ~ "earlier_ai_false_positive",
    human_code == 1           ~ "human_in_not_final",
    TRUE                      ~ "human_out_other"
  ))
}
test_data <- add_stratum(test_data)
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
  reasoning_effort = "high",
  verbosity = "low",
  overinclusive = FALSE,
  decision_description = TRUE
)

plan(sequential)

AIscreenR::screen_analyzer(res_test)
table(AI = res_test$answer_data$decision_binary, human = res_test$answer_data$human_code)
table(AI = res_test$answer_data$decision_binary, final = res_test$answer_data$included_final)

# Human excluded studies only: random ones vs the ones the earlier AI run wrongly included
res_test$answer_data |>
  filter(human_code == 0) |>
  count(earlier_fp, AI = decision_binary)

# Estimated share of ALL studies the AI would include (the earlier full run included 23.7%).
# Note: earlier_fp_ids come from screening_Endedijk_ai.rds, so do not overwrite that file before testing.
group_sizes <- screening_data |> add_stratum() |> count(stratum, name = "n_all")
res_test$answer_data |>
  group_by(stratum) |>
  summarise(ai_include_rate = mean(decision_binary == 1), n_test = n()) |>
  left_join(group_sizes, by = "stratum") |>
  mutate(ai_included_all = ai_include_rate * n_all) |>
  summarise(estimated_ai_included_share = sum(ai_included_all) / sum(n_all))

# Finally included studies the AI excluded, with its explanation
fn_final_included <- res_test$answer_data |>
  filter(decision_binary == 0, included_final == 1) |>
  select(id, title, abstract, detailed_description)

# Short version (title and the start of the explanation), so the whole list fits in a copy-paste.
# Use the abstract only for a single study: fn_final_included$abstract[fn_final_included$id == "3801"]
cat("Final included, missed by the AI:", nrow(fn_final_included), "\n")
for (i in seq_len(nrow(fn_final_included))) {
  cat("[", fn_final_included$id[i], "] ", fn_final_included$title[i], "\n   ",
      substr(fn_final_included$detailed_description[i], 1, 250), "\n", sep = "")
}

# Human excluded studies the AI included, with its explanation
fp_human_code <- res_test$answer_data |>
  filter(decision_binary == 1, human_code == 0) |>
  select(id, title,abstract, detailed_description)

cat("Human excluded, included by the AI:", nrow(fp_human_code), "\n")
for (i in seq_len(nrow(fp_human_code))) {
  cat("[", fp_human_code$id[i], "] ", fp_human_code$title[i], "\n   ",
      substr(fp_human_code$detailed_description[i], 1, 250), "\n", sep = "")
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
  reasoning_effort = "high",
  verbosity = "low",
  overinclusive = FALSE,
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
