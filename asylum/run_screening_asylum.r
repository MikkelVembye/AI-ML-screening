library(dplyr)
# Install aiscreenR from GitHub
remotes::install_github("MikkelVembye/AIscreenR", build_vignettes = TRUE)
library(AIscreenR)
library(future)
library(CiteSource)
packageVersion("rlang")
#--------------------------------------------------------------------------
# Prompt
#--------------------------------------------------------------------------
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
# Data
#--------------------------------------------------------------------------
raw <- readRDS("asylum/data/all_studies_with_labels.rds")

asylum_raw <- raw |>
  transmute(
    # 5 of the benchmark records have no eppi_id (empty), so they get their own id
    id             = ifelse(is.na(eppi_id) | eppi_id == "", paste0("no_eppi_id_", row_number()), as.character(eppi_id)),
    title          = title,
    abstract       = abstract,
    author         = author,
    year           = year,
    doi            = doi,
    human_code     = as.numeric(human_code),
    # the 12 records labelled "benchmark" are the studies included in the review (included_from_review.ris)
    included_final = as.integer(grepl("benchmark", cite_label))
  )

asylum_unique <- CiteSource::dedup_citations(asylum_raw, manual = TRUE)$unique |>
  # CiteSource returns the labels as text ("0"/"1"), so they are converted back to numbers
  mutate(human_code = as.numeric(human_code), included_final = as.integer(included_final))

cat("Finally included before / after CiteSource:    ", sum(asylum_raw$included_final), "/", sum(asylum_unique$included_final), "\n")

# If a record has included_final = 1, then human_code must be 1 (included). If not, set human_code to 1.
asylum_unique <- asylum_unique |> mutate(human_code = ifelse(included_final == 1, 1, human_code))

# Duplicates:
duplicate_check <- asylum_unique |>
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
  arrange(desc(abstract_chars), id, .by_group = TRUE) |>
  mutate(kept = row_number() == 1) |>
  ungroup()

duplicate_copies <- duplicate_check |>
  filter(n_copies > 1) |>
  arrange(title_key, desc(kept)) |>
  select(title_key, kept, n_copies, id, title, year, abstract_chars,
         human_code_original, included_final_original, human_code, included_final)
#saveRDS(duplicate_copies, "asylum/data/duplicates_asylum.rds")

cat("Duplicate studies (same title):                ", n_distinct(duplicate_copies$title_key), "\n")
cat("  extra copies removed:                        ", sum(!duplicate_copies$kept), "\n")
cat("  removed copies that were finally included:   ", sum(!duplicate_copies$kept & duplicate_copies$included_final_original == 1), "\n")

unique_citations <- duplicate_check |>
  filter(kept) |>
  select(-title_key, -abstract_chars, -n_copies, -human_code_original, -included_final_original, -kept)

stopifnot(!anyDuplicated(unique_citations$id), !anyNA(unique_citations$human_code), !anyNA(unique_citations$included_final))

# All records without an abstract are removed before screening, including finally included studies
no_abstract <- is.na(unique_citations$abstract) | trimws(unique_citations$abstract) == "" | unique_citations$abstract == "No information"
removed_ids <- unique_citations$id[no_abstract]
included_final_ids <- unique_citations$id[no_abstract & unique_citations$included_final == 1] # finally included, removed too

cat("Records in data:                               ", nrow(asylum_raw), "\n")
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
  reasoning_effort = "medium",
  verbosity = "low",
  overinclusive = FALSE, # default TRUE lets the AI answer "1.1" (unsure), which counts as include
  decision_description = TRUE # explanations, to see why records were wrongly excluded/included
)

plan(sequential)

AIscreenR::screen_analyzer(res_test)

#   promptid model       reps p_agreement recall specificity n_screened n_missing
#      <int> <chr>      <dbl>       <dbl>  <dbl>       <dbl>      <int>     <int>
# 1        1 gpt-6-luna     1       0.741  0.532           1        112         0

# # A tibble: 1 × 8
#   promptid model          reps p_agreement recall specificity n_screened n_missing
#      <int> <chr>         <dbl>       <dbl>  <dbl>       <dbl>      <int>     <int>
# 1        1 gpt-6.1-terra     1       0.795  0.629           1        112         0

table(AI = res_test$answer_data$decision_binary, human = res_test$answer_data$human_code)
table(AI = res_test$answer_data$decision_binary, final = res_test$answer_data$included_final)

# Finally included studies the AI excluded, with its explanation
fn_final_included <- res_test$answer_data |>
  filter(decision_binary == 0, included_final == 1) |>
  select(id, title, abstract, detailed_description)

for (i in seq_len(nrow(fn_final_included))) {
  cat("ID:", fn_final_included$id[i], "\n")
  cat("Title:", fn_final_included$title[i], "\n")
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
  reasoning_effort = "medium",
  verbosity = "low",
  overinclusive = FALSE,
  decision_description = FALSE
)

plan(sequential)

AIscreenR::screen_analyzer(res_all)
#   promptid model          reps p_agreement recall specificity n_screened n_missing
#      <int> <chr>         <dbl>       <dbl>  <dbl>       <dbl>      <int>     <int>
# 1        1 gpt-6.1-terra     1       0.944  0.581       0.958       8113         0
table(AI = res_all$answer_data$decision_binary, human = res_all$answer_data$human_code)

table(AI = res_all$answer_data$decision_binary, final = res_all$answer_data$included_final)

# False negatives: included by humans but excluded by AI
false_negatives <- res_all$answer_data |>
  filter(human_code == 1 & decision_binary == 0) |>
  select(id, title, abstract, human_code, decision_binary, included_final)

cat("False negatives (human in, AI out):", nrow(false_negatives),
    "| of which finally included:", sum(false_negatives$included_final == 1), "\n")

saveRDS(res_all, "asylum/data/screening_asylum_ai.rds")
saveRDS(removed_ids, "asylum/data/screening_asylum_removed_no_abstract_ids.rds")

#--------------------------------------------------------------------------
# Prepare the data for the simulation
#--------------------------------------------------------------------------
asylum_data <- res_all$answer_data |>
  mutate(
    id              = as.character(id),
    human_code      = as.numeric(human_code),
    decision_binary = as.numeric(decision_binary),
    included_final  = as.integer(included_final),
    human_and_ai_in = as.integer(decision_binary == 1 & human_code == 1) # included by both the AI and the humans
  )

stopifnot(!anyDuplicated(asylum_data$id), !anyNA(asylum_data$decision_binary))

cat("Prepared data:", nrow(asylum_data), "records |",
    "AI included:", sum(asylum_data$decision_binary == 1),
    "| human included:", sum(asylum_data$human_code == 1),
    "| human and AI:", sum(asylum_data$human_and_ai_in == 1),
    "| finally included:", sum(asylum_data$included_final == 1), "\n")

attr(asylum_data, "data_name") <- "asylum_data"
saveRDS(asylum_data, "asylum/data/asylum_cleaned.rds")