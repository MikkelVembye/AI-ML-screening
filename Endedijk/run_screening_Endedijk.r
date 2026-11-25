library(dplyr)
library(AIscreenR)
library(future)

#--------------------------------------------------------------------------
# Prompt
#--------------------------------------------------------------------------
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
  confidence = TRUE,
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
