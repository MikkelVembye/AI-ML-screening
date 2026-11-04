library(dplyr)
library(AIscreenR)
library(future)
library(CiteSource)

#--------------------------------------------------------------------------
# Prompt
#--------------------------------------------------------------------------

# # Title and abstract screening criteria

# ## Purpose of this stage

# Title and abstract screening has one job: remove records that clearly cannot contribute data to the review, and pass everything else to full-text assessment. The two possible errors do not cost the same.

# A wrongly excluded record is lost permanently. It never reaches a reviewer who could find the correlation table buried in its results section. A wrongly included record costs a few minutes of full-text reading. The criteria are therefore written to favour recall over precision, and screeners resolve doubt in favour of inclusion.

# This also limits which of the review's eligibility criteria can be applied here. An included study must meet three conditions:

# 1. It administered the LASSI.
# 2. It measured an academic outcome.
# 3. It reports an extractable association between the two.

# From an abstract, usually only the first can be judged. Abstracts of dissertations and institutional reports, which make up more than half of the included studies, often describe LASSI use without naming the outcome variables. Correlation coefficients almost never appear at this level of detail. Screening at this stage therefore rests on two questions and a small set of exclusions that require explicit evidence.

# ## Criterion 1: the LASSI was administered to participants

# A record meets this criterion when the Learning and Study Strategies Inventory was completed by the participants in the study.

# **Version does not matter.** Any edition qualifies: the 1987 original, the second edition, the 2016 third edition, the High School version and the online version. The review treats these as one instrument and models version as a moderator.

# **Translations and partial use qualify.** Translated or culturally adapted versions qualify in any language. Studies that administered only some subscales qualify too. Motivation and time management are often used on their own, and excluding partial use would remove a known part of the evidence base.

# **The LASSI's role in the study is irrelevant.** It may be a predictor in a correlational study, a pre- and post-test measure in an intervention, or a diagnostic or placement tool in a student support programme. All count, because each can yield an association with an academic outcome.

# The criterion is not met when the LASSI is only mentioned rather than used. A sentence such as "developmental reading students completed the LASSI at intake" meets the criterion. A sentence such as "support programmes commonly use instruments such as the LASSI" does not.

# The criterion is also not met when the record concerns a different learning strategies instrument, such as the MSLQ, SSHA, SPQ or ASSIST, with no indication that the LASSI was also given.

# **False positives from the acronym.** The search used "LASSI" anywhere in the document, so several sources of false positives need to be excluded explicitly:

# - The LASSI-L (Loewenstein-Acevedo Scales for Semantic Interference and Learning), a cognitive assessment used in Alzheimer's disease research.
# - Lassi as an author's surname, which is common among Finnish researchers.
# - The yoghurt drink.
# - Records where the LASSI appears only in the reference list.

# **When the instrument is not named.** Where an abstract says learning or study strategies were measured but names no instrument, the record is unclear and goes forward. Many abstracts name the measure only in the methods section.

# This is the most expensive category of unclear decision, because it pulls in studies that turn out to use other instruments. It is still the right call, since the alternative is losing eligible studies. A record that names a different instrument is a clear exclusion, not an unclear one.

# ## Criterion 2: participants are students in formal education

# The review concerns learners whose academic outcomes are recorded by an educational institution. This covers:

# - Primary and secondary school pupils.
# - College and university students, including community colleges.
# - Developmental and remedial education.
# - Vocational and professional training programmes, such as nursing, medicine, pharmacy and teacher education.

# Professional programmes are included because the original review counted licensure examinations as test score outcomes.

# The criterion is not met when the sample is clearly outside formal education, for example employees assessed outside any training programme, or clinical populations. When the abstract does not describe the sample, the record is unclear and goes forward.

# ## Exclusions requiring positive evidence

# Three further grounds for exclusion apply at this stage. Each requires the abstract to state the disqualifying feature. The absence of information is not enough.

# **No primary data.** Narrative reviews, commentaries, editorials, theoretical papers and test manuals without empirical data are excluded. Reviews and meta-analyses should be tagged rather than simply discarded, because their reference lists feed backward citation searching.

# **Psychometric studies with no academic outcome.** Studies that examine only factor structure, reliability or translation equivalence are excluded, but only when the abstract makes clear that no academic outcome was collected. This needs care. Validation studies frequently report concurrent validity against GPA in a single table that the abstract never mentions.

# A reasonable test is whether the abstract lists the full set of measures and analyses. If it does, and none is academic, exclude. If it describes only the factor analysis and is silent about anything else, the record goes forward.

# **Only non-academic outcomes.** Studies that relate the LASSI solely to other psychological constructs, such as personality, self-efficacy, wellbeing or other strategy measures, are excluded when the abstract makes clear that these are the only variables. The same caution applies: an abstract that mentions only psychological correlates may still report grades in the full text.

# ## What does not count against inclusion

# Several features that matter later in the review play no part at this stage:

# - The abstract reports no correlation or effect size.
# - The abstract does not mention an academic outcome, unless it rules one out.
# - Language of publication. The review placed no restriction on language and translated non-English reports.
# - Publication status. Dissertations, theses, institutional reports, conference papers and practitioner newsletters such as *LASSI in Action* are all eligible.
# - Country.
# - Study design.
# - Publication year, provided it is 1987 or later, when the instrument was first published.

# Records without an abstract are screened on the title alone. If the title makes LASSI use plausible, the record goes forward. Many older dissertations and institutional reports are indexed without abstracts, and these are disproportionately likely to be eligible grey literature.

# ## Recording decisions

# Each record receives one of three decisions: include, exclude or unclear. Unclear records go to full-text review on the same footing as includes, but are recorded separately. The unclear rate is diagnostic: a high rate signals that screeners are reading the criteria differently, or that abstracts in this literature are too thin to support decisions.

# Exclusions are recorded with the first applicable reason, in this fixed order:

# 1. Not the LASSI instrument (acronym collision).
# 2. LASSI not administered.
# 3. Participants not students.
# 4. No primary data.
# 5. No academic outcome, on explicit evidence.

# A fixed order keeps exclusion counts comparable between screeners and makes the PRISMA flow diagram reproducible.

# ## Calibration and reliability

# Before full screening, two screeners should independently screen a pilot sample of around 100 records. They should then compute agreement and discuss every disagreement until the criteria are applied consistently. The original review reported coder agreement only for data extraction (92.4%), not for screening, so agreement at this stage should be measured and reported separately.

# The original review provides a reference point for the expected include rate. It forwarded 730 of 2,782 records (26%) to full text and ultimately included 158, which is 5.7% of all screened records. If a replication or update forwards substantially fewer than a quarter of records, the most likely cause is screeners excluding on missing outcome information. That is the error these criteria are designed to prevent.

prompt <- ""

#--------------------------------------------------------------------------
# Data
#--------------------------------------------------------------------------
raw <- read.csv("Fong/data/07_Fong_data.csv", stringsAsFactors = FALSE)

fong_raw <- raw |>
  transmute(
    id             = as.character(key),
    title          = title,
    abstract       = abstract,
    author         = authors,
    year           = year,
    human_code     = as.numeric(label_included),
    included_final = as.integer(included_final)
  )

unique_citations <- CiteSource::dedup_citations(fong_raw, manual = TRUE)$unique

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
saveRDS(duplicate_copies, "Fong/data/duplicates_Fong.rds")
write.csv(duplicate_copies, "Fong/data/duplicates_Fong.csv", row.names = FALSE)

cat("Duplicate studies (same title):                ", n_distinct(duplicate_copies$title_key), "\n")
cat("  extra copies removed:                        ", sum(!duplicate_copies$kept), "\n")
cat("  removed copies that were finally included:   ", sum(!duplicate_copies$kept & duplicate_copies$included_final_original == 1), "\n")
cat("  studies whose copies had different labels:   ",
    sum(tapply(duplicate_copies$human_code_original, duplicate_copies$title_key, function(x) length(unique(x))) > 1 |
        tapply(duplicate_copies$included_final_original, duplicate_copies$title_key, function(x) length(unique(x))) > 1), "\n")

unique_citations <- duplicate_check |>
  filter(kept) |>
  select(-title_key, -abstract_chars, -n_copies, -human_code_original, -included_final_original, -kept)

stopifnot(!anyDuplicated(unique_citations$id), !anyNA(unique_citations$human_code), !anyNA(unique_citations$included_final))

# All records without an abstract are removed before screening, including finally included studies
no_abstract <- is.na(unique_citations$abstract) | trimws(unique_citations$abstract) == "" | unique_citations$abstract == "No information"
removed_ids <- unique_citations$id[no_abstract]
included_final_ids <- unique_citations$id[no_abstract & unique_citations$included_final == 1] # finally included, removed too

cat("Records in data:                               ", nrow(fong_raw), "\n")
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
  decision_description = TRUE
)

plan(sequential)

AIscreenR::screen_analyzer(res_test)
table(AI = res_test$answer_data$decision_binary, human = res_test$answer_data$human_code)
table(AI = res_test$answer_data$decision_binary, final = res_test$answer_data$included_final)

# Finally included studies the AI excluded, with its explanation
res_test$answer_data |>
  filter(decision_binary == 0, included_final == 1) |>
  select(id, title, detailed_description)

# Human excluded studies the AI included, with its explanation
res_test$answer_data |>
  filter(decision_binary == 1, human_code == 0) |>
  select(id, title, detailed_description)

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

saveRDS(res_all, "Fong/data/screening_Fong_ai.rds")

#--------------------------------------------------------------------------
# Prepare the data for the simulation
#--------------------------------------------------------------------------
fong_data <- res_all$answer_data |>
  mutate(
    id              = as.character(id),
    human_code      = as.numeric(human_code),
    decision_binary = as.numeric(decision_binary),
    included_final  = as.integer(included_final),
    human_and_ai_in = as.integer(decision_binary == 1 & human_code == 1) # included by both the AI and the humans
  )

cat("Prepared data:", nrow(fong_data), "records |",
    "AI included:", sum(fong_data$decision_binary == 1),
    "| human included:", sum(fong_data$human_code == 1),
    "| human and AI:", sum(fong_data$human_and_ai_in == 1),
    "| finally included:", sum(fong_data$included_final == 1), "\n")

attr(fong_data, "data_name") <- "fong_data"
saveRDS(fong_data, "Fong/data/fong_cleaned.rds")
