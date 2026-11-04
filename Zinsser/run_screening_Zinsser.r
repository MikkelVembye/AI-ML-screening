library(dplyr)
library(AIscreenR)
library(future)
library(CiteSource)

#--------------------------------------------------------------------------
# Prompt
#--------------------------------------------------------------------------
# Based on the inclusion criteria and the abstract screening procedure in Zinsser, Silver, Shenberger & Jackson
# (2022), A Systematic Review of Early Childhood Exclusionary Discipline, Review of Educational Research.
prompt <- "Evaluate the following study based on the selection criteria
 for a systematic review of early childhood exclusionary discipline, that is, the suspension and expulsion of
 young children from early care and education. The review gathers peer-reviewed empirical studies that explain
 or predict the exclusion of children from birth to age 5 from early childhood care and education (ECCE)
 settings: home-based, center-based and school-based care such as child care, daycare, family child care,
 preschool and pre-kindergarten. Exclusionary discipline means the ways a child can be removed from a child
 care or preschool arrangement for behavioral reasons: expulsion (permanent removal, being asked to leave the
 program), suspension (not being allowed to attend for a number of days) and early pick-ups (parents being asked
 to take the child home early because of behavior). The review also includes studies of the risk of exclusion,
 for example teachers asking for a child to be removed, teachers considering or perceiving a risk of expulsion,
 or parents being warned that their child may be excluded. Quantitative, qualitative and mixed-methods studies
 all count, and the studies can be about the rates of exclusion, how it is defined or measured, which children
 or programs are affected, the factors related to it (child, family, teacher, administrator, program and policy
 factors), or programs that are evaluated on exclusion outcomes. You only have the title and the abstract and
 the full text is checked later, so if you are unsure, include the study.
 For each study, I would like you to assess in this order:
 1) Is the sample children under the age of 6 (birth to age 5), or early childhood care and education programs
 or their teachers, providers or directors? Studies in which the abstract describes a sample that is too old
 (school-age children over age 6, elementary, middle or high school students, adolescents) or programs that
 serve only older children do not count. If not, exclude study.
 2) Is the study from the United States? Exclude it only if the abstract says that the sample or the programs
 are in another country. If the abstract does not say where the study was done, answer yes. If not, exclude
 study.
 3) Does the study include a measure of exclusion (suspension, expulsion, early pick-ups, children asked to leave
 or removed from the program because of behavior) or of exclusion risk (teachers' requests for removal or
 perceived expulsion risk)? Studies that only use the words expulsion or suspension in another meaning, for
 example the expulsion of food in infant feeding studies, or studies that are not about school or child care
 discipline do not count. If not, exclude study.
 4) Is it an empirical study published in a peer-reviewed academic journal in English? Book chapters, theses and
 dissertations, white papers, policy briefs, reports by state agencies and other gray literature do not count,
 and neither do commentaries, opinion pieces, policy statements and literature reviews. If not, exclude study.
 If all four questions are answered yes or probably yes, include the study."


#--------------------------------------------------------------------------
# Data
#--------------------------------------------------------------------------
raw <- read.csv("Zinsser/data/27_Zinsser_data.csv", stringsAsFactors = FALSE)

zinsser_raw <- raw |>
  transmute(
    id             = as.character(Key),
    title          = title,
    abstract       = abstract,
    author         = authors,
    year           = year,
    doi            = doi,
    human_code     = as.numeric(label_included),
    included_final = as.integer(included_final)
  )

unique_citations <- CiteSource::dedup_citations(zinsser_raw, manual = TRUE)$unique

# If a record has included_final = 1, then human_code must be 1 (included). If not, set human_code to 1.
unique_citations <- unique_citations |> mutate(human_code = ifelse(included_final == 1, 1, human_code))

stopifnot(!anyDuplicated(unique_citations$id), !anyNA(unique_citations$human_code), !anyNA(unique_citations$included_final))

# All records without an abstract are removed before screening, including finally included studies
no_abstract <- is.na(unique_citations$abstract) | trimws(unique_citations$abstract) == "" | unique_citations$abstract == "No information"
removed_ids <- unique_citations$id[no_abstract]
included_final_ids <- unique_citations$id[no_abstract & unique_citations$included_final == 1] # finally included, removed too

cat("Records in data:                               ", nrow(zinsser_raw), "\n")
cat("Records without abstract (all removed):        ", sum(no_abstract), "\n")
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

for (i in seq_len(nrow(false_negatives))) {
  cat("\nFalse negative study", i, "of", nrow(false_negatives), "\n")
  cat("ID:", false_negatives$id[i], "\n")
  cat("Title:", false_negatives$title[i], "\n")
  cat("Abstract:", false_negatives$abstract[i], "\n")
  cat("Human code:", false_negatives$human_code[i], "\n")
  cat("AI code:", false_negatives$decision_binary[i], "\n")
  cat("Finally included:", false_negatives$included_final[i], "\n")
}

saveRDS(res_all, "Zinsser/data/screening_Zinsser_ai.rds")
res_all <- readRDS("Zinsser/data/screening_Zinsser_ai.rds") # to reload the results
#--------------------------------------------------------------------------
# Prepare the data for the simulation
#--------------------------------------------------------------------------
zinsser_data <- res_all$answer_data |>
  mutate(
    id              = as.character(id),
    human_code      = as.numeric(human_code),
    decision_binary = as.numeric(decision_binary),
    included_final  = as.integer(included_final),
    human_and_ai_in = as.integer(decision_binary == 1 & human_code == 1) # included by both the AI and the humans
  )

cat("Prepared data:", nrow(zinsser_data), "records |",
    "AI included:", sum(zinsser_data$decision_binary == 1),
    "| human included:", sum(zinsser_data$human_code == 1),
    "| human and AI:", sum(zinsser_data$human_and_ai_in == 1),
    "| finally included:", sum(zinsser_data$included_final == 1), "\n")

attr(zinsser_data, "data_name") <- "zinsser_data"
saveRDS(zinsser_data, "Zinsser/data/zinsser_cleaned.rds")
