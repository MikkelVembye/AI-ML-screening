# Data manipulation

Records in data: 4,210. Records to screen: 4,094 (116 removed: 14 by CiteSource and 102 as same-title duplicates). Line numbers refer to `Endedijk/run_screening_Endedijk.r`.

**1. Duplicates removed: 116 records**

*CiteSource: 14 records* (line 71)

- None of the 14 was included by humans or finally included.
- Removed ids: 353, 373, 799, 851, 872, 903, 965, 2292, 2855, 2885, 2912, 3088, 3982, 4055.

*Same title: 102 records* (lines 83-117)

- The script removes records with the same title after CiteSource. It ignores case, punctuation and HTML codes such as `&apos;`. This finds studies that CiteSource missed because the title was written in a different way.
- 102 studies had two copies. One copy of each was kept (the one with the longest abstract), so 102 extra copies were removed.
- Of the 102 removed copies, 6 were included by humans and 5 were finally included. For all 6, the kept copy is also included by humans.
- All 5 finally included studies that lost a copy are still in the data. See the table below.
- 4 of the 102 studies had copies with different labels. The kept copy gets the highest label of its copies (`max`, lines 94-95), so no label is lost.

| Author(s) | Year | Study | Kept copy | Removed copy |
| --- | --- | --- | --- | --- |
| Sabol TJ; Bohlmann NL; Downer JT | 2018 | Low-Income Ethnically Diverse Children's Engagement as a Predictor of School Readiness above Preschool Classroom Quality | id 3570 | id 3810 |
| Zhang F; Jiang Y; Lei X; Huang S | 2019 | Teacher Power and Children's Loneliness: Moderating Effects of Teacher-Child Relationships and Peer Relationships | id 3893 | id 3829 |
| Kim H; Cameron CE; Kelly CA; West H; Mashburn AJ; Grissmer DW | 2019 | Using an Individualized Observational Measure to Understand Children's Interactions in Underserved Kindergarten Classrooms | id 3894 | id 3871 |
| Roubinov DS; Boyce WT; Bush NR | 2019 / 2020 | Informant-specific reports of peer and teacher relationships buffer the effects of harsh parenting on children's oppositional defiant disorder during kindergarten | id 3881 (2019) | id 3811 (2020) |
| National Institute of Child Health and Human Development, Early Child Care Research Network | 2000 / 2002 | The relation of global first-grade classroom environment to structural classroom features and teacher and student behaviors | id 3734 (2000, no author) | id 692 (2002) |

**2. Records without abstract removed: 0 records** (lines 121-132)

- Every record in the data has an abstract, so nothing is removed here.
- All 288 finally included studies had an abstract.

**4 records got a changed label**

| Author(s) | Year | Study | Change |
| --- | --- | --- | --- |
| Sabol TJ; Bohlmann NL; Downer JT | 2018 | Low-Income Ethnically Diverse Children's Engagement as a Predictor of School Readiness above Preschool Classroom Quality (id 3570) | finally included 0 to 1 |
| Zhang F; Jiang Y; Lei X; Huang S | 2019 | Teacher Power and Children's Loneliness: Moderating Effects of Teacher-Child Relationships and Peer Relationships (id 3893) | finally included 0 to 1 |
| Kim H; Cameron CE; Kelly CA; West H; Mashburn AJ; Grissmer DW | 2019 | Using an Individualized Observational Measure to Understand Children's Interactions in Underserved Kindergarten Classrooms (id 3894) | finally included 0 to 1 |
| Moffat AK; Redmond G; Raghavendra P | 2019 | The Impact of Social Network Characteristics and Gender on Covert Bullying in Australian Students with Disability in the Middle Years (id 4307) | human_code 0 to 1 |

- The first three changed because the removed copy was finally included and the kept copy was not (lines 94-95, `max`).
- The last one is finally included but was not marked as included by humans. Line 78 sets `human_code = 1` for all finally included studies. It also raised the removed copy id 692 (NICHD 2002), but that copy was then removed as a duplicate.
- Ni H; Li C; Li B; Xi H (2020), *Elementary students' perceptions of classroom resilience-promoting factors in China and the United States*, also had copies with different labels (id 3885 kept, id 3897 removed). The kept copy already had the highest label, so nothing changed.

## Overall effect

- Number included by humans: 356 to 352. This is 356 + 2 (raised to 1 because they are finally included: ids 4307 and 692) − 6 (removed as duplicates) = 352.
- Finally included: 288 to 286. Two studies (Roubinov et al. and the NICHD study) were counted twice in the original data, because both copies were marked as finally included. The other 3 duplicated studies had only one copy marked.
- 4,094 records are screened (3,742 human excluded and not finally included, 66 human included but not finally included, 286 finally included).

## Data versus the information in the paper


| | Paper | CSV |
| --- | --- | --- |
| Records screened | 4,364 | 4,210 |
| Passed title and abstract | 926 (21%) | not in the data |
| Included after full text | 385 (8.8%) | `label_included = 1`: 356 (8.5%) |
| Included in the meta-analysis | 297 papers (6.8%) | `included_final = 1`: 288 (6.8%) |

- **Labels are from a later stage than the AI's**
  - Shares match full text and final inclusion, not title and abstract
  - Many abstract-relevant records failed at full text, so all are `label_included = 0`
  - The AI includes many of them, so false positives and specificity mislead
- **The paper's criteria need the full text**
  - Individual-level scores on both relationships and "peers = classmates": mostly full text only
  - Effect sizes taken from the full text
  - Included but not recognisable from the abstract: Brandseth et al. 2019 (class belonging, N = 574) and Haidari and Karakuş 2019 (scale validation, N = 339)
  - "Support from teachers and peers" studies: no abstract wording separates included from excluded
