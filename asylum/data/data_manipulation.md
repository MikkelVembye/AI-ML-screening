# Data manipulation

**1. Duplicates removed: 418 records**

- CiteSource removes duplicates first (line 81). Then the script removes records with the same title (lines 90-122).
- Of the 418, 41 were included by humans and 377 were excluded.
- For 36 of the 41, a copy with the same title was kept. The other 5 were matched by CiteSource's own duplicate detection (not by title).
- One of the 12 finally included studies was among the removed copies: Robjant K, Robbins I, Senior V (2009). *Psychological distress amongst immigration detainees: a cross-sectional questionnaire study* (id 10234623). The study is not lost: the other copy (id 10232135) was kept and is now marked "finally included".

**2. Records without abstract removed: 1,248 records** (lines 126-137)

- All have an empty abstract. None has the text "No information".
- 232 of them were included by humans and 1,016 were excluded.
- None of them is finally included, so all 12 finally included studies are still in the data.
- The ids are saved in `screening_asylum_removed_no_abstract_ids.rds`.

**10 records got a changed label**

When two copies have the same title, the kept copy gets the highest label of the two (`max`, lines 102-103). Because of this, 10 records changed label:

| Author(s)                      | Year | Study                                                                                                     | Change                  |
| ------------------------------ | ---- | --------------------------------------------------------------------------------------------------------- | ----------------------- |
| Robjant K; Robbins I; Senior V | 2009 | Psychological distress amongst immigration detainees: a cross-sectional questionnaire study (id 10232135) | finally included 0 → 1 |
| Ager A; Ager W; Long L         | 1995 | The Differential Experience of Mozambican Refugee Women and Men                                           | human_code 0 → 1       |
| Reynolds B; White J            | 2010 | Seeking asylum and motherhood: health and wellbeing needs                                                 | 0 → 1                  |
| Nawyn SJ                       | 2012 | Managing the Undesirables: Refugee Camps and Humanitarian Government                                      | 0 → 1                  |
| Silove D                       | n/a  | The asylum debacle in Australia: a challenge for psychiatry                                               | 0 → 1                  |
| Elmeroth E                     | 2003 | From Refugee Camp to Solitary Confinement: Illiterate adults learn Swedish as a second language           | 0 → 1                  |
| Gillen S                       | 2003 | Asylum Seekers (id 10226122)                                                                              | 0 → 1                  |
| Fitzpatrick J                  | n/a  | Temporary protection of refugees: Elements of a formalized regime                                         | 0 → 1                  |
| Wong MG                        | 1985 | Korean Immigrants in America: A Structural Analysis of Ethnic Confinement and Adhesive Adaptation         | 0 → 1                  |
| Mansouri F; Cauchi S           | 2006 | The psychological impact of extended temporary protection                                                 | 0 → 1                  |

(n/a = no year in the data.)

Line 88 also sets `human_code = 1` for all finally included studies. 

## Overall effect

- Number included by humans: 567 → 303. This is 567 − 41 (removed as duplicates) − 232 (removed because of missing abstract) + 9 (raised to 1 when labels of duplicates were merged) = 303.
- Finally included: 12 → 12. None is lost.
- 8,113 records are screened.
