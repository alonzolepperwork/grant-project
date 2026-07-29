# Script guide - what each script does + how it maps to the paper draft

A companion to `README.md`. The README has the run order and the consolidation
mapping; this file says, for each script, **what it produces** and **which part of
the draft it feeds**. Two views: scripts -> draft (Part 1), and draft -> scripts
(Part 2, the reverse lookup). This is the single draft<->code index for the project.

**Reproduction layer:** `../scripts_paper/` holds one thin script per paper section
(`02_methods.R`, `03_results_*.R`, ...) that *reads* the pipeline outputs and reports
every table + number for that section **in the order they appear in the draft**, with
the draft value noted in-line after each result (e.g. `nrow(grants)  #528 (draft: 528)`).
Those scripts are the per-number reproduction record; this guide is the section-level
map of which pipeline script builds the underlying data. Section II (Methods) is done
and every element reproduces; later sections are added as the audit proceeds.

Run order, in one line:
`00a -> 00b -> 01 -> (00c, 00d, 00e) -> 02 -> 03 -> 04 -> 01* -> 05 -> 06 -> 07 -> 08 -> 09 -> 10`
(`01*` = the second pass that reads 04's off-topic exclusion list.)

---

## Part 1 - Script-by-script

### Acquisition (pull the raw inputs; all cached)

**`00a_scrape_ies_award_pages.R`** - scrapes/parses the IES award pages: PI,
institution, award year, project type, title, program topic, and the product
citations. *Network: IES site.*
-> Draft: **Methods / Data / Grants** (the 528-grant table, grant types) and every
**Results / B** cut that uses institution / topic / project type.

**`00b_acquire_grant_dois.R`** - finds a DOI for each grant three ways in
sequence (ERIC -> OpenAlex -> Crossref). *Network: ERIC/OpenAlex/Crossref.*
-> Draft: **Methods / Data / Papers and DOIs** (how DOIs were identified; the
2,904-DOI / 3,227-pair counts).

**`00c_acquire_meta_analyses.R`** - builds the meta route: review-type works
citing grant DOIs, narrowed to true meta-analyses (`is_meta_analysis`).
*Network: OpenAlex/Europe PMC.*
-> Draft: **Methods / Data / Meta-analyses** (the 1,112 -> 336 -> 313 funnel).

**`00d_enrich_universe_pubtypes.R`** - adds abstracts + publication-type tags
(Europe PMC -> Semantic Scholar -> Crossref) so the empirical/non-empirical split
has evidence. *Network: EPMC/S2/Crossref.*
-> Draft: **Methods / Data / Papers and DOIs** (empirical vs non-empirical
classification; the 2,767 / 137 split).

**`00e_extract_overton_dump.R`** - filters the local 2026-03-05 Overton dump to
the IES-relevant first-order links, the policy->policy (second-order) edges, doc
metadata, and the IPTC/SDG/topic classification streams. *Local only.*
-> Draft: **Methods / Data / Overton data** (the corpus, 10.17M docs) and the raw
material for all linking/geography/category work downstream.

### Analysis (run in numeric order)

**`01_build_grant_universe.R`** (former 01) - assembles the 528-grant x
2,904-DOI universe; applies the empirical/non-empirical and off-topic filters.
-> Draft: **Methods / Data / Grants + Papers and DOIs** (universe definition,
general statistics).

**`02_link_grants_to_publications.R`** (former 02) - the linking engine: joins
grant DOIs and meta-analyses to Overton policy docs, producing the direct route,
meta route, the combined `routes` table, and the second-order amplification links.
-> Draft: **Methods / Analysis / B1 Linking** (direct + meta route; 8,209 / 8,421
first-order docs; 298 / 301 grants) and **Results / A** second-order layer.

**`03_build_grant_master.R`** (former 03 + 03b) - the grant-level master table
everything joins against; backfills covariates (institution type, topic, center).
-> Draft: provides the covariates behind **Results / B** (Differences).

**`04_classify_and_screen_universe.R`** (former 04 + 07 + 13 + 14) - classifies
grants (design/intervention/subject area), enriches DOIs with OpenAlex
bibliometrics, and screens out off-topic mis-attributed DOIs. *Network: OpenAlex
(PART B).*
-> Draft: **Methods / Data** (off-topic screening; `grant_subject_area` used in
**Results / B** by-topic) .

**`05_policy_doc_analysis.R`** (former 05) - characterizes the policy-doc corpus:
source type (gov / think tank / IGO), publishers, language, year, and the
**geography** (country canonicalization, region split, US federal vs state/local).
-> Draft: **Methods / Data / Overton data** (2,370 gov / 6,051 non-gov) and
**Results / C** (source-type table, geography by region, US fed/state).

**`06_reach_cohorts_and_sensitivity.R`** (former 06 + 11 + 12 + 15) - four reach
analyses: PART A the award->pub->meta->policy **timing chain** + cohort reach;
PART B the **Overton category baseline + lift**; PART C the **sensitivity**
restrictions; PART D **empirical vs non-empirical** reach. *Network: none (reads
the 5.5 GB classifications dump locally in PART B).*
-> Draft: **Results / A** (timing), **Results / C / Appendix** (category lift
table, the 10.2% / 50.7% education figures), **Results / D** (sensitivity),
**Methods / Data / Papers** (empirical vs non-empirical reach rates).

**`07_grant_timeline_tables.R`** (former 16 + 17 + 18 + 19) - per-grant
first-citation years + citation counts + year-by-year breakdown.
-> Draft: **Results / A** (the award->pub->policy timing table; first-gov/non-gov
years used in **Results / B**).

**`08_reach_profile_and_lattice.R`** (former 22 + 23) - two policy-reach
constructs: PART A the **DOI reach profile + the three Section C overlap
matrices**; PART B the **9-path pathway lattice**.
-> Draft: **Results / A** (pathway lattice), **Results / C** (DOI overlap 2x2
matrices).
(The survival / time-to-event datasets + the cure / hurdle models are a coauthor's
downstream step, built from the shared master file - not part of this pipeline.)

**`09_descriptives_and_exhibits.R`** (former 08 + 10) - the output layer:
descriptive tables + the shareable descriptives workbook, then the full
paper-exhibits Excel workbook (every table the paper might reference).
-> Draft: produces the **exhibit workbook** that backs Results A-D; the headline
descriptives and institution tables.

**`10_audit_reconcile_dictionary.R`** (former 09 + 20 + 99) - quality +
documentation: a validation undercount audit, a draft-number reconciliation, and
the data dictionary. *Network: OpenAlex (PART A audit).*
-> Draft: supports the **Limitations** discussion (undercount) and is the
provenance/reconciliation layer for the numbers in the draft.

### Standalone

**`policy_to_policy_crosswalk_and_workflow.R`** - combines the crosswalk extract
(00e), the second-order linking (02), and the chronological "who-cited-first"
workflow (09/10) into one script.
-> Draft: **Results / C / Discussion** (second-order amplification, the
think-tank-vs-government "who cites first" finding).

---

## Part 2 - Draft section -> script(s)

### Methods / A. Data
| Draft element | Script(s) | Key output |
|---|---|---|
| Grants (528, type table) | `00a`, `01` | `table_02_current_grant_universe.csv` |
| Papers and DOIs (2,904; empirical vs non-empirical) | `00b`, `00d`, `01`, `04` | `table_01_current_grant_doi_pair_union.csv` |
| Meta-analyses (1,112->336->313) | `00c`, `02` | `table_11_meta_work_metadata.csv` |
| Overton data (10.17M; gov/non-gov; category table) | `00e`, `02`, `05`, `06` (PART B) | `_overton_baseline/category_*.csv` |

### Methods / B. Analysis
| Draft element | Script(s) | Key output |
|---|---|---|
| B1 Linking (direct + meta route) | `02` | `table_01` direct, `table_03/05` meta, `table_07` routes |
| B2 Models (survival) | `08` (PART A) | `_time_series/grant_survival_*.csv` |

### Results / A. Grant-to-Policy Timelines
| Draft element | Script(s) | Key output |
|---|---|---|
| 9-path pathway lattice | `08` (PART C) | `_pathway_lattice/` |
| Timing table (award->pub->meta->non-gov->gov) | `06` (PART A), `07`, `08` (PART A) | `grant_survival_input.csv`, timing tables |
| Direct / Indirect / Second-order routes | `02`, `policy_to_policy_crosswalk_and_workflow` | `table_08` second-order links |

### Results / B. Differences in the pipeline
| Draft cut | Script(s) | Dataset |
|---|---|---|
| Institution type, subject area, NCER/NCSER, multi-grant, #DOIs/metas, #policy docs | `08` (PART A) + covariates from `03`; reach tables `06`, `09` | `grant_survival_input.csv` (one row per grant, all covariates + first-gov year + counts) |

### Results / C. Types of Policy Documents
| Draft element | Script(s) | Key output |
|---|---|---|
| Source types (gov / think tank / IGO) | `05` | `12_build_policy_doc_overview/` |
| Geography by region, US fed/state | `05` | `12b_build_policy_doc_geography/` |
| 3 DOI overlap matrices (gov/non-gov, US/non-US, fed/state) | `08` (PART B) | `overlap_matrix_*.csv` |
| Category lift table (Appendix) | `06` (PART B) | `_overton_baseline/category_overview.csv` |
| Second-order / think-tank-bridge | `02`, `policy_to_policy_crosswalk_and_workflow` | `_p2p_crosswalk_workflow/` |

### Results / D. Sensitivity Analyses
| Draft element | Script(s) | Key output |
|---|---|---|
| Strict / no-working-paper / no-multi-grant reach | `06` (PART C) | sensitivity table |
| Multi-grant-excluded survival variant | `08` (PART A) | `grant_survival_*_excl_multigrant.csv` |

### Discussion / Limitations
| Draft element | Script(s) |
|---|---|
| Publication-undercount limitation | `10` (PART A validation audit) |
| Data dictionary / number provenance | `10` (PART C) |

---

## Figures (Results A2 visuals)

The draft's three Results-A2 visuals each map to exactly one canonical script (all
gov-incl-IGO, matching the draft). Run the script to regenerate the figure.

| Draft visual | Script | Output |
|---|---|---|
| Sankey: Grant -> DOI -> Meta -> Non-gov policy -> Gov policy (5-column drop-off, custom palette) | `build_sankey_grant_doi_meta_policy.R` (shareable copy: `scripts_to_share/sankey_grant_doi_meta_policy.R`) | `outputs/_sankey/sankey_grant_doi_meta_policy.{html,png}` |
| Sankey: research reaches a policy doc that is itself cited by a further policy doc (onward citation) | `build_sankey_second_order_relabel.R` | `outputs/_pathway_lattice/sankey_second_order_relabel.{html,png}` |
| 9-path pathway lattice - path definitions + non-empirical DOI overlay | `build_pathway_lattice_govinclIGO.R` | `outputs/_pathway_lattice/pathway_lattice_govinclIGO.csv`, `nonemp_overlay_govinclIGO.csv` |

The base 9-path lattice (gov-EXCL-IGO) is stage `08` PART C / `23_build_pathway_lattice.R`;
the paper uses the incl-IGO version above. Four earlier figure iterations
(`build_sankey_lattice_flow.R`, `build_sankey_lattice_flow_simple.R`,
`build_sankey_grant_routes_govinclIGO.R`, `build_pathway_figures.R` - the route-volume
bar) were retired 2026-07.

---

## Note - three analyses that live OUTSIDE this folder

A few recent additions are not in `scripts_to_share/` (they sit in
`scripts_rewrite/` or as standalones):

- **Full-corpus Overton geographic baseline** (Section C / A1 "Overton" region
  columns), on the deduped 8,740,487-doc basis ->
  `scripts_rewrite/build_overton_geo_gov_by_region.R`
  -> `outputs/_overton_baseline/geo_region_full_distinct.csv`.
  (The older non-deduped `build_overton_geo_baseline.R` + its `geo_baseline_*.csv` /
  `geo_gov_by_region.csv` outputs were retired 2026-07 - they double-counted the
  ~1.43M duplicate doc rows. The Overton category dedup likewise now lives in stage
  11 / `06` PART B, so `recompute_overton_categories_dedup.R` was retired too.)
- **The mega-files** (denormalized grant->DOI->meta->policy tables) ->
  `scripts_rewrite/build_megafile_grant_doi_meta_policy.R`.
- **Policy-before-publication audit** (the timing grace filter's diagnostic) ->
  `scripts_rewrite/audit_policy_before_pub.R`.

The paste-ready filled draft tables (timing, Section B, Section C, geography,
overlap matrices) are assembled in `docs/draft_fill_tables_2026-06-09.md`.
