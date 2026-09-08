# Grant-to-Policy pipeline (consolidated, shareable)

This is the analysis pipeline for our project on how IES-funded education research gets
cited in policy documents.

The workflow is consolidated into 15 scripts, grouped by data source
and the pipeline phase. All external responses are cached, so re-runs make no
new API calls. (this was a headache before I figured it out)

Paths use `here()`, which anchors to the project root, so these run from this
folder. Before running anything that touches the network, set your email: the six
network scripts carry `your_email_here@email.com` as a polite-pool placeholder, so
search-and-replace it with your address across the folder.

## Acquisition (run first; each is independently cached)

| # | Script | Network |
|---|---|---|
| 1 | `00a_scrape_ies_award_pages.R` - IES award metadata + product citations | IES site |
| 2 | `00b_acquire_grant_dois.R` - grant DOIs (ERIC -> OpenAlex -> Crossref) | ERIC/OpenAlex/Crossref |
| 3 | `00c_acquire_meta_analyses.R` - meta route, `is_meta_analysis` | OpenAlex/Europe PMC |
| 4 | `00d_enrich_universe_pubtypes.R` - universe abstracts + pubtypes | EPMC/S2/Crossref |
| - | `00e_extract_overton_dump.R` - filter the local Overton dump | local only |

`00a` before `00b` (DOI recovery reads the award-page titles + product citations).
`00c`/`00d`/`00e` need the universe pair table from stage 01.

## Analysis (run in numeric order)

| # | Script | Merges former |
|---|---|---|
| 1 | `01_build_grant_universe.R` | 01 |
| 2 | `02_link_grants_to_publications.R` | 02 |
| 3 | `03_build_grant_master.R` | 03 + 03b |
| 4 | `04_classify_and_screen_universe.R` | 04 + 07 + 13 + 14 |
| 5 | `05_policy_doc_analysis.R` | 05 |
| 6 | `06_reach_cohorts_and_sensitivity.R` | 06 + 11 + 12 + 15 |
| 7 | `07_grant_timeline_tables.R` | 16 + 17 + 18 + 19 |
| 8 | `08_reach_profile_and_lattice.R` | 22 + 23 |
| 9 | `09_descriptives_and_exhibits.R` | 08 + 10 |
| 10 | `10_audit_reconcile_dictionary.R` | 09 + 20 + 99 |

Inside a merged script, each former stage sits under a `PART A - name` divider
that keeps its original inline comments, under one library block at the top. The
small redundant groups (the acquisition scripts, `07`) are merged more tightly:
shared setup runs once and results pass in memory.

## Run order / two-pass loops

The analysis side has two intentional two-pass loops, same as the original
pipeline:
- `04` writes the off-topic exclusion list that `01` reads, so a full refresh is
  `01 -> 04 -> 01`. On a first-ever run the list is simply absent and `01` proceeds.
- `04`'s grant classifications optionally feed `03`'s covariate recovery; `03`
  runs before `04`, so it picks them up on a second pass (absent on the first).

## Notes

- **`00c` section 1 is OFF by default** (`REFETCH_META_CITERS <- FALSE`): a ~48 min
  OpenAlex re-derivation whose output isn't wired downstream (sections 2-4 read
  the curated legacy link file). Flip it only to rebuild that file.
- **`00a` drops `ies_purpose`** (matches the former `00c2`); recoverable from the
  cached HTML if a keyword classifier needs it again.
- **`00e` reads the 2.9 GB `df_policy_doc_info.csv` once** (former 00f + 00g read
  it twice).
- **`04` and `10` touch the network** (they contain former 07 and 09, the OpenAlex
  enrichment / validation passes); everything else on the analysis side is local.
- **`06` PART B (former 11) Overton category shares use a distinct-doc denominator** (not
  the raw row count, which double-counts duplicate doc rows); see the comment at
  `total_docs_in_dump` in `06_reach_cohorts_and_sensitivity.R` for the why.

## Standalone companions (not in the numbered run order)

- **`policy_to_policy_crosswalk_and_workflow.R`** - second-order (policy-cites-policy)
  amplification crosswalk.
- **`us_gov_population_fed_state.R`** - full Overton US-government population baseline,
  split Federal / State / Local. One pass over the `df_policy_doc_info.csv` dump;
  classifies each of the 130 US-gov
  publisher slugs via the committed `data/_rewrite_outputs/us_gov_slug_level_lookup.csv`.
  This is the population denominator behind the IES-subset federal/state split in `05`/`06`.
- **`sankey_grant_doi_meta_policy.R`** - grant-level Sankey with five columns in order
  (Grants -> DOIs -> Meta -> Non-gov policy -> Gov policy) where a grant drops off at
  whatever stage its reach ends. Reads the project-root megafile; writes
  `outputs/_sankey/sankey_grant_doi_meta_policy.{html,png}`.
- **`plot_sankey_grant_reach.R`** - the same grant-level Sankey, recolored and
  high-resolution, with the four pathways to government policy marked on the flows
  (P1 direct, P2 via non-gov, P3 via meta, P4 via meta then non-gov). Writes
  `outputs/_sankey/sankey_grant_reach.{html,png}`.

The originals in `scripts_rewrite/` are left untouched for diffing.
