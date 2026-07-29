# per-grant timeline tables for the paper's reach-over-time analyses - first-
# citation years, citation counts, the two joined, and the year-by-year breakdown
# - running off a single shared prep: the universe + award year, the grant-DOI
# pairs, the DOI publication years, and the direct policy citations get loaded and
# pre-award-filtered once, then four sections build the five output tables
#
# THE PRE-AWARD FILTER (applied once, used by all four): a paper published before
# the grant was funded cannot be a grant product (author-disambiguation artifacts
# in OpenAlex / ERIC), and a policy doc dated before the grant was funded cannot
# legitimately cite it (Overton date errors). both are dropped. DOIs / docs with
# a missing year are kept (can't verify either way)
#
# award_year fallback: the parsed IES page only resolves award_year for ~9% of
# grants (the post-2012 layout broke the parser), but every grant ID encodes the
# fiscal year in chars 8-9 (R305A040043 -> 2004), so we fall back to that
#
# all source counts mirror the rest of the pipeline: each policy doc is counted
# once per DOI it cites; NA source_type is ignored for the gov / non-gov buckets
#
# outputs (all under outputs/_paper_sensitivity/):
#   grant_first_citation_years.csv   - grant_id, award_year, first_{doi,gov,nongov}_year
#   grant_citation_counts.csv        - grant_id, award_year, n_{dois,gov,nongov}
#   grant_first_year_and_counts.csv  - the two above joined, counts beside years
#   grant_year_breakdown_long.csv    - one row per (grant, year) with that year's counts
#   grant_year_breakdown_wide.csv    - same, pivoted to one row per grant

#------------------
## 0. INITIALIZE ##
#------------------

library(tidyverse)
library(here)

# policy-before-publication grace (years). a policy doc dated more than this many
# years before the grant's EARLIEST publication cannot cite any of the grant's
# papers, so it must not set the grant's first-policy clock - it's an off-topic
# mis-link, a bad Overton date, or a working-paper-version year mismatch. the
# 1-year grace keeps online-first / early-view citations. applied to the FIRST-
# year (timing) events in section 2 only; the counts in sections 3 and 5 are left
# intact so reach is unchanged. same grace is used in 06_reach_cohorts_and_sensitivity.R
POLICY_PUB_GRACE <- 1L

out_dir <- here("outputs", "_paper_sensitivity")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

#--------------------------------------------------
## 1. SHARED PREP (universe, pairs, years, links) ##
#--------------------------------------------------

# 528-grant universe with award_year, ID-fallback applied
universe <- read_csv(
  here("outputs", "02_build_current_grant_doi_universe",
       "table_02_current_grant_universe.csv"), show_col_types = FALSE) %>%
  select(grant_id, award_year) %>%
  mutate(year_from_id = 2000L + as.integer(str_extract(grant_id, "[0-9]{2}(?=[0-9]{4}$)")),
         award_year = coalesce(award_year, year_from_id)) %>%
  select(grant_id, award_year)

pairs <- read_csv(
  here("outputs", "02_build_current_grant_doi_universe",
       "table_01_current_grant_doi_pair_union.csv"), show_col_types = FALSE) %>%
  distinct(grant_id, doi)

oa <- read_csv(
  here("outputs", "openalex_enrichment", "doi_metadata.csv"), show_col_types = FALSE) %>%
  distinct(doi, .keep_all = TRUE) %>%
  select(doi, publication_year)

direct <- read_csv(
  here("outputs", "05_rebuild_current_policy_routes",
       "table_01_current_direct_policy_links.csv"), show_col_types = FALSE) %>%
  select(grant_id, doi, policy_document_id, policy_source_type, policy_published_year)

# pre-award filter on grant-DOI pairs (keep publication_year for the year tables)
pairs_with_year <- pairs %>%
  left_join(oa, by = "doi") %>%
  left_join(universe, by = "grant_id")
pre_award <- pairs_with_year %>%
  filter(!is.na(publication_year), publication_year < award_year)
cat("Dropped", nrow(pre_award), "grant-DOI pairs where pub_year < award_year",
    "(across", n_distinct(pre_award$grant_id), "grants)\n")

pairs_kept      <- pairs_with_year %>% filter(is.na(publication_year) | publication_year >= award_year)
valid_grant_doi <- pairs_kept %>% distinct(grant_id, doi)

# restrict direct citations to surviving pairs, then drop citations dated before
# the grant was funded
direct <- direct %>%
  semi_join(valid_grant_doi, by = c("grant_id", "doi")) %>%
  left_join(universe, by = "grant_id") %>%
  filter(is.na(policy_published_year) | policy_published_year >= award_year) %>%
  select(-award_year)

#----------------------------------------
## 2. FIRST-CITATION YEARS (was 16) ##
#----------------------------------------

# "first" = earliest year observed in each window
first_doi <- pairs_kept %>%
  filter(!is.na(publication_year)) %>%
  group_by(grant_id) %>%
  summarize(first_doi_year = min(publication_year), .groups = "drop")

# policy-before-publication grace filter (timing events only). drop direct
# citations dated > POLICY_PUB_GRACE years before the grant's earliest publication
# BEFORE taking the min - such a doc cannot cite any of the grant's papers, so it
# would set a spurious (too-early) first-policy year. `direct` itself is left
# untouched so the counts in sections 3 and 5 keep full reach. grants with no
# resolvable first_doi_year keep all their citations
direct_timing <- direct %>%
  left_join(first_doi, by = "grant_id") %>%
  filter(is.na(first_doi_year) |
         policy_published_year >= first_doi_year - POLICY_PUB_GRACE) %>%
  select(-first_doi_year)

first_gov <- direct_timing %>%
  filter(policy_source_type == "government", !is.na(policy_published_year)) %>%
  group_by(grant_id) %>%
  summarize(first_gov_year = min(policy_published_year), .groups = "drop")

first_nongov <- direct_timing %>%
  filter(policy_source_type %in% c("think tank", "igo", "other"),
         !is.na(policy_published_year)) %>%
  group_by(grant_id) %>%
  summarize(first_nongov_year = min(policy_published_year), .groups = "drop")

years_out <- universe %>%
  left_join(first_doi, by = "grant_id") %>%
  left_join(first_gov, by = "grant_id") %>%
  left_join(first_nongov, by = "grant_id") %>%
  arrange(grant_id)
write_csv(years_out, file.path(out_dir, "grant_first_citation_years.csv"))

#----------------------------------------
## 3. CITATION COUNTS (was 17) ##
#----------------------------------------

n_dois <- valid_grant_doi %>%
  group_by(grant_id) %>%
  summarize(n_dois = n_distinct(doi), .groups = "drop")

n_gov <- direct %>%
  filter(policy_source_type == "government") %>%
  group_by(grant_id) %>%
  summarize(n_gov_citations = n(), .groups = "drop")

n_nongov <- direct %>%
  filter(policy_source_type %in% c("think tank", "igo", "other")) %>%
  group_by(grant_id) %>%
  summarize(n_nongov_citations = n(), .groups = "drop")

counts_out <- universe %>%
  left_join(n_dois, by = "grant_id") %>%
  left_join(n_gov, by = "grant_id") %>%
  left_join(n_nongov, by = "grant_id") %>%
  mutate(across(starts_with("n_"), ~ replace_na(.x, 0L))) %>%
  arrange(grant_id)
write_csv(counts_out, file.path(out_dir, "grant_citation_counts.csv"))

#-----------------------------------------------
## 4. COUNTS + FIRST YEARS, JOINED (was 18) ##
#-----------------------------------------------

# both tables are already in memory - no re-read
combined <- counts_out %>%
  left_join(years_out %>% select(grant_id, first_doi_year, first_gov_year, first_nongov_year),
            by = "grant_id") %>%
  select(grant_id, award_year,
         n_dois,             first_doi_year,
         n_gov_citations,    first_gov_year,
         n_nongov_citations, first_nongov_year) %>%
  arrange(grant_id)
write_csv(combined, file.path(out_dir, "grant_first_year_and_counts.csv"))

#--------------------------------------------
## 5. YEAR-BY-YEAR BREAKDOWN (was 19) ##
#--------------------------------------------

# per (grant, year) DOI publication count
doi_year <- pairs_kept %>%
  filter(!is.na(publication_year)) %>%
  group_by(grant_id, year = publication_year) %>%
  summarize(n_dois_published = n_distinct(doi), .groups = "drop")

# per (grant, year) gov / non-gov citation counts
cite_long <- direct %>%
  filter(!is.na(policy_published_year),
         policy_source_type %in% c("government", "think tank", "igo", "other")) %>%
  mutate(bucket = if_else(policy_source_type == "government",
                          "n_gov_citations", "n_nongov_citations")) %>%
  count(grant_id, year = policy_published_year, bucket) %>%
  pivot_wider(names_from = bucket, values_from = n, values_fill = 0L)

long <- full_join(doi_year, cite_long, by = c("grant_id", "year")) %>%
  mutate(across(c(n_dois_published, n_gov_citations, n_nongov_citations),
                ~ replace_na(.x, 0L))) %>%
  left_join(universe, by = "grant_id") %>%
  select(grant_id, award_year, year,
         n_dois_published, n_gov_citations, n_nongov_citations) %>%
  arrange(grant_id, year)
write_csv(long, file.path(out_dir, "grant_year_breakdown_long.csv"))

# wide companion: one row per grant, columns pivoted to metric_year
wide <- long %>%
  pivot_longer(c(n_dois_published, n_gov_citations, n_nongov_citations),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric,
           n_dois_published   = "n_dois",
           n_gov_citations    = "n_gov",
           n_nongov_citations = "n_nongov"),
         col = paste(metric, year, sep = "_")) %>%
  select(grant_id, award_year, col, value) %>%
  pivot_wider(names_from = col, values_from = value, values_fill = 0L) %>%
  arrange(grant_id)

# put year-suffixed columns in chronological order
yr_cols <- setdiff(names(wide), c("grant_id", "award_year"))
yr_cols <- yr_cols[order(as.integer(str_extract(yr_cols, "[0-9]{4}$")),
                         str_extract(yr_cols, "^[a-z_]+"))]
wide <- wide[, c("grant_id", "award_year", yr_cols)]
write_csv(wide, file.path(out_dir, "grant_year_breakdown_wide.csv"))

#----------------------
## 6. CONSOLE SUMMARY ##
#----------------------

cat("\nGrant timeline tables written to", out_dir, "\n")
cat("  grant_first_citation_years.csv :", nrow(years_out), "grants",
    "| has first_doi", sum(!is.na(years_out$first_doi_year)),
    "| first_gov", sum(!is.na(years_out$first_gov_year)),
    "| first_nongov", sum(!is.na(years_out$first_nongov_year)), "\n")
cat("  grant_citation_counts.csv      :", nrow(counts_out), "grants",
    "| total gov", sum(counts_out$n_gov_citations),
    "| total non-gov", sum(counts_out$n_nongov_citations), "\n")
cat("  grant_first_year_and_counts.csv:", nrow(combined), "grants\n")
cat("  grant_year_breakdown_long.csv  :", nrow(long), "rows",
    sprintf("(%d-%d)", min(long$year, na.rm = TRUE), max(long$year, na.rm = TRUE)), "\n")
cat("  grant_year_breakdown_wide.csv  :", nrow(wide), "grants\n")
