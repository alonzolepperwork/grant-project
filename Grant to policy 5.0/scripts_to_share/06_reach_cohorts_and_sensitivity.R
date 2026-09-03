# reach analyses for the paper: cohort/timing patterns, an Overton topic
# baseline, headline-reach sensitivity checks, and the empirical-vs-non-empirical
# split. these four analyses all read the finished routes / master and answer
# some reach question; they don't depend on each other, so the order below is
# just how the paper presents them
#
#   PART A: cohort reach rates (with censoring caveats), the full
#     award -> pub -> meta -> policy timing chain, skew-high / skew-low grants,
#     and the grant co-citation network
#   PART B: one row per IPTC top-level category with the full
#     Overton baseline share (8.74M distinct docs) beside the IES-cited share + lift
#     reads the 5.5 GB classifications dump (data.table fread, ~1-2 min)
#   PART C: four robustness restrictions on the headline reach figure
#     (strict / no-working-papers / no-multi-grant / combined) in one table
#   PART D: does a grant's meta-analysis output reach policy more,
#     per DOI and per grant, than its primary empirical papers?

#------------------
## 0. INITIALIZE ##
#------------------

library(tidyverse)
library(here)
library(data.table)

# policy-before-publication grace (years), used by the PART A timing chain. a
# policy doc dated more than this many years before the grant's EARLIEST
# publication cannot cite any of its papers, so it must not set the first-policy
# clock (the L4 lag) - it's an off-topic mis-link, a bad Overton date, or a
# working-paper-version year mismatch. the 1-year grace keeps online-first /
# early-view citations. TIMING only; reach counts are untouched. same grace is
# used in 07_grant_timeline_tables.R.
POLICY_PUB_GRACE <- 1L

# ============================================================================ #
# PART A - analyze_cohorts_and_reach
# ============================================================================ #
#load master (one row per grant) + policy routes (long table)
master <- read_csv(
  here("outputs", "06_build_grant_policy_master",
       "table_01_grant_policy_master.csv"),
  show_col_types = FALSE
)
routes <- read_csv(
  here("outputs", "05_rebuild_current_policy_routes",
       "table_07_current_grant_policy_doc_routes.csv"),
  show_col_types = FALSE
)
pairs <- read_csv(
  here("outputs", "02_build_current_grant_doi_universe",
       "table_01_current_grant_doi_pair_union.csv"),
  show_col_types = FALSE
)
pub_meta <- read_csv(
  here("outputs", "02b_build_publication_metadata",
       "table_02_publication_metadata_combined.csv"),
  show_col_types = FALSE
)
meta_links <- read_csv(
  here("outputs", "05_rebuild_current_policy_routes",
       "table_03_current_grant_to_meta_links.csv"),
  show_col_types = FALSE
)

#----------------------
## 1. TIMING CHAIN ##
#----------------------

#per-grant award year
award_year <- master %>%
  select(grant_id, award_year) %>%
  mutate(award_year = as.integer(award_year)) %>%
  filter(!is.na(award_year))

#L1: award_year -> first publication_year. earliest pub across all the
#grant's pubs
first_pub <- pairs %>%
  inner_join(pub_meta %>% select(doi, publication_year), by = "doi") %>%
  filter(!is.na(publication_year)) %>%
  group_by(grant_id) %>%
  summarize(first_pub_year = min(publication_year), .groups = "drop")

l1 <- first_pub %>%
  inner_join(award_year, by = "grant_id") %>%
  mutate(lag_years = first_pub_year - award_year) %>%
  filter(!is.na(lag_years))
#L1 mean = 4.7 yrs, median = 5

#L2: publication year -> first meta-analysis year. per source DOI
first_meta_per_doi <- meta_links %>%
  filter(!is.na(meta_year)) %>%
  group_by(grant_id, searched_doi) %>%
  summarize(first_meta_year = min(meta_year), .groups = "drop")

l2 <- first_meta_per_doi %>%
  inner_join(pub_meta %>% select(doi, publication_year),
             by = c("searched_doi" = "doi")) %>%
  filter(!is.na(publication_year)) %>%
  mutate(lag_years = first_meta_year - as.integer(publication_year)) %>%
  filter(!is.na(lag_years))
#L2 mean = 5.8 yrs, median = 5

#L4: award_year -> first policy citation (either route). uses the routes
#table which has policy_published_year for both direct and meta paths
#policy-before-publication grace filter applied (timing only): a policy doc dated
#> POLICY_PUB_GRACE years before the grant's first publication cannot cite any of
#its papers, so it must not set the first-policy clock. grants with no resolvable
#first_pub_year keep all their citations
first_policy <- routes %>%
  filter(!is.na(policy_published_year)) %>%
  left_join(first_pub, by = "grant_id") %>%
  filter(is.na(first_pub_year) |
         as.integer(policy_published_year) >= first_pub_year - POLICY_PUB_GRACE) %>%
  group_by(grant_id) %>%
  summarize(first_policy_year = min(as.integer(policy_published_year)),
            .groups = "drop")

l4 <- first_policy %>%
  inner_join(award_year, by = "grant_id") %>%
  mutate(lag_years = first_policy_year - award_year) %>%
  filter(!is.na(lag_years))
#L4 mean = 6.5 yrs, median = 6 across the 300 reached grants (with the grace
#filter; ~0.6yr higher than the unfiltered value as spurious early policy years
#are dropped)

#L4b: same as L4 but restricted to grants whose only route is meta. these
#are structurally slower because the meta-analysis has to come out first
meta_only_grants <- master %>%
  filter(has_only_meta_mediated_reach %in% c(TRUE, "TRUE", "true", 1)) %>%
  pull(grant_id)

l4b <- l4 %>%
  filter(grant_id %in% meta_only_grants) %>%
  mutate(meta_only_grant = TRUE)
#L4b meta-only mean = 11.8 yrs (vs 5.9 for any-route)

#------------------------------------
## 2. PER-COHORT REACH ##
#------------------------------------

#award year is the cohort. cells with very recent awards are mechanically
#low-reach (insufficient time elapsed) - that's the right-censoring
#problem. we report the table as-is; interpretation happens in the
#cohort narrative downstream

cohort_reach <- master %>%
  mutate(
    award_year = as.integer(award_year),
    reached_broad = has_any_current_policy_reach %in% c(TRUE, "TRUE", "true", 1)
  ) %>%
  filter(!is.na(award_year), award_year >= 2002) %>%
  group_by(`Award year` = award_year) %>%
  summarize(
    `Grants in cohort` = n(),
    `Grants reached` = sum(reached_broad),
    `% reached` = paste0(round(100 * mean(reached_broad), 1), "%"),
    `Mean docs (reached)` = round(mean(n_policy_docs[reached_broad],
                                        na.rm = TRUE), 1),
    `Median docs (reached)` = median(n_policy_docs[reached_broad],
                                      na.rm = TRUE),
    .groups = "drop"
  )
#table(master$has_any_current_policy_reach, master$award_year > 2017)

#---------------------------
## 3. SKEW / DOMINANT FEW ##
#---------------------------

#what fraction of all policy-doc reaches comes from the top decile of grants?
#expect heavy skew - a few grants dominate

grant_doc_counts <- routes %>%
  distinct(grant_id, policy_document_id) %>%
  count(grant_id, name = "n_docs")

top_decile_threshold <- quantile(grant_doc_counts$n_docs, 0.9, na.rm = TRUE)
top_decile <- grant_doc_counts %>% filter(n_docs >= top_decile_threshold)
share_top_decile <- sum(top_decile$n_docs) / sum(grant_doc_counts$n_docs)
#share_top_decile #~75% of all reaches from top 10%

skew_distribution <- tibble(
  Bucket = c("Zero reach", "1 doc", "2-5 docs", "6-20 docs", "21-50 docs", "51+ docs"),
  Threshold_low = c(0, 1, 2, 6, 21, 51),
  Threshold_high = c(0, 1, 5, 20, 50, Inf)
) %>%
  mutate(Grants = map2_int(Threshold_low, Threshold_high, function(lo, hi) {
    n_doc_join <- master %>% select(grant_id, n_policy_docs) %>%
      mutate(n_policy_docs = replace_na(n_policy_docs, 0L))
    sum(n_doc_join$n_policy_docs >= lo & n_doc_join$n_policy_docs <= hi)
  }))

#-------------------
## 4. ZERO REACH ##
#-------------------

#zero-reach grants: which grants got 0 policy citations?
#cross-cut by IES topic area, institution type, cohort

#topic_area handling. ies_program_topic is NA for 300 grants:
#  270 are scrape failures (no IES page data at all)
#  30 are special programs that don't have a single Program Topic field
#    on IES's side (R&D Centers, Preschool Curriculum Eval, etc.)
#strategy: fall back to ies_program_name when topic is missing but name
#exists, and label the truly-missing-from-scrape grants by IES Center
#(NCER for R305*, NCSER for R324*) from the grant_id prefix. that way the
#"(missing)" bucket shrinks from 300 to 0 and the labels are at least
#meaningful coarse classifications
fill_topic_area <- function(grant_id, ies_program_topic, ies_program_name) {
  center <- case_when(
    startsWith(grant_id, "R305") ~ "NCER (no topic on file)",
    startsWith(grant_id, "R324") ~ "NCSER (no topic on file)",
    TRUE                          ~ "(missing)"
  )
  coalesce(ies_program_topic, ies_program_name, center)
}

zero_grants <- master %>%
  filter(!has_any_current_policy_reach %in% c(TRUE, "TRUE", "true", 1)) %>%
  mutate(topic_area = fill_topic_area(grant_id, ies_program_topic, ies_program_name)) %>%
  select(grant_id, award_year, institution_type_detail, topic_area)
#nrow(zero_grants) #227 of 528 (528 - 301 reached)

#zero-reach DIAGNOSIS: bucket grants by why they're zero-reach. legacy
#scripts/13c writes this and the dashboard (scripts/16) reads the
#"Reached policy" bucket as the headline reach count, so the rewrite
#must produce it too. otherwise the dashboard shows a stale May 28
#snapshot instead of the current numbers
#
#~half the master rows currently have NA award_year. grant IDs like
#R305A100654 encode the year (10 -> 2010); fall back to that when the
#master year is missing so we don't bucket 246 grants into "year unknown"
year_from_grant_id <- function(gid) {
  yr2 <- suppressWarnings(as.integer(
    sub(".*[A-Z](\\d{2})\\d{4}$", "\\1", gid, perl = TRUE)))
  ifelse(is.na(yr2), NA_integer_, 2000L + yr2)
}

n_total_grants <- nrow(master)
diagnosis <- master %>%
  mutate(
    has_doi = (replace_na(as.integer(n_current_dois), 0L) > 0),
    has_pol = has_any_current_policy_reach %in% c(TRUE, "TRUE", "true", 1),
    award_year = suppressWarnings(as.integer(award_year)),
    award_year = coalesce(award_year, year_from_grant_id(grant_id)),
    n_ies_products = replace_na(as.integer(n_products_total), 0L),
    bucket = case_when(
      has_pol                                       ~ "Reached policy",
      !has_doi & n_ies_products > 0                 ~ "Zero policy: no DOI on file, but has IES outputs",
      !has_doi                                      ~ "Zero policy: no DOI on file",
      has_doi & is.na(award_year)                   ~ "Zero policy: has DOI (year unknown)",
      has_doi & award_year >= 2018                  ~ "Zero policy: has DOI but recent (>=2018)",
      has_doi & award_year < 2018                   ~ "Zero policy: has DOI and older (<2018)",
      TRUE                                          ~ "(other)"
    )
  )

zero_diag <- diagnosis %>%
  count(Bucket = bucket, name = "Grants") %>%
  mutate(`Share of all grants` = sprintf("%.1f%%",
                                         100 * Grants / n_total_grants)) %>%
  arrange(desc(Grants))

#legacy dashboard (scripts/16) expects 5 columns here: Topic area,
#Total grants, Reached, Zero reach, % reached. compute over the full
#master, not just zero_grants, so we can report a reach rate per topic
#fill_topic_area() above handles the 300 grants with NA ies_program_topic
#- they fall back to program_name or to the IES Center label
zero_by_topic <- master %>%
  mutate(topic_area = fill_topic_area(grant_id, ies_program_topic, ies_program_name),
         has_pol = has_any_current_policy_reach %in% c(TRUE, "TRUE", "true", 1)) %>%
  group_by(`Topic area` = topic_area) %>%
  summarize(
    `Total grants` = n(),
    `Reached`      = sum(has_pol),
    `Zero reach`   = sum(!has_pol),
    `% reached`    = sprintf("%.1f%%", 100 * mean(has_pol)),
    .groups = "drop"
  ) %>%
  arrange(desc(`Total grants`))

zero_by_inst <- zero_grants %>%
  left_join(master %>% select(grant_id, n_policy_docs), by = "grant_id") %>%
  group_by(`Institution type` = institution_type_detail) %>%
  summarize(
    `Total grants` = n(),
    Reached = sum((replace_na(n_policy_docs, 0L)) > 0),
    `Zero reach` = sum((replace_na(n_policy_docs, 0L)) == 0),
    `% reached` = paste0(round(100 * mean(replace_na(n_policy_docs, 0L) > 0), 1), "%"),
    .groups = "drop"
  ) %>%
  arrange(desc(`Total grants`))

#legacy dashboard schema: Cohort (4-year bucket), Total grants, Reached,
#Zero reach, % reached. cohort_bin matches scripts/13c
cohort_bin <- function(y) {
  case_when(
    is.na(y)  ~ "(unknown)",
    y <= 2007 ~ "2004-2007",
    y <= 2011 ~ "2008-2011",
    y <= 2015 ~ "2012-2015",
    y <= 2019 ~ "2016-2019",
    TRUE      ~ "2020+"
  )
}
zero_by_cohort <- master %>%
  mutate(award_year = suppressWarnings(as.integer(award_year)),
         award_year = coalesce(award_year, year_from_grant_id(grant_id)),
         has_pol = has_any_current_policy_reach %in% c(TRUE, "TRUE", "true", 1),
         cohort = cohort_bin(award_year)) %>%
  group_by(Cohort = cohort) %>%
  summarize(
    `Total grants` = n(),
    `Reached`      = sum(has_pol),
    `Zero reach`   = sum(!has_pol),
    `% reached`    = sprintf("%.1f%%", 100 * mean(has_pol)),
    .groups = "drop"
  ) %>%
  arrange(Cohort)

#-------------------------
## 5. CO-CITATION ##
#-------------------------

#which pairs of grants tend to get cited in the same policy docs? this
#is a co-citation network. for each policy doc, take all the unique
#grants that reach it, generate the (grant_a, grant_b) pairs, count them

doc_grant_pairs <- routes %>%
  distinct(grant_id, policy_document_id)

cocitation <- doc_grant_pairs %>%
  group_by(policy_document_id) %>%
  filter(n() >= 2) %>%
  summarize(grant_list = list(sort(unique(grant_id))), .groups = "drop") %>%
  mutate(pairs = map(grant_list, function(g) {
    if (length(g) < 2) return(NULL)
    combn(g, 2) %>% t() %>% as_tibble(.name_repair = ~c("g1", "g2"))
  })) %>%
  pull(pairs) %>% bind_rows() %>%
  count(g1, g2, name = "n_shared_docs") %>%
  arrange(desc(n_shared_docs))

top_cocitations <- cocitation %>% head(50)

#synthesis docs - policy docs citing 5+ distinct IES grants
synthesis_docs <- doc_grant_pairs %>%
  count(policy_document_id, name = "n_grants_cited") %>%
  filter(n_grants_cited >= 5) %>%
  left_join(routes %>% distinct(policy_document_id,
                                  policy_source_type, policy_source_country,
                                  policy_published_year),
            by = "policy_document_id") %>%
  arrange(desc(n_grants_cited))

#-------------------
## 6. WRITE OUT ##
#-------------------

out09 <- here("outputs", "09_build_timing_visuals")
out13 <- here("outputs", "13_build_cohort_growth_and_skew")
out13c <- here("outputs", "13c_build_zero_reach_topic_center")
out13d <- here("outputs", "13d_build_cocitation_and_lag_shape")
walk(c(out09, out13, out13c, out13d), ~dir.create(.x, showWarnings = FALSE, recursive = TRUE))

write_csv(l1, file.path(out09, "table_01_lag_award_to_publication.csv"))
write_csv(l2, file.path(out09, "table_02_lag_publication_to_meta.csv"))
write_csv(l4, file.path(out09, "table_04_lag_award_to_policy.csv"))
write_csv(l4b, file.path(out09, "table_05_lag_award_to_meta_policy.csv"))

write_csv(cohort_reach, file.path(out13, "table_01_cohort_reach.csv"))
write_csv(skew_distribution, file.path(out13, "table_05_grant_skew_distribution.csv"))

write_csv(zero_diag, file.path(out13c, "table_01_zero_reach_diagnosis.csv"))
write_csv(zero_by_topic, file.path(out13c, "table_02_zero_reach_by_topic.csv"))
write_csv(zero_by_inst,
          file.path(out13c, "table_03_zero_reach_by_institution_type.csv"))
write_csv(zero_by_cohort, file.path(out13c, "table_04_zero_reach_by_cohort.csv"))

write_csv(top_cocitations, file.path(out13d, "table_01_top_cocitations.csv"))
write_csv(synthesis_docs, file.path(out13d, "table_02_synthesis_docs.csv"))

#console summary
cat("L1 (award->pub):     mean=", round(mean(l1$lag_years), 1),
    " median=", median(l1$lag_years), "\n", sep = "")
cat("L2 (pub->meta):      mean=", round(mean(l2$lag_years), 1),
    " median=", median(l2$lag_years), "\n", sep = "")
cat("L4 (award->policy):  mean=", round(mean(l4$lag_years), 1),
    " median=", median(l4$lag_years), "  n=", nrow(l4), "\n", sep = "")
if (nrow(l4b) > 0) {
  cat("L4b (meta-only):     mean=", round(mean(l4b$lag_years), 1),
      " median=", median(l4b$lag_years), "  n=", nrow(l4b), "\n", sep = "")
}
cat("Top decile of grants accounts for ",
    round(100 * share_top_decile, 1), "% of all reaches\n", sep = "")

# ============================================================================ #
# PART B - overton_baseline_categories (reads the Overton dump)
# ============================================================================ #

overton_dir <- here("Overton_20260305")
rewrite_dir <- here("data", "_rewrite_outputs")
out_dir     <- here("outputs", "_overton_baseline")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# IPTC tags are hierarchical ("education>school>higher education"). we
# collapse to the top-level parent so the comparison is across the 17ish
# headline categories instead of the hundreds of sub-tags
top_level <- function(x) {
  x %>% str_split(">", simplify = TRUE) %>% .[, 1] %>% str_trim()
}


#---------------------------------------------
## 1. FULL DUMP - DISTINCT DOCS PER CATEGORY ##
#---------------------------------------------

# 66.9M rows / 5.5 GB. fread loads in ~60s on this machine
classif_raw <- fread(
  file.path(overton_dir, "df_policy_classifications.csv"),
  encoding = "UTF-8"
)

# build a (policy_document_id, top_level) pair table. distinct() dedupes
# the "doc tagged with education AND education>school" case so a doc
# only counts once per top-level category
classif_top <- classif_raw[
  , .(policy_document_id, category = top_level(classification))
] %>%
  unique()

rm(classif_raw); invisible(gc())

## denominator is the DISTINCT policy_document_id count, NOT the raw row count:
## df_policy_doc_info carries duplicate doc rows, and the dups are all uncategorised,
## so a raw-row denominator would only inflate the cascade "Other" bucket + total.
## every numerator below is already a distinct-doc count (n_distinct / unique), so the
## denominator must match - on the corrected 2026-08-01 dump this is what makes
## education read 12.2%.
total_docs_in_dump <- 21620712L    # distinct policy_document_id in the corrected 2026-08-01 dump (was 8740487L in the buggy March dump); see scripts_rewrite/99

full_dump <- classif_top %>%
  as_tibble() %>%
  group_by(category) %>%
  summarize(full_dump_docs = n_distinct(policy_document_id),
            .groups = "drop") %>%
  mutate(full_dump_pct = round(100 * full_dump_docs / total_docs_in_dump, 2)) %>%
  arrange(desc(full_dump_docs))

cat("\nFull-dump top categories (top 15):\n")
print(head(full_dump, 15))


#-----------------------------------------------
## 2. IES-CITED SUBSET - SAME BREAKDOWN ##
#-----------------------------------------------

# overton_full_classifications.csv is already filtered (by 00h) to docs
# in our IES-relevant set: first-order + second-order
ies_subset_raw <- read_csv(
  file.path(rewrite_dir, "overton_full_classifications.csv"),
  show_col_types = FALSE
)

# we need to know which IDs are first-order vs second-order so we can
# also report the split
first_order_ids <- read_csv(
  file.path(rewrite_dir, "overton_full_policy_links.csv"),
  show_col_types = FALSE
) %>% pull(policy_document_id) %>% unique()

p2p_edges <- read_csv(
  file.path(rewrite_dir, "overton_full_p2p_edges.csv"),
  show_col_types = FALSE
)
second_order_ids <- setdiff(unique(p2p_edges$policy_document_id), first_order_ids)

ies_subset <- ies_subset_raw %>%
  transmute(policy_document_id, category = top_level(classification)) %>%
  distinct()

total_ies_docs <- n_distinct(ies_subset$policy_document_id)
total_fo_docs  <- length(first_order_ids)
total_so_docs  <- length(second_order_ids)

ies_share <- ies_subset %>%
  group_by(category) %>%
  summarize(ies_cited_docs = n_distinct(policy_document_id),
            .groups = "drop") %>%
  mutate(ies_cited_pct = round(100 * ies_cited_docs / total_ies_docs, 2))


#---------------------------------------
## 3. COMPARISON TABLE ##
#---------------------------------------

# lift = (IES-cited share for category C) / (full-dump share for category C)
# lift > 1 means IES research over-represents in this category vs the
# Overton baseline; lift < 1 means under-represents

comparison <- full_dump %>%
  full_join(ies_share, by = "category") %>%
  mutate(
    full_dump_docs  = replace_na(full_dump_docs, 0L),
    full_dump_pct   = replace_na(full_dump_pct, 0),
    ies_cited_docs  = replace_na(ies_cited_docs, 0L),
    ies_cited_pct   = replace_na(ies_cited_pct, 0),
    lift            = ifelse(full_dump_pct > 0,
                              round(ies_cited_pct / full_dump_pct, 2),
                              NA_real_)
  ) %>%
  arrange(desc(ies_cited_docs))


#---------------------------------------
## 4. FIRST-ORDER vs SECOND-ORDER SPLIT ##
#---------------------------------------

by_layer <- ies_subset %>%
  mutate(layer = case_when(
    policy_document_id %in% first_order_ids  ~ "first_order",
    policy_document_id %in% second_order_ids ~ "second_order",
    TRUE                                      ~ "other"
  )) %>%
  group_by(category, layer) %>%
  summarize(docs = n_distinct(policy_document_id), .groups = "drop") %>%
  pivot_wider(names_from = layer, values_from = docs, values_fill = 0L) %>%
  mutate(first_order_pct  = round(100 * first_order  / max(total_fo_docs, 1), 2),
         second_order_pct = round(100 * second_order / max(total_so_docs, 1), 2)) %>%
  left_join(full_dump %>% select(category, full_dump_pct), by = "category") %>%
  arrange(desc(first_order))


#---------------------------------------
## 5. CASCADING DECOMPOSITION ##
#---------------------------------------

# the lift table answers "is category C over-represented?" - good for the
# narrative but each row stands alone. the cascade table below answers a
# different question: "of all IES-cited docs, where do they fall when we
# layer the dominant categories one at a time?"
#
# rows:
#   - All data
#   - Education + (docs tagged education at top level)
#   - (Non-Education) Economy + (econ docs that DON'T also have education)
#   - (Non-Educ Non-Econ) Health + (health docs that have neither
#     education nor economy)
#   - <Other> (everything else)
#
# the four content rows sum to 100% in each column. paper-friendly because
# every doc gets counted exactly once and the residual is explicit

# set of doc IDs at each category for both the full dump and the IES subset
build_cascade <- function(class_tbl, total_n) {
  edu_ids  <- unique(class_tbl[category == "education", policy_document_id])
  econ_ids <- unique(class_tbl[category == "economy, business and finance",
                                policy_document_id])
  heal_ids <- unique(class_tbl[category == "health", policy_document_id])

  edu_only         <- length(edu_ids)
  econ_after_edu   <- length(setdiff(econ_ids, edu_ids))
  heal_after_2     <- length(setdiff(heal_ids, union(edu_ids, econ_ids)))
  other            <- total_n - edu_only - econ_after_edu - heal_after_2

  c(All = total_n,
    Education = edu_only,
    `(Non-Education) Economy, Business, and Finance` = econ_after_edu,
    `(Non-Educ, Non-Economy) Health` = heal_after_2,
    `(Non-Educ, Non-Econ, Non-Health) <Other>` = other)
}

# convert both classification tables to data.table for the cascade builder
full_dt <- as.data.table(classif_top)
ies_dt  <- as.data.table(ies_subset)

# the cascade denominator is the full IES universe (first-order plus
# second-order docs, whether or not they carry a classification). compute
# it from the id sets built in section 2 so it tracks the current data
# instead of a frozen constant
ies_universe_n <- length(union(first_order_ids, second_order_ids))

cascade_full <- build_cascade(full_dt, total_docs_in_dump)
cascade_ies  <- build_cascade(ies_dt,  ies_universe_n)

cascade <- tibble(
  Category       = names(cascade_full),
  `Overton Docs` = as.integer(cascade_full),
  `Overton %`    = sprintf("%.1f%%", 100 * cascade_full / total_docs_in_dump),
  `IES Docs`     = as.integer(cascade_ies),
  `IES %`        = sprintf("%.1f%%", 100 * cascade_ies / ies_universe_n)
)


#---------------------
## 6. WRITE OUTPUTS ##
#---------------------

write_csv(comparison, file.path(out_dir, "category_overview.csv"))
write_csv(by_layer,   file.path(out_dir, "category_share_by_layer.csv"))
write_csv(cascade,    file.path(out_dir, "category_cascade.csv"))


#---------------------
## 7. CONSOLE REPORT ##
#---------------------

cat(sprintf("\nFull Overton dump: %s policy docs (cached count)\n",
            format(total_docs_in_dump, big.mark = ",")))
cat(sprintf("IES-cited subset (first + second order): %s docs\n",
            format(total_ies_docs, big.mark = ",")))
cat(sprintf("  first-order: %s\n", format(total_fo_docs, big.mark = ",")))
cat(sprintf("  second-order: %s\n\n", format(total_so_docs, big.mark = ",")))

cat("Education in context (top 10 categories by IES-cited share):\n\n")
comparison %>%
  head(10) %>%
  transmute(
    Category        = category,
    `Full dump`     = sprintf("%s (%.1f%%)",
                              format(full_dump_docs, big.mark = ","),
                              full_dump_pct),
    `IES-cited`     = sprintf("%s (%.1f%%)",
                              format(ies_cited_docs, big.mark = ","),
                              ies_cited_pct),
    Lift            = sprintf("%.2fx", lift)
  ) %>%
  print()

cat("\nWrote:\n")
cat("  outputs/_overton_baseline/category_overview.csv\n")
cat("  outputs/_overton_baseline/category_share_by_layer.csv\n")

# ============================================================================ #
# PART C - reach_sensitivity_analyses
# ============================================================================ #

out_dir <- here("outputs", "_paper_sensitivity")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

routes <- read_csv(
  here("outputs", "05_rebuild_current_policy_routes",
       "table_07_current_grant_policy_doc_routes.csv"),
  show_col_types = FALSE
)

# policy_doc_master carries the overton_policy_document_series field that
# routes itself lacks. join in only the column we need
doc_meta <- read_csv(
  here("outputs", "12_build_policy_doc_overview", "policy_doc_master.csv"),
  show_col_types = FALSE
) %>%
  select(policy_document_id, overton_policy_document_series)

n_grants_total <- 528L


#---------------------
## 1. BROAD REACH ##
#---------------------

# the headline figure. a grant is reached if any of its first-order
# routes lands in any policy doc
n_broad <- n_distinct(routes$grant_id)


#-------------------------
## 2. STRICT REACH ##
#-------------------------

# only counts grants that reach a government or IGO doc. excludes
# think-tank-only reach. measures how much of the headline depends on
# think-tank intermediaries
strict_doc_ids <- routes %>%
  filter(replace_na(policy_source_type, "") %in% c("government", "igo")) %>%
  pull(policy_document_id) %>% unique()

n_strict <- routes %>%
  filter(policy_document_id %in% strict_doc_ids) %>%
  pull(grant_id) %>% n_distinct()


#--------------------------------
## 3. EXCLUDING WORKING PAPERS ##
#--------------------------------

# overton tags ~22% of first-order docs as "Working paper" - NBER, IZA,
# IfO, etc. depending on how strictly a reviewer wants to define "policy
# document," these may or may not count. test how much the headline depends
# on them
working_paper_ids <- doc_meta %>%
  filter(overton_policy_document_series == "Working paper") %>%
  pull(policy_document_id)

n_no_wp <- routes %>%
  filter(!policy_document_id %in% working_paper_ids) %>%
  pull(grant_id) %>% n_distinct()


#------------------------------------
## 4. EXCLUDING MULTI-GRANT DOCS ##
#------------------------------------

# a policy doc that cites multiple IES grants is probably aggregating
# literature (a WWC-style review, a synthesis report) rather than engaging
# with any single study. drop those and see how reach shifts
multi_grant_doc_ids <- routes %>%
  distinct(grant_id, policy_document_id) %>%
  count(policy_document_id, name = "n_grants_cited") %>%
  filter(n_grants_cited > 1) %>%
  pull(policy_document_id)

n_single_grant_only <- routes %>%
  filter(!policy_document_id %in% multi_grant_doc_ids) %>%
  pull(grant_id) %>% n_distinct()


#--------------------------------
## 5. BOTH FILTERS COMBINED ##
#--------------------------------

n_both <- routes %>%
  filter(!policy_document_id %in% working_paper_ids,
         !policy_document_id %in% multi_grant_doc_ids) %>%
  pull(grant_id) %>% n_distinct()


#---------------------
## 6. ASSEMBLE TABLE ##
#---------------------

pct_pt <- function(new, base = n_broad) {
  sprintf("%+.1f pp", 100 * (new - base) / n_grants_total)
}
grants_minus <- function(new, base = n_broad) {
  sprintf("%+d", new - base)
}

sensitivity <- tribble(
  ~Definition,
    ~`Grants reached`, ~`% of 528`, ~`Delta from broad`, ~`Notes`,
  "Broad reach (any Overton doc)",
    n_broad, sprintf("%.1f%%", 100 * n_broad / n_grants_total), "-",
    "Headline definition.",
  "Strict reach (government / IGO only)",
    n_strict, sprintf("%.1f%%", 100 * n_strict / n_grants_total),
    sprintf("%s (%s)", grants_minus(n_strict), pct_pt(n_strict)),
    "Excludes think-tank docs; measures dependence on think-tank intermediaries.",
  "Excluding working-paper documents",
    n_no_wp, sprintf("%.1f%%", 100 * n_no_wp / n_grants_total),
    sprintf("%s (%s)", grants_minus(n_no_wp), pct_pt(n_no_wp)),
    sprintf("Drops %d of %d first-order docs (~%.0f%%).",
            length(working_paper_ids), n_distinct(routes$policy_document_id),
            100 * length(working_paper_ids) / n_distinct(routes$policy_document_id)),
  "Excluding multi-grant policy documents",
    n_single_grant_only, sprintf("%.1f%%", 100 * n_single_grant_only / n_grants_total),
    sprintf("%s (%s)", grants_minus(n_single_grant_only), pct_pt(n_single_grant_only)),
    sprintf("Drops %d of %d first-order docs (~%.0f%%); these cite 2+ IES grants.",
            length(multi_grant_doc_ids), n_distinct(routes$policy_document_id),
            100 * length(multi_grant_doc_ids) / n_distinct(routes$policy_document_id)),
  "Both filters combined",
    n_both, sprintf("%.1f%%", 100 * n_both / n_grants_total),
    sprintf("%s (%s)", grants_minus(n_both), pct_pt(n_both)),
    "No working papers AND no multi-grant docs."
)


#----------------------
## 7. WRITE + REPORT ##
#----------------------

write_csv(sensitivity, file.path(out_dir, "sensitivity_overview.csv"))

cat("Reach sensitivity analyses:\n\n")
print(sensitivity)
cat(sprintf("\nWrote: outputs/_paper_sensitivity/sensitivity_overview.csv\n"))

# ============================================================================ #
# PART D - empirical_vs_nonempirical_reach
# ============================================================================ #

pairs <- read_csv(
  here("outputs", "02_build_current_grant_doi_universe",
       "table_01_current_grant_doi_pair_union.csv"),
  show_col_types = FALSE
)

direct <- read_csv(
  here("outputs", "05_rebuild_current_policy_routes",
       "table_01_current_direct_policy_links.csv"),
  show_col_types = FALSE
)

out_dir <- here("outputs", "_paper_sensitivity")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


#-------------------------
## 1. PER-DOI OVERVIEW ##
#-------------------------

# every universe DOI either reaches a first-order policy doc or it
# doesn't. tag with empirical status and aggregate
doi_level <- pairs %>%
  distinct(doi, is_empirical) %>%
  mutate(reached_policy = doi %in% unique(direct$doi))

doi_overview <- doi_level %>%
  group_by(`Type` = if_else(is_empirical, "Empirical", "Non-empirical (meta-analysis)")) %>%
  summarize(
    `Distinct DOIs` = n(),
    `DOIs cited in >=1 policy doc` = sum(reached_policy),
    `% of DOIs reaching policy` =
      sprintf("%.1f%%", 100 * mean(reached_policy)),
    .groups = "drop"
  ) %>%
  arrange(desc(`Distinct DOIs`))


#-----------------------------
## 2. PER-DOI POLICY-DOC FAN ##
#-----------------------------

# for each DOI that reaches policy, how many docs cite it? non-empirical
# DOIs (reviews) often pull much higher per-DOI citation counts in policy
direct_with_type <- direct %>%
  distinct(doi, policy_document_id) %>%
  inner_join(pairs %>% distinct(doi, is_empirical), by = "doi")

per_doi_fan <- direct_with_type %>%
  group_by(doi, is_empirical) %>%
  summarize(n_policy_docs = n_distinct(policy_document_id), .groups = "drop")

fan_stats <- per_doi_fan %>%
  group_by(`Type` = if_else(is_empirical, "Empirical", "Non-empirical (meta-analysis)")) %>%
  summarize(
    `DOIs reaching policy` = n(),
    `Median policy docs per reached DOI` = median(n_policy_docs),
    `Mean policy docs per reached DOI` = round(mean(n_policy_docs), 1),
    `95th percentile policy docs per reached DOI` =
      round(quantile(n_policy_docs, 0.95), 0),
    `Max policy docs per reached DOI` = max(n_policy_docs),
    `Total policy-doc citations attributable to type` = sum(n_policy_docs),
    .groups = "drop"
  )


#-----------------------------
## 3. PER-GRANT BREAKDOWN ##
#-----------------------------

# does the grant have any empirical citations? any non-empirical
# citations? lets us answer "how many grants reach policy ONLY via
# their non-empirical work" - a worst-case for a strict empirical-only
# reach definition
grant_level <- pairs %>%
  distinct(grant_id, doi, is_empirical) %>%
  left_join(direct %>% distinct(doi, grant_id_in_direct = grant_id),
            by = "doi") %>%
  mutate(reached = !is.na(grant_id_in_direct)) %>%
  group_by(grant_id) %>%
  summarize(
    n_empirical_dois        = sum(is_empirical, na.rm = TRUE),
    n_nonempirical_dois     = sum(!is_empirical, na.rm = TRUE),
    n_empirical_reached     = sum(is_empirical & reached, na.rm = TRUE),
    n_nonempirical_reached  = sum(!is_empirical & reached, na.rm = TRUE),
    reach_status = case_when(
      n_empirical_reached > 0 & n_nonempirical_reached > 0 ~ "Both reach policy",
      n_empirical_reached > 0                              ~ "Only empirical reaches",
      n_nonempirical_reached > 0                           ~ "Only non-empirical reaches",
      TRUE                                                  ~ "No reach"
    ),
    .groups = "drop"
  )

grant_status_summary <- grant_level %>%
  count(`Reach status` = reach_status, name = "Grants") %>%
  mutate(`% of 528 grants` =
           sprintf("%.1f%%", 100 * Grants / 528L)) %>%
  arrange(desc(Grants))


#---------------------
## 4. WRITE OUTPUTS ##
#---------------------

write_csv(doi_overview,
          file.path(out_dir, "empirical_vs_nonempirical_doi_overview.csv"))
write_csv(fan_stats,
          file.path(out_dir, "empirical_vs_nonempirical_fan_stats.csv"))
write_csv(grant_status_summary,
          file.path(out_dir, "empirical_vs_nonempirical_grant_status.csv"))
write_csv(grant_level,
          file.path(out_dir, "empirical_vs_nonempirical_by_grant.csv"))


#-----------------------------
## 5. CONSOLE REPORT ##
#-----------------------------

cat("DOI-level overview:\n"); print(doi_overview)
cat("\nFan stats per reached DOI (policy docs per DOI):\n"); print(fan_stats)
cat("\nGrant-level reach status:\n"); print(grant_status_summary)

cat(sprintf("\nWrote 4 files to %s/\n", out_dir))
