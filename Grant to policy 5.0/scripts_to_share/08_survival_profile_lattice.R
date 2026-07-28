# per-grant terminal-outcome constructs: the time-to-event datasets, the DOI
# policy-reach profile + Section C overlap matrices, and the 9-path pathway
# lattice. the three constructs are independent, each classifying a grant (or
# its DOIs) by how far and by what route it reaches policy; all read the finished
# routes / master
#
#   PART A: analysis-ready survival datasets. time origin = award
#     year; events in pipeline order = first publication, first meta-analysis,
#     first non-gov policy citation, first gov policy citation. each event gets a
#     (time, status) pair for right-censoring. pre-award filter applied
#     throughout; policy events use BOTH routes
#   PART B: one row per grant DOI that reaches policy DIRECTLY, tagged
#     by arena (gov / non-gov, US vs non-US gov, federal vs state-local), from
#     which the three Section C 2x2 overlap matrices are derived
#   PART C: each grant placed in exactly one of 9 terminal paths by
#     its furthest, most-direct outcome (gov > non-gov, direct > meta); emitted
#     in the draft's diagram order

#------------------
## 0. INITIALIZE ##
#------------------

library(tidyverse)
library(here)

# ============================================================================ #
# PART A - build_survival_datasets
# ============================================================================ #
P  <- here("outputs", "05_rebuild_current_policy_routes")
clean_grant <- function(x) str_to_upper(str_trim(as.character(x)))
norm_doi <- function(x) str_trim(str_to_lower(
  str_remove(str_remove(as.character(x), "^https?://doi\\.org/"), "^doi:")))

# policy-before-publication grace (years). a policy doc dated more than this many
# years before the grant's EARLIEST publication cannot cite any of the grant's
# papers, so it cannot legitimately set the first-policy clock; see the timing
# filter in build_and_write(). 1 year lets online-first / early-view cases survive
POLICY_PUB_GRACE <- 1L

#------------------------------
## 1. GRANTS + COVARIATES ##
#------------------------------

master <- read_csv(here("outputs", "06_build_grant_policy_master",
                        "table_01_grant_policy_master.csv"), show_col_types = FALSE) %>%
  mutate(grant_id = clean_grant(grant_id))

# institutions holding >=2 IES grants (the "multi-grant institution" covariate)
inst_counts <- master %>%
  filter(!is.na(institution)) %>%
  mutate(inst_key = str_squish(str_to_upper(institution))) %>%
  count(inst_key, name = "n_grants_at_institution")

covariates <- master %>%
  mutate(inst_key = str_squish(str_to_upper(institution))) %>%
  left_join(inst_counts, by = "inst_key") %>%
  transmute(
    grant_id, award_year = as.integer(award_year),
    grant_title = title, pi,
    institution, institution_type, institution_type_detail,
    is_university = institution_type == "Uni",
    center, topic_area, grant_subject_area, program_name,
    n_grants_at_institution = replace_na(n_grants_at_institution, 0L),
    multi_grant_institution = n_grants_at_institution >= 2
  )

#------------------------------
## 2. EVENT YEARS PER GRANT ##
#------------------------------

# (a) publications: grant DOIs joined to OpenAlex publication years
pairs <- read_csv(here("outputs", "02_build_current_grant_doi_universe",
                       "table_01_current_grant_doi_pair_union.csv"), show_col_types = FALSE) %>%
  transmute(grant_id = clean_grant(grant_id), doi = norm_doi(doi))
oa <- read_csv(here("outputs", "openalex_enrichment", "doi_metadata.csv"),
               show_col_types = FALSE) %>%
  transmute(doi = norm_doi(doi), pub_year = publication_year,
            pub_date = suppressWarnings(as.Date(publication_date))) %>%
  distinct(doi, .keep_all = TRUE)

# coalesce recovered publication years/dates (recover_missing_pub_years.R) on top
# of OpenAlex: ~87 universe DOIs have no OpenAlex year, so a no-year DOI is skipped
# when first-pub is taken as the min year - reading first-pub too late and turning
# legitimate same-year policy citations into false policy-before-pub events (the
# grace filter then drops them). full_join so DOIs absent from OpenAlex enter as
# new rows. recovers 30 of the 87 from cached 02b / Crossref
bf_path <- here("outputs", "openalex_enrichment", "doi_publication_year_backfill.csv")
if (file.exists(bf_path)) {
  bf <- read_csv(bf_path, show_col_types = FALSE) %>%
    transmute(doi = norm_doi(doi), bf_year = as.integer(publication_year),
              bf_date = suppressWarnings(as.Date(publication_date)))
  oa <- oa %>% full_join(bf, by = "doi") %>%
    mutate(pub_year = coalesce(pub_year, bf_year),
           pub_date = coalesce(pub_date, bf_date)) %>%
    select(-bf_year, -bf_date)
}

pubs_all <- pairs %>% left_join(oa, by = "doi") %>%
  left_join(covariates %>% select(grant_id, award_year), by = "grant_id") %>%
  filter(!is.na(pub_year), is.na(award_year) | pub_year >= award_year)
pubs <- pubs_all %>% group_by(grant_id) %>%
  summarize(first_pub_year = min(pub_year), n_dois = n_distinct(doi), .groups = "drop")
# the grant's earliest publication: its DOI + full publication date. order by
# date (falling back to year) so "first paper" is well-defined
first_pub <- pubs_all %>%
  mutate(ord = coalesce(pub_date, as.Date(paste0(pub_year, "-12-31")))) %>%
  arrange(grant_id, ord, pub_year) %>%
  group_by(grant_id) %>% slice(1) %>%
  transmute(grant_id, first_pub_doi = doi, date_doi_published = pub_date)

# (b) meta-analyses (empirical-anchored, true meta set from stage 02)
metas <- read_csv(file.path(P, "table_03_current_grant_to_meta_links.csv"), show_col_types = FALSE) %>%
  mutate(grant_id = clean_grant(grant_id)) %>%
  filter(!is.na(meta_year)) %>%
  left_join(covariates %>% select(grant_id, award_year), by = "grant_id") %>%
  filter(is.na(award_year) | meta_year >= award_year) %>%
  group_by(grant_id) %>%
  summarize(first_meta_year = min(meta_year), n_metas = n_distinct(meta_doi), .groups = "drop")

# full policy publication dates: the route/reference tables keep only the year,
# but the raw Overton dump carries the `published_on` date - join it back by doc
# id so we can report the actual gov / non-gov publication dates
of_docs <- read_csv(here("data", "_rewrite_outputs", "overton_full_policy_docs.csv"),
                    show_col_types = FALSE)
id_col <- intersect(c("policy_document_id", "id"), names(of_docs))[1]
pol_dates <- of_docs %>%
  transmute(policy_document_id = as.character(.data[[id_col]]),
            policy_pub_date = suppressWarnings(as.Date(published_on))) %>%
  filter(!is.na(policy_pub_date)) %>% distinct(policy_document_id, .keep_all = TRUE)

# earliest non-missing date in a vector (Date-safe; NA if none)
safe_min_date <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) as.Date(NA) else min(x) }

# (c) policy citations via either route (direct + meta), split gov / non-gov
routes <- read_csv(file.path(P, "table_07_current_grant_policy_doc_routes.csv"), show_col_types = FALSE) %>%
  mutate(grant_id = clean_grant(grant_id),
         policy_document_id = as.character(policy_document_id)) %>%
  filter(!is.na(policy_published_year)) %>%
  left_join(covariates %>% select(grant_id, award_year), by = "grant_id") %>%
  filter(is.na(award_year) | policy_published_year >= award_year) %>%
  left_join(pol_dates, by = "policy_document_id") %>%
  mutate(is_gov = policy_source_type == "government" & !is.na(policy_source_type))

# multi-grant policy documents: a doc citing >=2 distinct IES grants is probably
# aggregating literature (a synthesis / WWC-style review) rather than engaging a
# single study. the Section D sensitivity dataset drops these and recomputes the
# policy events on single-grant documents only. (same definition as stage 12.)
multi_grant_doc_ids <- routes %>%
  distinct(grant_id, policy_document_id) %>%
  count(policy_document_id, name = "n_grants_cited") %>%
  filter(n_grants_cited > 1) %>% pull(policy_document_id)

# the grant's earliest gov / non-gov policy document - its id, year, and full
# date all taken from the SAME first document (ordered by date, then year, so a
# dated doc precedes a year-only doc in the same year)
first_policy <- function(df) {
  df %>%
    mutate(ord = coalesce(policy_pub_date, as.Date(paste0(policy_published_year, "-12-31")))) %>%
    arrange(grant_id, ord) %>% group_by(grant_id)
}

# build a (time, status) pair for one event: time = years from award to the
# event if observed, else years from award to the cutoff (right-censored)
add_surv <- function(df, event_year_col, cutoff, prefix) {
  ey <- df[[event_year_col]]
  reached <- !is.na(ey)
  yrs <- ey - df$award_year
  cens <- cutoff - df$award_year
  df[[paste0(prefix, "_yrs")]]    <- ifelse(reached, yrs, NA_real_)   # time-to-event (reached only)
  df[[paste0(prefix, "_status")]] <- as.integer(reached)              # 1 = event, 0 = censored
  df[[paste0(prefix, "_time")]]   <- ifelse(reached, yrs, cens)       # Surv() time
  df
}

#------------------------------
## 3. ASSEMBLE + CENSOR + WRITE ##
#------------------------------

out_dir <- here("outputs", "_time_series")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# cutoffs = last observed year for each event family. policy_cutoff is taken
# over ALL policy docs (the data-collection limit, ~2023) and held fixed across
# the main and sensitivity datasets so the censoring horizon is identical -
# excluding multi-grant docs changes which grants reach policy, not when the
# data ends
pub_cutoff    <- max(pubs$first_pub_year, na.rm = TRUE)
meta_cutoff   <- max(metas$first_meta_year, na.rm = TRUE)
policy_cutoff <- max(routes$policy_published_year, na.rm = TRUE)
cat("Censoring cutoffs - pub:", pub_cutoff, " meta:", meta_cutoff,
    " policy:", policy_cutoff, "\n")

# given a (possibly filtered) routes table, recompute the per-grant policy
# events, assemble the full grant table with (time, status) pairs, and write the
# three dataset files under `suffix`. publications + meta-analyses are
# unaffected by the policy-doc filter, so they are computed once (above) and
# reused for both the main and the sensitivity builds
build_and_write <- function(routes_use, suffix, label) {
  # gov_route / nongov_route record HOW the grant's FIRST gov / non-gov policy
  # document referenced it: "direct" = the doc cited the grant's (empirical)
  # publication DOI; "meta" = the doc cited a meta-analysis of the grant; "both"
  # = the same doc did both. policy events use either route, so this flag lets
  # the analyst tell which mechanism set the clock for each grant
  # POLICY-BEFORE-PUBLICATION grace filter (TIMING events only). a policy doc
  # dated > POLICY_PUB_GRACE years before the grant's earliest publication cannot
  # cite any of the grant's papers (first_pub_year is the minimum) - it's a
  # structural error (off-topic mis-link, bad Overton date, or working-paper-
  # version year mismatch) and must not set the grant's first-policy clock. the
  # 1-year grace keeps online-first / early-view citations; the rule is grant-level
  # so it catches BOTH routes (incl. meta-route anomalies the direct audit misses)
  # n_policy_docs below uses the UNfiltered routes_use, so reach counts are intact
  routes_timing <- routes_use %>%
    left_join(pubs %>% select(grant_id, first_pub_year), by = "grant_id") %>%
    filter(is.na(first_pub_year) |
           policy_published_year >= first_pub_year - POLICY_PUB_GRACE) %>%
    select(-first_pub_year)

  gov <- first_policy(routes_timing %>% filter(is_gov)) %>%
    summarize(first_gov_year = first(policy_published_year),
              date_gov_published = first(policy_pub_date),
              gov_policy_id = first(policy_document_id),
              gov_route = first(case_when(has_direct_path & has_meta_path ~ "both",
                                          has_direct_path ~ "direct",
                                          TRUE ~ "meta")),
              n_gov_docs = n_distinct(policy_document_id), .groups = "drop")
  nongov <- first_policy(routes_timing %>% filter(!is_gov)) %>%
    summarize(first_nongov_year = first(policy_published_year),
              date_nongov_published = first(policy_pub_date),
              nongov_policy_id = first(policy_document_id),
              nongov_route = first(case_when(has_direct_path & has_meta_path ~ "both",
                                             has_direct_path ~ "direct",
                                             TRUE ~ "meta")),
              n_nongov_docs = n_distinct(policy_document_id), .groups = "drop")
  n_policy <- routes_use %>% group_by(grant_id) %>%
    summarize(n_policy_docs = n_distinct(policy_document_id), .groups = "drop")

  grant <- covariates %>%
    left_join(pubs,     by = "grant_id") %>%
    left_join(first_pub, by = "grant_id") %>%
    left_join(metas,    by = "grant_id") %>%
    left_join(gov,      by = "grant_id") %>%
    left_join(nongov,   by = "grant_id") %>%
    left_join(n_policy, by = "grant_id") %>%
    mutate(across(c(n_dois, n_metas, n_gov_docs, n_nongov_docs, n_policy_docs),
                  ~replace_na(., 0L))) %>%
    add_surv("first_pub_year",    pub_cutoff,    "pub") %>%
    add_surv("first_meta_year",   meta_cutoff,   "meta") %>%
    add_surv("first_nongov_year", policy_cutoff, "nongov") %>%
    add_surv("first_gov_year",    policy_cutoff, "gov")

  write_csv(grant, file.path(out_dir, paste0("grant_survival_dataset", suffix, ".csv")))

  # PRE-SURVIVAL INPUT dataset: only the raw variables needed to FIT a survival
  # model - the award-year origin, the four event years, the grouping
  # covariates, and the counts - WITHOUT any (time, status) pair baked in. this
  # lets the analyst pick the censoring cutoff and construct Surv() themselves:
  #   gov_status = as.integer(!is.na(first_gov_year))
  #   gov_time   = ifelse(!is.na(first_gov_year), first_gov_year - award_year,
  #                       CUTOFF_YEAR - award_year)   # CUTOFF e.g. 2023 for policy
  input_vars <- grant %>%
    select(grant_id, grant_title, pi, award_year,
           institution, institution_type, institution_type_detail, is_university,
           center, topic_area, grant_subject_area, program_name,
           n_grants_at_institution, multi_grant_institution,
           first_pub_year, date_doi_published,
           first_meta_year,
           first_nongov_year, date_nongov_published,
           first_gov_year, date_gov_published,
           n_dois, n_metas, n_nongov_docs, n_gov_docs, n_policy_docs)
  write_csv(input_vars, file.path(out_dir, paste0("grant_survival_input", suffix, ".csv")))

  # tidy long format: one row per grant per event, for survfit(Surv ~ group)
  # the milestone YEARS (first pub / meta / gov / non-gov) are carried on every
  # row as reference columns, alongside the gov_policy_id / nongov_policy_id
  # Overton ids of the grant's first gov / non-gov policy documents - NA only
  # when the grant never reached that milestone
  long <- bind_rows(
    grant %>% transmute(grant_id, event = "publication",   time = pub_time,    status = pub_status),
    grant %>% transmute(grant_id, event = "meta_analysis", time = meta_time,   status = meta_status),
    grant %>% transmute(grant_id, event = "nongov_policy", time = nongov_time, status = nongov_status),
    grant %>% transmute(grant_id, event = "gov_policy",    time = gov_time,    status = gov_status)
  ) %>%
    left_join(grant %>% select(grant_id, grant_title, pi, award_year, is_university,
                               center, topic_area, grant_subject_area, multi_grant_institution,
                               n_dois, n_metas, n_policy_docs,
                               doi = first_pub_doi, first_pub_year, first_meta_year,
                               first_gov_year, gov_policy_id, gov_route,
                               first_nongov_year, nongov_policy_id, nongov_route),
              by = "grant_id") %>%
    filter(!is.na(time), time >= 0) %>%
    relocate(grant_id, grant_title, pi, award_year, event, time, status,
             doi, first_pub_year, first_meta_year,
             first_gov_year, gov_policy_id, gov_route,
             first_nongov_year, nongov_policy_id, nongov_route)
  write_csv(long, file.path(out_dir, paste0("grant_survival_long", suffix, ".csv")))

  cat("\n[", label, "] ", nrow(grant), " grants\n", sep = "")
  ev <- function(s) sprintf("%d events / %d censored", sum(grant[[s]] == 1), sum(grant[[s]] == 0))
  cat("  publication:   ", ev("pub_status"), "\n")
  cat("  meta-analysis: ", ev("meta_status"), "\n")
  cat("  non-gov policy:", ev("nongov_status"), "\n")
  cat("  gov policy:    ", ev("gov_status"), "\n")
  cat("  any policy reach (n_policy_docs>0):", sum(grant$n_policy_docs > 0), "grants\n")
  invisible(grant)
}

# main dataset (all policy docs) + Section D sensitivity (single-grant docs only)
build_and_write(routes, "", "main - all policy docs")
build_and_write(routes %>% filter(!policy_document_id %in% multi_grant_doc_ids),
                "_excl_multigrant",
                "Section D sensitivity - multi-grant docs excluded")

cat("\nMulti-grant policy docs excluded in the sensitivity build:",
    length(multi_grant_doc_ids), "of", n_distinct(routes$policy_document_id), "\n")
cat("Wrote main + _excl_multigrant variants of grant_survival_{dataset,input,long}.csv\n")

#------------------------------------------
## 6. PUBLICATION x POLICY LONG DATASET ##
#------------------------------------------

# a more granular long file (separate from the milestone long above): one row
# per (grant, publication DOI, associated policy document). each publication
# carries its own publication date + year alongside the date + year of each
# policy document that cites it DIRECTLY. a publication that reaches no (valid)
# policy appears once with the policy columns blank. direct route only - the
# meta route links policy to a meta-analysis, not to the publication itself, so
# there is no single publication date to pair with it here
#
# pre-award filter applied here too (matching the milestone datasets above): a
# publication or policy citation dated before the grant's award year is a
# structural impossibility (Overton/OpenAlex date error), so pre-award
# publications are dropped entirely and pre-award policy links are dropped. rows
# with an unknown (blank) year are kept, since they cannot be placed before the
# award

direct <- read_csv(file.path(P, "table_01_current_direct_policy_links.csv"),
                   show_col_types = FALSE) %>%
  transmute(grant_id = clean_grant(grant_id),
            doi = norm_doi(doi),
            policy_document_id = as.character(policy_document_id),
            policy_source_type,
            policy_year = policy_published_year) %>%
  left_join(pol_dates, by = "policy_document_id") %>%
  rename(policy_date = policy_pub_date) %>%
  # drop policy links dated before the grant's award year
  left_join(covariates %>% select(grant_id, award_year), by = "grant_id") %>%
  filter(is.na(award_year) | is.na(policy_year) | policy_year >= award_year) %>%
  select(-award_year)

pub_policy_long <- pairs %>%
  distinct(grant_id, doi) %>%
  left_join(oa, by = "doi") %>%                                   # pub_year, pub_date
  left_join(covariates %>% select(grant_id, award_year), by = "grant_id") %>%
  # drop publications dated before the grant's award year
  filter(is.na(award_year) | is.na(pub_year) | pub_year >= award_year) %>%
  left_join(direct, by = c("grant_id", "doi")) %>%               # one row per citing policy doc
  transmute(grant_id, award_year,
            doi, pub_year, pub_date,
            policy_document_id, policy_source_type, policy_year, policy_date) %>%
  arrange(grant_id, doi, policy_year, policy_document_id)

write_csv(pub_policy_long, file.path(out_dir, "grant_survival_publication_policy_long.csv"))

cat("\nPublication x policy long (pre-award filtered):", nrow(pub_policy_long), "rows |",
    n_distinct(pub_policy_long$doi), "distinct publications |",
    sum(!is.na(pub_policy_long$policy_document_id)), "pub-policy links |",
    sum(is.na(pub_policy_long$policy_document_id)), "publications with no policy\n")

# ============================================================================ #
# PART B - build_doi_reach_profile + Section C overlap matrices
# ============================================================================ #

out <- here("outputs", "22_build_doi_reach_profile")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

#-----------------------------
## 1. LOAD LINKS + ARENAS ##
#-----------------------------

dl <- read_csv(
  here("outputs", "05_rebuild_current_policy_routes",
       "table_01_current_direct_policy_links.csv"),
  show_col_types = FALSE
) %>%
  mutate(policy_document_id = as.character(policy_document_id))

master <- read_csv(
  here("outputs", "12b_build_policy_doc_geography",
       "policy_doc_master_with_geo.csv"),
  show_col_types = FALSE
) %>%
  transmute(policy_document_id = as.character(policy_document_id),
            canonical_country, us_level)

#per direct citation link: source type + geography + US level
links <- dl %>%
  left_join(master, by = "policy_document_id") %>%
  mutate(
    src      = replace_na(policy_source_type, "other"),
    country  = canonical_country,
    is_us    = !is.na(country) & country == "United States",
    #source-type families
    f_gov    = src == "government",
    f_igo    = src == "igo",
    f_tt     = src == "think tank",
    f_other  = src %in% c("other"),
    #government, split US vs non-US (national governments only; IGO is separate)
    f_us_gov    = f_gov & is_us,
    f_nonus_gov = f_gov & !is_us & !is.na(country),
    #within US government: federal vs state/local
    f_fed   = f_us_gov & us_level == "Federal government",
    f_state = f_us_gov & us_level == "State/Local government"
  )

#-----------------------------
## 2. DOI-LEVEL PROFILE ##
#-----------------------------

#grant linkage for the "unique grants" counts (a DOI can map to >1 grant)
doi_grant <- dl %>% distinct(grant_id, doi)

profile <- links %>%
  group_by(doi) %>%
  summarise(
    n_grants       = n_distinct(grant_id),
    grant_ids      = paste(sort(unique(grant_id)), collapse = "; "),
    n_policy_docs  = n_distinct(policy_document_id),
    n_gov_docs     = n_distinct(policy_document_id[f_gov]),
    n_igo_docs     = n_distinct(policy_document_id[f_igo]),
    n_tt_docs      = n_distinct(policy_document_id[f_tt]),
    n_other_docs   = n_distinct(policy_document_id[f_other]),
    n_us_gov_docs  = n_distinct(policy_document_id[f_us_gov]),
    n_nonus_gov_docs = n_distinct(policy_document_id[f_nonus_gov]),
    n_fed_docs     = n_distinct(policy_document_id[f_fed]),
    n_state_docs   = n_distinct(policy_document_id[f_state]),
    #presence flags (cited by >=1 doc of the arena)
    cited_by_gov        = any(f_gov),
    cited_by_igo        = any(f_igo),
    cited_by_thinktank  = any(f_tt),
    cited_by_other      = any(f_other),
    cited_by_us_gov     = any(f_us_gov),
    cited_by_nonus_gov  = any(f_nonus_gov),
    cited_by_us_federal = any(f_fed),
    cited_by_us_state   = any(f_state),
    .groups = "drop"
  ) %>%
  mutate(
    #authoritative (government incl. IGO) vs civil-society (think tank/other)
    cited_by_govlike = cited_by_gov | cited_by_igo,
    cited_by_nongov  = cited_by_thinktank | cited_by_other,
    #cell labels for each Section C matrix
    cell_gov_vs_nongov = case_when(
      cited_by_govlike & cited_by_nongov  ~ "Both",
      cited_by_govlike & !cited_by_nongov ~ "Government/IGO only",
      !cited_by_govlike & cited_by_nongov ~ "Non-government only",
      TRUE ~ NA_character_
    ),
    cell_usgov_vs_nonusgov = case_when(
      cited_by_us_gov & cited_by_nonus_gov  ~ "Both",
      cited_by_us_gov & !cited_by_nonus_gov ~ "US government only",
      !cited_by_us_gov & cited_by_nonus_gov ~ "Non-US government only",
      TRUE ~ NA_character_
    ),
    cell_federal_vs_state = case_when(
      cited_by_us_federal & cited_by_us_state  ~ "Both",
      cited_by_us_federal & !cited_by_us_state ~ "Federal only",
      !cited_by_us_federal & cited_by_us_state ~ "State/Local only",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(desc(n_policy_docs))

write_csv(profile, file.path(out, "doi_policy_reach_profile.csv"))

#-----------------------------
## 3. OVERLAP MATRICES ##
#-----------------------------

#count DOIs and unique grants for a set of DOIs
grant_n <- function(dois) doi_grant %>% filter(doi %in% dois) %>%
  distinct(grant_id) %>% nrow()

#build a tidy long matrix from a cell column (restricted to a doi subset)
build_matrix <- function(df, cell_col, levels) {
  df %>% filter(!is.na(.data[[cell_col]])) %>%
    group_by(cell = .data[[cell_col]]) %>%
    summarise(dois = n(), .groups = "drop") %>%
    rowwise() %>%
    mutate(unique_grants = grant_n(
      df %>% filter(.data[[cell_col]] == cell) %>% pull(doi))) %>%
    ungroup() %>%
    mutate(cell = factor(cell, levels = levels)) %>%
    arrange(cell)
}

#matrix 3: government/IGO vs non-government (all reached DOIs)
m_gov <- build_matrix(profile, "cell_gov_vs_nongov",
  c("Government/IGO only", "Both", "Non-government only"))

#matrix 1: US gov vs non-US gov (DOIs cited by >=1 national government doc)
gov_dois <- profile %>% filter(cited_by_us_gov | cited_by_nonus_gov)
m_usgov <- build_matrix(gov_dois, "cell_usgov_vs_nonusgov",
  c("US government only", "Both", "Non-US government only"))

#matrix 2: federal vs state/local (DOIs cited by >=1 US government doc)
usgov_dois <- profile %>% filter(cited_by_us_federal | cited_by_us_state)
m_fed <- build_matrix(usgov_dois, "cell_federal_vs_state",
  c("Federal only", "Both", "State/Local only"))

write_csv(m_gov,   file.path(out, "overlap_matrix_gov_vs_nongov.csv"))
write_csv(m_usgov, file.path(out, "overlap_matrix_usgov_vs_nonusgov.csv"))
write_csv(m_fed,   file.path(out, "overlap_matrix_federal_vs_statelocal.csv"))

#-----------------------------
## 4. CONSOLE SUMMARY ##
#-----------------------------

cat("DOI policy-reach profile built.\n")
cat("  reached DOIs (>=1 direct policy citation):", nrow(profile), "\n")
cat("  unique grants:", n_distinct(doi_grant$grant_id), "\n\n")

cat("Matrix 3 - Government/IGO vs Non-government (all reached DOIs):\n")
print(m_gov)
cat("\nMatrix 1 - US vs Non-US government (DOIs cited by a national govt):\n")
print(m_usgov)
cat("\nMatrix 2 - Federal vs State/Local (DOIs cited by a US govt doc):\n")
print(m_fed)

# ============================================================================ #
# PART C - build_pathway_lattice
# ============================================================================ #

P <- here("outputs", "05_rebuild_current_policy_routes")
clean_grant <- function(x) str_to_upper(str_trim(as.character(x)))

#------------------
## 1. LOAD INPUTS ##
#------------------

# the 528-grant universe + has_doi flag (480 with a DOI, 48 without -> path 1)
universe <- read_csv(here("outputs", "02_build_current_grant_doi_universe",
                          "table_02_current_grant_universe.csv"), show_col_types = FALSE) %>%
  mutate(grant_id = clean_grant(grant_id))

# grant-DOI pairs carry is_empirical; the DOI column counts empirical DOIs only
pairs <- read_csv(here("outputs", "02_build_current_grant_doi_universe",
                       "table_01_current_grant_doi_pair_union.csv"), show_col_types = FALSE) %>%
  mutate(grant_id = clean_grant(grant_id))

# grant -> meta-analyses (empirical-anchored true-meta set)
metas <- read_csv(file.path(P, "table_03_current_grant_to_meta_links.csv"), show_col_types = FALSE) %>%
  mutate(grant_id = clean_grant(grant_id))

# grant -> policy docs, typed (gov/non-gov) and routed (has_direct_path / has_meta_path)
routes <- read_csv(file.path(P, "table_07_current_grant_policy_doc_routes.csv"), show_col_types = FALSE) %>%
  mutate(grant_id = clean_grant(grant_id),
         policy_document_id = as.character(policy_document_id),
         is_gov = policy_source_type == "government" & !is.na(policy_source_type))

#----------------------------------
## 2. PER-GRANT REACH INDICATORS ##
#----------------------------------

# does the grant reach gov / non-gov, and via which route (direct vs meta)?
reach <- routes %>%
  group_by(grant_id) %>%
  summarise(d_gov    = any(has_direct_path & is_gov),
            d_nongov = any(has_direct_path & !is_gov),
            m_gov    = any(has_meta_path & is_gov),
            m_nongov = any(has_meta_path & !is_gov),
            .groups = "drop")

# has the grant any meta-analysis at all (even one that reaches no policy -> path 9)
meta_grant <- metas %>% distinct(grant_id) %>% mutate(has_meta = TRUE)

grants <- universe %>%
  transmute(grant_id, has_doi) %>%
  left_join(reach, by = "grant_id") %>%
  left_join(meta_grant, by = "grant_id") %>%
  mutate(across(c(d_gov, d_nongov, m_gov, m_nongov, has_meta), ~replace_na(., FALSE)))

#--------------------------------------
## 3. CLASSIFY INTO THE 9 PATHS ##
#--------------------------------------

# priority: furthest stage wins (gov > non-gov > none), then direct > meta
# case_when is evaluated top-down, so the gov branches (checked first) win over
# the non-gov ones, and within gov the direct cases (d_gov) win over meta (m_gov)
grants <- grants %>%
  mutate(path = case_when(
    !has_doi          ~ 1L,                      # grant -> (no DOI)
    d_gov &  d_nongov ~ 5L,                      # DOI -> non-gov -> gov (direct)
    d_gov & !d_nongov ~ 4L,                      # DOI -> gov (direct)
    m_gov &  m_nongov ~ 8L,                      # DOI -> meta -> non-gov -> gov
    m_gov & !m_nongov ~ 7L,                      # DOI -> meta -> gov
    d_nongov          ~ 3L,                      # DOI -> non-gov only (direct)
    m_nongov          ~ 6L,                      # DOI -> meta -> non-gov only
    has_meta          ~ 9L,                      # DOI -> meta -> (no policy)
    TRUE              ~ 2L                        # DOI -> (no policy, no meta)
  ))

# sanity: this should reproduce the scaffold lattice exactly (48/162/41/43/210/0/1/7/16)
stopifnot(nrow(grants) == 528, sum(is.na(grants$path)) == 0)

#--------------------------------------
## 4. ON-PATH STAGE COUNTS ##
#--------------------------------------

grant_path <- grants %>% select(grant_id, path)

# empirical DOIs of each path's grants (distinct)
doi_by_path <- pairs %>% filter(is_empirical) %>%
  distinct(grant_id, doi) %>%
  inner_join(grant_path, by = "grant_id") %>%
  group_by(path) %>% summarise(n_emp_dois = n_distinct(doi), .groups = "drop")

# meta-analyses citing each path's grants (distinct)
meta_by_path <- metas %>% distinct(grant_id, meta_doi) %>%
  inner_join(grant_path, by = "grant_id") %>%
  group_by(path) %>% summarise(n_metas = n_distinct(meta_doi), .groups = "drop")

# policy docs by route x type, per path - compute all four, pick the on-path one below
docs_by_path <- routes %>%
  inner_join(grant_path, by = "grant_id") %>%
  group_by(path) %>%
  summarise(direct_nongov = n_distinct(policy_document_id[has_direct_path & !is_gov]),
            direct_gov    = n_distinct(policy_document_id[has_direct_path &  is_gov]),
            meta_nongov   = n_distinct(policy_document_id[has_meta_path   & !is_gov]),
            meta_gov      = n_distinct(policy_document_id[has_meta_path   &  is_gov]),
            .groups = "drop")

#--------------------------------------
## 5. ASSEMBLE THE 9-ROW TABLE ##
#--------------------------------------

# grants are classified above by the script's own `path` integer (1-9). the
# table, however, is emitted in the DRAFT DIAGRAM ORDER (rows 2-5 = the direct
# route, rows 6-9 = the meta route). `path` is the internal classification key;
# `diagram` is the row number shown, and the cell logic below stays keyed on
# `path` so it doesn't have to change
labels <- tibble::tribble(
  ~path, ~diagram, ~Path,
  1L, 1L, "1. Grant -> (falls off)",
  3L, 2L, "2. DOI -> Non-Gov -> (falls off)",
  5L, 3L, "3. DOI -> Non-Gov -> Gov",
  4L, 4L, "4. DOI -> Gov (no non-gov)",
  2L, 5L, "5. DOI -> (falls off)",
  6L, 6L, "6. DOI -> Meta -> Non-Gov -> (falls off)",
  8L, 7L, "7. DOI -> Meta -> Non-Gov -> Gov",
  7L, 8L, "8. DOI -> Meta -> Gov (no non-gov)",
  9L, 9L, "9. DOI -> Meta -> (falls off)"
)

lattice <- labels %>%
  left_join(count(grants, path, name = "Grants"), by = "path") %>%
  left_join(doi_by_path, by = "path") %>%
  left_join(meta_by_path, by = "path") %>%
  left_join(docs_by_path, by = "path") %>%
  mutate(Grants = replace_na(Grants, 0L)) %>%
  # fill only the on-path stage cells (keyed on the script `path`); leave the
  # rest blank like the draft template, then order/keep the diagram columns
  mutate(
    DOI       = if_else(path >= 2, replace_na(n_emp_dois, 0L), NA_integer_),
    Meta      = if_else(path %in% c(6L, 7L, 8L, 9L), replace_na(n_metas, 0L), NA_integer_),
    `Non-Gov` = case_when(path %in% c(3L, 5L) ~ replace_na(direct_nongov, 0L),
                          path %in% c(6L, 8L) ~ replace_na(meta_nongov, 0L),
                          TRUE ~ NA_integer_),
    Gov       = case_when(path %in% c(4L, 5L) ~ replace_na(direct_gov, 0L),
                          path %in% c(7L, 8L) ~ replace_na(meta_gov, 0L),
                          TRUE ~ NA_integer_)
  ) %>%
  arrange(diagram) %>%
  select(Path, Grants, DOI, Meta, `Non-Gov`, Gov)

# total row (grants only - the stage columns are funnel counts, not summable)
lattice_out <- bind_rows(
  lattice,
  tibble(Path = "Total", Grants = sum(lattice$Grants),
         DOI = NA_integer_, Meta = NA_integer_, `Non-Gov` = NA_integer_, Gov = NA_integer_)
)

out_dir <- here("outputs", "_pathway_lattice")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(lattice_out, file.path(out_dir, "pathway_lattice.csv"))

#--------------------------------------
## 6. CONSOLE SUMMARY ##
#--------------------------------------

cat("Grant-to-policy pathway lattice (Results III.A, Step 1):\n\n")
print(lattice_out, n = 10)
cat("\nGrant counts (diagram order):", paste(lattice$Grants, collapse = "/"),
    "(sum", sum(lattice$Grants), ")\n")
cat("Government-reaching grants: direct =", sum(grants$path %in% c(4L, 5L)),
    "| meta-only =", sum(grants$path %in% c(7L, 8L)),
    "| total =", sum(grants$path %in% c(4L, 5L, 7L, 8L)), "\n")
