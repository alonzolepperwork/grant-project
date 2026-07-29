# two per-grant policy-reach constructs: the DOI policy-reach profile + Section C
# overlap matrices, and the 9-path pathway lattice. both read the finished
# routes / master and classify a grant (or its DOIs) by how far and by what route
# it reaches policy.
#
#   PART A: one row per grant DOI that reaches policy DIRECTLY, tagged
#     by arena (gov / non-gov, US vs non-US gov, federal vs state-local), from
#     which the three Section C 2x2 overlap matrices are derived
#   PART B: each grant placed in exactly one of 9 terminal paths by
#     its furthest, most-direct outcome (gov > non-gov, direct > meta); emitted
#     in the draft's diagram order
#
# NOTE: the survival / time-to-event datasets and the cure / hurdle models are a
# separate downstream step, built by a coauthor from the shared master file - not
# part of this script.

#------------------
## 0. INITIALIZE ##
#------------------

library(tidyverse)
library(here)

# ============================================================================ #
# PART A - build_doi_reach_profile + Section C overlap matrices
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
# PART B - build_pathway_lattice
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
         is_gov = policy_source_type %in% c("government", "igo") & !is.na(policy_source_type))

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

# government includes IGO (gov = policy_source_type in {government, igo}), the
# convention used in the draft lattice/sankey. draft counts (from the frozen
# megafile, 480 DOI grants): path order 48/163/31/58/207/0/3/2/16. exact counts
# depend on the DOI universe (table_02), which can drift as recovery improves.
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
