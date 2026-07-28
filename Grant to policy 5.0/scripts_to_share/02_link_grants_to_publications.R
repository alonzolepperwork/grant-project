#builds the link tables from grant publications to policy documents
#two pathways:
#  direct:   grant -> grant's publication DOI -> policy document that cites it
#  meta:     grant -> grant's publication DOI -> meta-analysis that cites it
#            -> policy document that cites the meta-analysis
#
#the direct path comes from Overton's submission-based search. the meta
#path comes from OpenAlex (review-type works that cite our grant pubs)
#then re-joined to Overton for the policy citations

#------------------
## 0. INITIALIZE ##
#------------------

#load libraries
library(tidyverse)
library(here)

# rewrite-vs-legacy path helper. all writes go to data/_rewrite_outputs/
# so the original data/02_legacy_existing_outputs/ stays untouched as a
# read-only fallback. reads prefer the rewrite output when it exists
rewrite_dir <- here("data", "_rewrite_outputs")
legacy_dir  <- here("data", "02_legacy_existing_outputs")
dir.create(rewrite_dir, showWarnings = FALSE, recursive = TRUE)
pick_input <- function(filename) {
  p <- file.path(rewrite_dir, filename)
  if (file.exists(p)) p else file.path(legacy_dir, filename)
}

#tiny helpers (same as in 01_build_grant_universe.R)
clean_grant <- function(x) {
  x %>% as.character() %>% str_trim() %>% str_to_upper() %>%
    na_if("") %>% na_if("NA")
}

norm_doi <- function(x) {
  x %>% as.character() %>% str_to_lower() %>%
    str_remove("^https?://doi\\.org/") %>%
    str_remove("^doi:") %>% str_trim() %>%
    na_if("") %>% na_if("na")
}

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[1]
}

#col_or_na returns the column if present, else a same-length NA vector
#defends against snapshots of data files where optional columns are absent
col_or_na <- function(df, col) {
  if (is.na(col) || !(col %in% names(df))) rep(NA, nrow(df)) else df[[col]]
}

first_nonmissing <- function(x) {
  hit <- x[!is.na(x) & x != ""]
  if (length(hit) == 0) NA else hit[1]
}

#load the grant universe from stage 01
pairs <- read_csv(
  here("outputs", "02_build_current_grant_doi_universe",
       "table_01_current_grant_doi_pair_union.csv"),
  show_col_types = FALSE
) %>%
  transmute(grant_id = clean_grant(grant_id), doi = norm_doi(doi),
            is_meta_analysis = is_meta_analysis,
            is_empirical     = is_empirical) %>%
  filter(!is.na(grant_id), !is.na(doi)) %>%
  distinct()

#grant award years (committed snapshot from the master - award_year is not
#available this early in the pipeline). used to drop policy citations dated
#BEFORE the grant's award year (a chronological impossibility): a policy doc
#cannot cite a grant's paper before the grant exists. applied to both the
#direct and meta routes so every downstream count inherits the filter
award_lookup <- read_csv(
  here("data", "_rewrite_outputs", "grant_award_years.csv"),
  show_col_types = FALSE
) %>%
  transmute(grant_id = clean_grant(grant_id), award_year = as.integer(award_year))

drop_pre_award <- function(df) {
  df %>%
    left_join(award_lookup, by = "grant_id") %>%
    filter(is.na(award_year) | is.na(policy_published_year) |
           policy_published_year >= award_year) %>%
    select(-award_year)
}

#------------------------------------
## 1. POLICY DOC LINKAGE REFERENCE ##
#------------------------------------

#we union four sources of policy-doc-to-doi links to maximize coverage:
# (a) the Overton 20260305 full database dump (full DB, every policy doc)
# (b) the older Overton-style submission export (doi_policydoc_linkage + policydoc_info)
# (c) the legacy IES_policy_citations / IES_policy_doc_info pair
# (d) the legacy meta_policy_citations / meta_policy_doc_info pair
#full-dump (a) is preferred for metadata since it's both newest and broadest;
#the submission export (b) sometimes has cleaner titles for older docs;
#legacy (c)/(d) only fill gaps

#Overton 20260305 full database dump - the broadest source. produced by
#00f_extract_overton_full_dump.R, which filters the ~16M-row full dump
#down to just the policy docs that cite a DOI in our grant universe
#unlike the submission-based export below, it picks up every policy doc
#in Overton that mentions one of our DOIs, not just the matches Overton
#returned for our specific search
links_overton_full <- read_csv(
  pick_input("overton_full_policy_links.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    policy_document_id = col_or_na(., pick_col(., c("policy_document_id", "id"))),
    doi = norm_doi(col_or_na(., pick_col(., c("doi", "DOI")))),
    src_overton_full = TRUE
  ) %>% filter(!is.na(policy_document_id), !is.na(doi))

#older Overton submission-based export - still useful as a second
#source because some doc titles/metadata came through cleaner there
links_new <- read_csv(
  here("data", "04_new_policy_inputs", "doi_policydoc_linkage.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    policy_document_id = col_or_na(., pick_col(., c("policy_document_id", "id"))),
    doi = norm_doi(col_or_na(., pick_col(., c("doi", "DOI")))),
    src_new = TRUE
  ) %>% filter(!is.na(policy_document_id), !is.na(doi))

#metadata for matched docs from the full Overton dump (same schema as
#docs_new below - they're both Overton exports)
docs_overton_full <- read_csv(
  pick_input("overton_full_policy_docs.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    policy_document_id = col_or_na(., pick_col(., c("policy_document_id", "id"))),
    policy_title = col_or_na(., pick_col(., c("policy_title", "title"))),
    policy_source_title = col_or_na(., pick_col(., c("policy_source_title", "source_title"))),
    policy_source_id = col_or_na(., pick_col(., c("policy_source_id", "source_id"))),
    policy_source_type = col_or_na(., pick_col(., c("policy_source_type", "source_type"))),
    policy_source_country = col_or_na(., pick_col(., c("policy_source_country", "country"))),
    policy_published_date = col_or_na(., pick_col(., c("policy_published_date", "published_on", "published"))),
    policy_published_year = as.integer(str_extract(as.character(policy_published_date), "^(19|20)[0-9]{2}")),
    overton_policy_document_series = col_or_na(., pick_col(., c("overton_policy_document_series", "series"))),
    policy_document_url = col_or_na(., pick_col(., c("policy_document_url", "url"))),
    language = col_or_na(., pick_col(., c("language", "lang")))
  ) %>%
  distinct()

docs_new <- read_csv(
  here("data", "04_new_policy_inputs", "policydoc_info.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    policy_document_id = col_or_na(., pick_col(., c("policy_document_id", "id"))),
    policy_title = col_or_na(., pick_col(., c("policy_title", "title"))),
    policy_source_title = col_or_na(., pick_col(., c("policy_source_title", "source_title"))),
    policy_source_id = col_or_na(., pick_col(., c("policy_source_id", "source_id"))),
    policy_source_type = col_or_na(., pick_col(., c("policy_source_type", "source_type"))),
    policy_source_country = col_or_na(., pick_col(., c("policy_source_country", "country"))),
    policy_published_date = col_or_na(., pick_col(., c("policy_published_date", "published_on", "published"))),
    #year not stored in the source - extract from date string
    policy_published_year = as.integer(str_extract(as.character(policy_published_date), "^(19|20)[0-9]{2}")),
    overton_policy_document_series = col_or_na(., pick_col(., c("overton_policy_document_series", "series"))),
    policy_document_url = col_or_na(., pick_col(., c("policy_document_url", "url"))),
    language = col_or_na(., pick_col(., c("language", "lang")))
  ) %>%
  distinct()

#legacy IES direct citations (older Overton snapshot). the DOI column in
#these files is named `dois_cited` (not doi) - without that alias every
#row silently drops at the !is.na(doi) filter and the legacy sources
#contribute zero rows to the union
links_old_direct <- read_csv(
  here("data", "01_legacy_original_inputs", "IES_policy_citations.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    policy_document_id = col_or_na(., pick_col(., c("policy_document_id", "id"))),
    doi = norm_doi(col_or_na(., pick_col(., c("dois_cited", "doi", "DOI")))),
    src_old_direct = TRUE
  ) %>% filter(!is.na(policy_document_id), !is.na(doi))

#legacy meta citations (older Overton snapshot)
links_old_meta <- read_csv(
  here("data", "01_legacy_original_inputs", "meta_policy_citations.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    policy_document_id = col_or_na(., pick_col(., c("policy_document_id", "id"))),
    doi = norm_doi(col_or_na(., pick_col(., c("dois_cited", "doi", "DOI")))),
    src_old_meta = TRUE
  ) %>% filter(!is.na(policy_document_id), !is.na(doi))

#union all four with source flags. a (policy_doc, doi) pair seen in
#multiple sources gets every src_* flag it earned set to TRUE
policy_links <- bind_rows(
    links_overton_full, links_new, links_old_direct, links_old_meta
  ) %>%
  group_by(policy_document_id, doi) %>%
  summarize(
    src_overton_full = any(src_overton_full, na.rm = TRUE),
    src_new          = any(src_new,          na.rm = TRUE),
    src_old_direct   = any(src_old_direct,   na.rm = TRUE),
    src_old_meta     = any(src_old_meta,     na.rm = TRUE),
    .groups = "drop"
  )

#policy doc metadata - start from newest, fill gaps from legacy
docs_old_direct <- read_csv(
  here("data", "01_legacy_original_inputs", "IES_policy_doc_info.csv"),
  show_col_types = FALSE
)
docs_old_meta <- read_csv(
  here("data", "01_legacy_original_inputs", "meta_policy_doc_info.csv"),
  show_col_types = FALSE
)

#docs_overton_full first so its (newest, fullest) metadata wins for ids
#that appear in multiple sources. first_nonmissing on across(everything())
#means later sources only fill fields the earlier source left blank
policy_docs <- docs_overton_full %>%
  bind_rows(docs_new) %>%
  bind_rows(
    docs_old_direct %>% transmute(
      policy_document_id = col_or_na(., pick_col(., c("policy_document_id", "id"))),
      policy_title = col_or_na(., pick_col(., c("policy_title", "title"))),
      policy_source_title = col_or_na(., pick_col(., c("policy_source_title", "source_title")))
    ) %>% distinct()
  ) %>%
  bind_rows(
    docs_old_meta %>% transmute(
      policy_document_id = col_or_na(., pick_col(., c("policy_document_id", "id"))),
      policy_title = col_or_na(., pick_col(., c("policy_title", "title"))),
      policy_source_title = col_or_na(., pick_col(., c("policy_source_title", "source_title")))
    ) %>% distinct()
  ) %>%
  group_by(policy_document_id) %>%
  summarize(across(everything(), first_nonmissing), .groups = "drop")

#publisher-name fallback for source_type. Overton leaves source_type blank for a
#set of docs whose publisher clearly identifies the type ("Government of X" /
#"State of X" -> government; OECD/UNESCO/World Bank -> igo; NBER/Brookings/RAND ->
#think tank). fill it here, before the routes are built, so every downstream
#consumer (reach, overlap matrices, survival, lattice) inherits the corrected
#type. committed lookup shared with stage 05 (which uses its country column for
#the geography fallback). NA-only: Overton's existing tags are never overridden
pub_lookup <- read_csv(here("data", "_rewrite_outputs", "publisher_lookup.csv"),
                       show_col_types = FALSE)
pub_stype <- setNames(pub_lookup$stype, pub_lookup$publisher)
policy_docs <- policy_docs %>%
  mutate(policy_source_type = if_else(
    is.na(policy_source_type) & !is.na(policy_source_title),
    coalesce(unname(pub_stype[policy_source_title]), policy_source_type),
    policy_source_type))

#---------------------------------
## 2. DIRECT ROUTE (CURRENT) ##
#---------------------------------

#a policy document is on the direct route for a grant if it cites any DOI
#in the grant's publication universe. simple inner join on doi

#many-to-many is expected: a grant has multiple DOIs, and a DOI is cited
#by multiple policy docs. declaring intent silences dplyr's warning
direct_links <- pairs %>%
  inner_join(policy_links %>% select(policy_document_id, doi),
             by = "doi", relationship = "many-to-many") %>%
  left_join(policy_docs, by = "policy_document_id") %>%
  distinct(grant_id, doi, policy_document_id,
           policy_title, policy_source_type, policy_source_country,
           policy_published_year, policy_document_url) %>%
  drop_pre_award()

#per-grant direct route summary
direct_summary <- direct_links %>%
  group_by(grant_id) %>%
  summarize(
    n_direct_dois = n_distinct(doi),
    n_direct_docs = n_distinct(policy_document_id),
    .groups = "drop"
  )

#---------------------------------
## 3. META ROUTE (CURRENT) ##
#---------------------------------

#the OpenAlex meta-citers file maps every IES grant publication DOI to the
#meta-analyses that cite it (review-type works in OpenAlex). this comes
#out of stage 19 (Python script, runs against OpenAlex). we join twice:
#  - first, grant_id -> searched_doi -> meta_doi (the citing meta)
#  - then, meta_doi -> policy_document_id (treating the meta_doi as a
#    direct citation candidate against the same Overton link table)

meta_raw <- read_csv(
  pick_input("meta_analysis_doi_links_to_searched_dois_and_grants.csv"),
  show_col_types = FALSE
)

#meta-citation links per grant (one row per grant - searched_doi - meta_doi)
#we also keep meta_title because the legacy sankey/dashboard scripts
#(15, 16) display it as the citing paper's title
meta_links <- meta_raw %>%
  transmute(
    grant_id = clean_grant(col_or_na(., pick_col(., c("grant_id", "AwardNum")))),
    searched_doi = norm_doi(col_or_na(., pick_col(., c("searched_doi", "source_doi", "doi")))),
    meta_doi = norm_doi(col_or_na(., pick_col(., c("meta_analysis_doi", "meta_doi", "citing_doi")))),
    meta_year = col_or_na(., pick_col(., c("citing_year", "meta_year"))) %>% as.integer(),
    meta_title = col_or_na(., pick_col(., c("citing_title", "meta_title", "title")))
  ) %>%
  filter(!is.na(grant_id), !is.na(searched_doi), !is.na(meta_doi)) %>%
  #the searched_doi must be the grant's own paper in the universe AND it must be
  #EMPIRICAL. the meta route represents meta-analyses that pooled the grant's
  #empirical findings, so a meta-analysis that reaches a grant only by citing one
  #of the grant's NON-empirical products (a review/commentary the team wrote) is
  #not "a meta-analysis of the trial" and is dropped here. restricting
  #searched_doi to empirical removes the meta links anchored on the non-empirical
  #DOIs we filtered out of the first round (87 of 780 links; 23 metas that cite
  #only non-empirical grant papers)
  semi_join(pairs %>% filter(is_empirical),
            by = c("grant_id", "searched_doi" = "doi")) %>%
  distinct()

#restrict the meta route to TRUE meta-analyses. the citing-works set above is
#every OpenAlex review-type work that cites a grant paper, which lumps in plain
#systematic/literature reviews and the occasional meta-cognition paper. stages
#00d2/00d3 classify each citing work - OpenAlex title + self-referential
#abstract, plus Europe PMC's curated "Meta-Analysis" publication type - and
#write is_meta_analysis to table_11. keep only those so the "meta-analysis
#route" the paper reports is genuinely meta-analyses, not reviews in general
meta_class_path <- here("outputs", "05_rebuild_current_policy_routes",
                        "table_11_meta_work_metadata.csv")
if (file.exists(meta_class_path)) {
  true_meta <- read_csv(meta_class_path, show_col_types = FALSE) %>%
    filter(is_meta_analysis) %>%
    transmute(meta_doi = norm_doi(meta_doi)) %>%
    distinct()
  n_before <- n_distinct(meta_links$meta_doi)
  meta_links <- meta_links %>% semi_join(true_meta, by = "meta_doi")
  cat(sprintf("Meta route restricted to true meta-analyses: %d -> %d distinct meta DOIs\n",
              n_before, n_distinct(meta_links$meta_doi)))
} else {
  cat("WARNING: table_11 meta classification missing; meta route left unfiltered.\n")
}

#per-grant meta summary
meta_summary <- meta_links %>%
  group_by(grant_id) %>%
  summarize(
    n_source_dois_with_meta = n_distinct(searched_doi),
    n_meta_dois = n_distinct(meta_doi),
    .groups = "drop"
  )

#now route the meta DOIs to policy docs via the same Overton link table
meta_policy_links <- meta_links %>%
  inner_join(policy_links %>% select(policy_document_id, doi),
             by = c("meta_doi" = "doi"), relationship = "many-to-many") %>%
  left_join(policy_docs, by = "policy_document_id") %>%
  distinct(grant_id, searched_doi, meta_doi, meta_year, meta_title,
           policy_document_id, policy_title, policy_source_type,
           policy_source_country, policy_published_year,
           policy_document_url) %>%
  drop_pre_award()

meta_policy_summary <- meta_policy_links %>%
  group_by(grant_id) %>%
  summarize(
    n_meta_dois_reaching_policy = n_distinct(meta_doi),
    n_meta_policy_docs = n_distinct(policy_document_id),
    .groups = "drop"
  )

#------------------------------
## 4. COMBINE THE TWO ROUTES ##
#------------------------------

#one row per (grant, policy_doc) with a route flag. a policy doc can have
#BOTH routes (cited the grant's pub directly AND a meta-analysis that
#includes it). we keep that distinction in the source flags

routes <- bind_rows(
  direct_links %>% transmute(grant_id, policy_document_id,
                              policy_source_type, policy_source_country,
                              policy_published_year, policy_document_url,
                              has_direct_path = TRUE, has_meta_path = FALSE),
  meta_policy_links %>% transmute(grant_id, policy_document_id,
                                   policy_source_type, policy_source_country,
                                   policy_published_year, policy_document_url,
                                   has_direct_path = FALSE, has_meta_path = TRUE)
) %>%
  group_by(grant_id, policy_document_id) %>%
  summarize(
    has_direct_path = any(has_direct_path),
    has_meta_path = any(has_meta_path),
    policy_source_type = first_nonmissing(policy_source_type),
    policy_source_country = first_nonmissing(policy_source_country),
    policy_published_year = first_nonmissing(policy_published_year),
    policy_document_url = first_nonmissing(policy_document_url),
    .groups = "drop"
  )

#---------------------------------------------
## 4b. SECOND-ORDER ROUTE (P2P AMPLIFICATION) ##
#---------------------------------------------

#a policy doc D' is on the second-order route for grant G if D' cites
#some first-order policy doc D (one that already reaches G via direct
#or meta). chain: grant -> grant's DOI (or meta DOI) -> first-order
#doc D -> second-order doc D'
#
#this can't add new grants (no second-order without a first-order) but
#it dramatically expands the per-grant policy-doc footprint and lets us
#see which intermediaries (think tanks, IGOs) amplify IES research
#furthest into the policy literature
#
#00g pre-filters the 5.3M-row policy_to_policy table to edges where the
#cited side is in our first-order set, so the file we read is small

p2p_edges <- read_csv(
  pick_input("overton_full_p2p_edges.csv"),
  show_col_types = FALSE
)

#metadata for second-order docs not already in policy_docs. 00g pulled
#these from df_policy_doc_info.csv
docs_second_order <- read_csv(
  pick_input("overton_full_second_order_docs.csv"),
  show_col_types = FALSE,
  guess_max = 100000
) %>%
  transmute(
    policy_document_id = col_or_na(., pick_col(., c("policy_document_id", "id"))),
    policy_title = col_or_na(., pick_col(., c("policy_title", "title"))),
    policy_source_id = col_or_na(., pick_col(., c("policy_source_id", "source_id"))),
    policy_source_type = col_or_na(., pick_col(., c("policy_source_type", "source_type"))),
    policy_source_country = col_or_na(., pick_col(., c("policy_source_country", "country"))),
    policy_published_date = col_or_na(., pick_col(., c("policy_published_date", "published_on", "published"))),
    policy_published_year = as.integer(str_extract(as.character(policy_published_date), "^(19|20)[0-9]{2}")),
    overton_policy_document_series = col_or_na(., pick_col(., c("overton_policy_document_series", "series"))),
    policy_document_url = col_or_na(., pick_col(., c("policy_document_url", "url"))),
    language = col_or_na(., pick_col(., c("language", "lang")))
  ) %>%
  distinct()

#unified metadata table that covers BOTH first- and second-order docs,
#so the join below picks up the right titles/country/type either way
policy_docs_all <- bind_rows(policy_docs, docs_second_order) %>%
  group_by(policy_document_id) %>%
  summarize(across(everything(), first_nonmissing), .groups = "drop")

#start from routes (grant, first-order doc) and follow each first-order
#doc's incoming citations to the second-order docs
second_order_links <- routes %>%
  select(grant_id, intermediate_policy_document_id = policy_document_id) %>%
  inner_join(
    p2p_edges %>% rename(
      second_order_policy_document_id = policy_document_id,
      intermediate_policy_document_id = cited_policy_document_id
    ),
    by = "intermediate_policy_document_id",
    relationship = "many-to-many"
  ) %>%
  left_join(
    policy_docs_all %>% rename(second_order_policy_document_id = policy_document_id),
    by = "second_order_policy_document_id"
  ) %>%
  distinct(grant_id, intermediate_policy_document_id,
           second_order_policy_document_id, policy_title, policy_source_type,
           policy_source_country, policy_published_year, policy_document_url)

#per-grant second-order summary. amplification_factor = how many
#downstream docs each first-order doc reaches on average for this grant
#a grant with a single first-order doc that gets cited 50 times downstream
#is far more "amplified" than one with 50 first-order docs that each get
#cited once
second_order_summary <- second_order_links %>%
  group_by(grant_id) %>%
  summarize(
    n_intermediate_docs = n_distinct(intermediate_policy_document_id),
    n_second_order_docs = n_distinct(second_order_policy_document_id),
    amplification_factor = round(n_second_order_docs / n_intermediate_docs, 2),
    .groups = "drop"
  )

#which second-order docs are NEW (not also a first-order doc for this
#grant). this is the "true amplification" set - docs only reachable
#through P2P citation, not by directly citing an IES DOI
first_order_pairs_by_grant <- routes %>%
  select(grant_id, policy_document_id) %>%
  distinct()

second_order_new_only <- second_order_links %>%
  anti_join(
    first_order_pairs_by_grant,
    by = c("grant_id", "second_order_policy_document_id" = "policy_document_id")
  )

#--------------------------------------
## 5. POLICY SEARCH SCOPE PER GRANT ##
#--------------------------------------

#a grant that was never submitted to the most recent Overton search
#shouldn't be counted as "no reach" - it was simply never asked
#we compare two submitted lists vs. the grant's current DOIs:
#  - source DOIs submitted to the direct search
#  - meta DOIs submitted to the meta search

submitted_direct <- read_csv(
  here("data", "04_new_policy_inputs", "doi_inputs (2).csv"),
  show_col_types = FALSE
) %>%
  transmute(doi = norm_doi(col_or_na(., pick_col(., c("doi", "DOI"))))) %>%
  filter(!is.na(doi)) %>% distinct()

submitted_meta <- read_csv(
  here("data", "04_new_policy_inputs", "meta_review_doi (2).csv"),
  show_col_types = FALSE
) %>%
  transmute(meta_doi = norm_doi(col_or_na(., pick_col(., c("doi", "DOI"))))) %>%
  filter(!is.na(meta_doi)) %>% distinct()

#per-grant direct scope: how many of each grant's pubs were submitted?
direct_scope <- pairs %>%
  left_join(submitted_direct %>% mutate(was_submitted = TRUE), by = "doi") %>%
  mutate(was_submitted = replace_na(was_submitted, FALSE)) %>%
  group_by(grant_id) %>%
  summarize(
    n_current_dois = n(),
    n_dois_in_submitted_direct_search = sum(was_submitted),
    direct_search_scope = case_when(
      n_dois_in_submitted_direct_search == n_current_dois ~ "all",
      n_dois_in_submitted_direct_search == 0 ~ "none",
      TRUE ~ "partial"
    ),
    .groups = "drop"
  )

#per-grant meta scope: how many of each grant's meta DOIs were submitted?
meta_scope <- meta_links %>%
  distinct(grant_id, meta_doi) %>%
  left_join(submitted_meta %>% mutate(was_submitted = TRUE), by = "meta_doi") %>%
  mutate(was_submitted = replace_na(was_submitted, FALSE)) %>%
  group_by(grant_id) %>%
  summarize(
    n_meta_dois = n(),
    n_meta_dois_in_submitted = sum(was_submitted),
    meta_search_scope = case_when(
      n_meta_dois_in_submitted == n_meta_dois ~ "all",
      n_meta_dois_in_submitted == 0 ~ "none",
      TRUE ~ "partial"
    ),
    .groups = "drop"
  )

search_scope <- direct_scope %>%
  full_join(meta_scope, by = "grant_id")

#----------------------
## 6. WRITE OUTPUTS ##
#----------------------

#preserve original output paths so downstream scripts keep working
out03 <- here("outputs", "03_build_policy_doi_linkage_reference")
out05 <- here("outputs", "05_rebuild_current_policy_routes")
out05c <- here("outputs", "05c_build_policy_search_scope")
walk(c(out03, out05, out05c), ~dir.create(.x, showWarnings = FALSE, recursive = TRUE))

write_csv(policy_links, file.path(out03, "table_01_policy_doi_links_clean.csv"))
write_csv(policy_docs, file.path(out03, "table_02_policy_document_reference.csv"))

write_csv(direct_links, file.path(out05, "table_01_current_direct_policy_links.csv"))
write_csv(direct_summary,
          file.path(out05, "table_02_current_direct_policy_summary_by_grant.csv"))
write_csv(meta_links, file.path(out05, "table_03_current_grant_to_meta_links.csv"))
write_csv(meta_summary, file.path(out05, "table_04_current_grant_meta_summary.csv"))
write_csv(meta_policy_links,
          file.path(out05, "table_05_current_grant_to_meta_policy_links.csv"))
write_csv(meta_policy_summary,
          file.path(out05, "table_06_current_meta_policy_summary_by_grant.csv"))
write_csv(routes, file.path(out05, "table_07_current_grant_policy_doc_routes.csv"))

write_csv(search_scope, file.path(out05c, "table_01_grant_policy_search_scope.csv"))

#second-order outputs alongside the first-order route tables
write_csv(second_order_links,
          file.path(out05, "table_08_current_grant_to_second_order_links.csv"))
write_csv(second_order_summary,
          file.path(out05, "table_09_current_second_order_summary_by_grant.csv"))
write_csv(second_order_new_only,
          file.path(out05, "table_10_current_second_order_new_amplification.csv"))

#high-level numbers for the console
cat("Distinct policy docs:        ", n_distinct(policy_links$policy_document_id), "\n")
cat("Distinct grant-doc routes:   ", nrow(routes), "\n")
cat("Distinct grants with reach:  ", n_distinct(routes$grant_id), "\n")
cat("Direct-only reach:           ",
    sum(routes$has_direct_path & !routes$has_meta_path), "\n")
cat("Meta-only reach:             ",
    sum(!routes$has_direct_path & routes$has_meta_path), "\n")
cat("Both paths:                  ",
    sum(routes$has_direct_path & routes$has_meta_path), "\n")

#source contribution to the policy_links union - helps diagnose which
#feed is doing what for sanity-checking new dump pulls
cat("\n--- policy_links source contributions ---\n")
cat("from Overton 20260305 full dump: ", sum(policy_links$src_overton_full), "\n")
cat("from submission-based export:    ", sum(policy_links$src_new), "\n")
cat("from legacy IES direct snapshot: ", sum(policy_links$src_old_direct), "\n")
cat("from legacy meta snapshot:       ", sum(policy_links$src_old_meta), "\n")

#second-order summary (the P2P amplification layer)
cat("\n--- second-order (P2P amplification) ---\n")
cat("Distinct second-order docs:           ",
    n_distinct(second_order_links$second_order_policy_document_id), "\n")
cat("Grant -> second-order doc pairs:      ",
    nrow(second_order_links %>% distinct(grant_id, second_order_policy_document_id)), "\n")
cat("Grants with any second-order reach:   ",
    n_distinct(second_order_links$grant_id), "\n")
cat("Second-order pairs NOT also first-order:",
    nrow(second_order_new_only %>%
           distinct(grant_id, second_order_policy_document_id)), "\n")
