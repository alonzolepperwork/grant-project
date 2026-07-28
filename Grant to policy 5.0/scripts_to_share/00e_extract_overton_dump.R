# extracts everything we need from the Overton 20260305 full database dump in a
# single pass, so the giant source files are streamed once each and the 2.9 GB
# policy-doc metadata table is read ONLY ONCE instead of twice
#
# the dump is the full Overton database extract (~10M policy docs (not-deduped)), not just the
# matches our earlier submission-based search surfaced. linking against it is the
# coverage we're after, especially for think tanks and non- global north
# sources the submission flow under-counted. everything matches on normalized DOI
# / document id, so the keys line up with stage 02
#
# the work, in order: (note for self)
#   1. first-order links  - which policy docs cite a universe DOI       (was 00f)
#   2. second-order edges - which policy docs cite a first-order doc     (was 00g)
#   3. document metadata  - one fread of df_policy_doc_info for both sets (00f+00g)
#   4. classification streams - IPTC / SDG / granular topics             (was 00h)
#
# inputs (all under Overton_20260305/, at the project root since 2026-06-02):
#   df_policy_to_doi.csv        (1.16 GB, 16.4M rows)
#   df_policy_to_policy.csv     (478 MB,  5.3M rows)
#   df_policy_doc_info.csv      (2.9 GB,  10.17M rows)   <- read once, used twice
#   df_policy_classifications.csv (5.5 GB, 66.9M rows)
#   df_policy_sdgcategories.csv   (1.26 GB, 17.5M rows)
#   df_policy_topics.csv          (11 GB,  173.9M rows)
#   outputs/02_build_current_grant_doi_universe/table_01_current_grant_doi_pair_union.csv
#
# outputs (all small, cached under data/_rewrite_outputs/ for stage 02):
#   overton_full_policy_links.csv        - (policy_doc, doi) first-order matches
#   overton_full_policy_docs.csv         - metadata for first-order docs
#   overton_full_p2p_edges.csv           - (citing, cited) second-order edges
#   overton_full_second_order_docs.csv   - metadata for NEW second-order docs
#   overton_full_classifications.csv     - IPTC top-level topics
#   overton_full_sdg.csv                 - UN SDG codes
#   overton_full_topics.csv              - granular Overton topic tags
#
# all source streaming is local - no API calls

#------------------
## 0. INITIALIZE ##
#------------------

library(tidyverse)
library(here)
library(data.table)  # fread is much faster than read_csv on multi-GB CSVs

rewrite_dir <- here("data", "_rewrite_outputs")
dir.create(rewrite_dir, showWarnings = FALSE, recursive = TRUE)

overton_dir <- here("Overton_20260305")

# same normalization stage 02 uses, so the join keys line up
norm_doi <- function(x) {
  x %>% as.character() %>% str_to_lower() %>%
    str_remove("^https?://doi\\.org/") %>%
    str_remove("^doi:") %>% str_trim() %>%
    na_if("") %>% na_if("na")
}

#-----------------------------------------
## 1. UNIVERSE DOIS WE NEED TO MATCH ##
#-----------------------------------------

# pull the current 528-grant universe DOIs from stage 01's output. we only need
# the DOI column for filtering; grant attribution happens later in stage 02
universe_dois <- read_csv(
  here("outputs", "02_build_current_grant_doi_universe",
       "table_01_current_grant_doi_pair_union.csv"),
  show_col_types = FALSE
) %>%
  transmute(doi = norm_doi(doi)) %>%
  filter(!is.na(doi)) %>%
  distinct() %>%
  pull(doi)

cat("Universe DOIs to match:              ", length(universe_dois), "\n")

#----------------------------------------------------
## 2. FIRST-ORDER LINKS (df_policy_to_doi MATCHES) ##
#----------------------------------------------------

# fread + select=c(...) only loads the two columns we need. on a multi-GB CSV
# that drops memory pressure from ~5 GB to under 1 GB
overton_links_raw <- fread(
  file.path(overton_dir, "df_policy_to_doi.csv"),
  select = c("policy_document_id", "doi"),
  encoding = "UTF-8"
)

overton_links <- as_tibble(overton_links_raw) %>%
  transmute(policy_document_id, doi = norm_doi(doi)) %>%
  filter(doi %in% universe_dois) %>%
  distinct()

# release the big raw table before the next fread
rm(overton_links_raw); invisible(gc())

first_order_ids <- unique(overton_links$policy_document_id)

cat("Overton policy->DOI rows scanned:    ", "16.4M (streamed)\n")
cat("Matched (policy_doc, doi) pairs:     ", nrow(overton_links), "\n")
cat("Distinct DOIs matched:               ",
    n_distinct(overton_links$doi),
    sprintf("(%.1f%% of universe)\n",
            100 * n_distinct(overton_links$doi) / length(universe_dois)))
cat("First-order policy docs:             ", length(first_order_ids), "\n")

#-----------------------------------------------------
## 3. SECOND-ORDER EDGES (df_policy_to_policy CITES) ##
#-----------------------------------------------------

# second-order reach: a grant reaches doc D' if some first-order doc D (one that
# cites a universe DOI) is itself cited by D'. so the chain is
#   grant -> grant's DOI -> first-order doc D -> second-order doc D'
# we gain no new grants this way, but a much larger policy footprint, and we can
# explore which intermediaries amplify IES research furthest into the literature
#
# fread the whole edge table (~480 MB, 5.3M rows) and keep the rows whose cited
# side lands in our first-order set. the dump's orientation is
# policy_document_id = citing, cited_policy_document_id = cited; preserve it
p2p_raw <- fread(
  file.path(overton_dir, "df_policy_to_policy.csv"),
  encoding = "UTF-8"
)

p2p_edges <- as_tibble(p2p_raw) %>%
  filter(cited_policy_document_id %in% first_order_ids) %>%
  distinct()

rm(p2p_raw); invisible(gc())

second_order_ids <- unique(p2p_edges$policy_document_id)

cat("\nP2P edges retained (citing -> first-order):", nrow(p2p_edges), "\n")
cat("Distinct second-order doc ids:       ", length(second_order_ids), "\n")
cat("  - already first-order:             ",
    sum(second_order_ids %in% first_order_ids), "\n")
cat("  - genuinely new second-order:      ",
    sum(!(second_order_ids %in% first_order_ids)), "\n")

#-------------------------------------------------------
## 4. DOCUMENT METADATA (ONE fread, BOTH DOC SETS) ##
#-------------------------------------------------------

# the former 00f and 00g each fread this 2.9 GB / 10.17M-row file separately
# here we read it ONCE and slice out both the first-order docs and the NEW
# second-order docs (the ones not already covered as first-order). peak ~6 GB
#
# the upstream notebook (process_overton_dump_to_df.ipynb) has a bug where every
# row's `authors` field is hard-coded to the first record's value. the column is
# junk for this dump - drop it so it doesn't leak into downstream joins
new_second_order_ids <- setdiff(second_order_ids, first_order_ids)

doc_info_raw <- fread(
  file.path(overton_dir, "df_policy_doc_info.csv"),
  encoding = "UTF-8"
)
doc_info <- as_tibble(doc_info_raw) %>%
  select(-any_of("authors")) %>%
  distinct()
rm(doc_info_raw); invisible(gc())

overton_docs <- doc_info %>%
  filter(policy_document_id %in% first_order_ids)

second_order_meta <- doc_info %>%
  filter(policy_document_id %in% new_second_order_ids)

rm(doc_info); invisible(gc())

cat("\nFirst-order metadata rows:           ", nrow(overton_docs),
    "(of", length(first_order_ids), "ids)\n")
cat("New second-order metadata rows:      ", nrow(second_order_meta),
    "(of", length(new_second_order_ids), "needed)\n")

#-------------------------------------------------
## 5. CLASSIFICATION STREAMS (IPTC / SDG / TOPIC) ##
#-------------------------------------------------

# "what is this policy doc about?" is answered by three long-format
# (policy_document_id, tag) files. filter each to the IES-relevant union
# (first-order + second-order, ~47k ids) to drop them from GB to MB
#   classifications - IPTC top-level media topics (the headline "what topics?")
#   sdg             - UN SDG codes (secondary lens, international framing)
#   topics          - granular Overton tags (too fine for top lines, good for
#                     keyword analyses)
ies_relevant_ids <- union(first_order_ids, second_order_ids)
cat("\nIES-relevant union (1st + 2nd order):", length(ies_relevant_ids), "\n")
# IES-relevant union (1st + 2nd order): 41793 

# 5.5 GB / 66.9M rows. fread streams in ~30s
classif_raw <- fread(
  file.path(overton_dir, "df_policy_classifications.csv"),
  encoding = "UTF-8"
)
classif_filt <- as_tibble(classif_raw) %>%
  filter(policy_document_id %in% ies_relevant_ids) %>%
  distinct()
rm(classif_raw); invisible(gc())
cat("Classifications kept:      ", nrow(classif_filt),
    "for", n_distinct(classif_filt$policy_document_id), "docs\n")
# Classifications kept:       321166 for 41127 docs

# 1.26 GB / 17.5M rows. fastest of the three
sdg_raw <- fread(
  file.path(overton_dir, "df_policy_sdgcategories.csv"),
  encoding = "UTF-8"
)
sdg_filt <- as_tibble(sdg_raw) %>%
  filter(policy_document_id %in% ies_relevant_ids) %>%
  distinct()
rm(sdg_raw); invisible(gc())
cat("SDG rows kept:             ", nrow(sdg_filt),
    "for", n_distinct(sdg_filt$policy_document_id), "docs\n")
# SDG rows kept:              113304 for 37325 docs

# 11 GB / 173.9M rows - the heavy one. fread peaks around 10-12 GB RAM during
# the load. acceptable as a one-time cost; output is small
topics_raw <- fread(
  file.path(overton_dir, "df_policy_topics.csv"),
  encoding = "UTF-8"
)
topics_filt <- as_tibble(topics_raw) %>%
  filter(policy_document_id %in% ies_relevant_ids) %>%
  distinct()
rm(topics_raw); invisible(gc())
cat("Topic rows kept:           ", nrow(topics_filt),
    "for", n_distinct(topics_filt$policy_document_id), "docs\n")
# Topic rows kept:            1840194 for 41120 docs

#----------------------
## 6. WRITE OUTPUTS ##
#----------------------

write_csv(overton_links,      file.path(rewrite_dir, "overton_full_policy_links.csv"))
write_csv(overton_docs,       file.path(rewrite_dir, "overton_full_policy_docs.csv"))
write_csv(p2p_edges,          file.path(rewrite_dir, "overton_full_p2p_edges.csv"))
write_csv(second_order_meta,  file.path(rewrite_dir, "overton_full_second_order_docs.csv"))
write_csv(classif_filt,       file.path(rewrite_dir, "overton_full_classifications.csv"))
write_csv(sdg_filt,           file.path(rewrite_dir, "overton_full_sdg.csv"))
write_csv(topics_filt,        file.path(rewrite_dir, "overton_full_topics.csv"))

cat("\nWrote:\n")
cat("  overton_full_policy_links.csv       -", nrow(overton_links),     "rows\n")
cat("  overton_full_policy_docs.csv        -", nrow(overton_docs),      "rows\n")
cat("  overton_full_p2p_edges.csv          -", nrow(p2p_edges),         "edges\n")
cat("  overton_full_second_order_docs.csv  -", nrow(second_order_meta), "rows\n")
cat("  overton_full_classifications.csv    -", nrow(classif_filt),      "rows\n")
cat("  overton_full_sdg.csv                -", nrow(sdg_filt),          "rows\n")
cat("  overton_full_topics.csv             -", nrow(topics_filt),       "rows\n")
