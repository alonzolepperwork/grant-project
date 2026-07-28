# ============================================================================
# POLICY-TO-POLICY CROSSWALK -> SECOND-ORDER LINKING -> CHRONOLOGICAL WORKFLOW
# ============================================================================
#
#   PART 1  CROSSWALK EXTRACT
#           Read Overton's df_policy_to_policy.csv and keep the edges whose CITED
#           side is a first-order IES doc -> the (citing, cited) crosswalk
#   PART 2  SECOND-ORDER LINKING
#           Follow each grant's first-order doc through the crosswalk to the docs
#           that cite it -> grant -> DOI -> first-order doc -> second-order doc,
#           plus the per-grant amplification factor
#   PART 3  CHRONOLOGICAL WORKFLOW
#           For each grant, the FIRST year each source type (think tank / gov /
#           IGO) cited it, the order they cited in, and the think-tank-vs-gov lead
#
# PART 1 reads my multi-GB local dump and is OFF by default (EXTRACT_FROM_DUMP),
# because its outputs are already cached; flip it on only to rebuild the edges. (you should set it to TRUE)
# PARTS 2-3 read the finished routes table and the cached edge files, so the
# script runs end-to-end with no dump and no network
#
# outputs (outputs/_p2p_crosswalk_workflow/):
#   table_01_p2p_edges.csv                  citing -> cited (first-order) crosswalk
#   table_02_grant_second_order_links.csv   grant -> first-order -> second-order
#   table_03_grant_amplification.csv        per-grant amplification factor
#   table_04_workflow_distribution.csv      who-cited-first pathway distribution
#   table_05_workflow_tt_vs_gov_lag.csv     think-tank vs government lead/lag
#   table_06_workflow_per_grant.csv         per-grant first-year-by-type + pathway


#------------------
## 0. INITIALIZE ##
#------------------

library(tidyverse)
library(here)
library(data.table)   # fread - only used by PART 1

clean_grant <- function(x) str_to_upper(str_trim(as.character(x)))
norm_doi <- function(x) x %>% as.character() %>% str_to_lower() %>%
  str_remove("^https?://(dx\\.)?doi\\.org/") %>% str_remove("^doi:") %>%
  str_trim() %>% na_if("") %>% na_if("na")

rewrite_dir <- here("data", "_rewrite_outputs")
out_dir     <- here("outputs", "_p2p_crosswalk_workflow")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# normalise any Overton policy-doc metadata file to a common column set, so the
# first-order and second-order metadata can be stacked for the join in PART 2
read_policy_docs <- function(path) {
  if (!file.exists(path)) return(tibble(policy_document_id = character()))
  raw <- read_csv(path, show_col_types = FALSE, guess_max = 100000)
  pick <- function(cands) { h <- intersect(cands, names(raw)); if (length(h)) h[1] else NA_character_ }
  g <- function(col) if (is.na(col)) NA else raw[[col]]
  tibble(
    policy_document_id    = as.character(g(pick(c("policy_document_id", "id")))),
    policy_title          = g(pick(c("policy_title", "title"))),
    policy_source_type    = g(pick(c("policy_source_type", "source_type"))),
    policy_source_country = g(pick(c("policy_source_country", "country"))),
    policy_published_year = as.integer(str_extract(
      as.character(g(pick(c("policy_published_date", "published_on", "published")))),
      "^(19|20)[0-9]{2}")),
    policy_document_url   = g(pick(c("policy_document_url", "url")))
  ) %>% distinct(policy_document_id, .keep_all = TRUE)
}

# ============================================================================ #
# PART 1 - CROSSWALK EXTRACT (from 00e)  -- heavy; OFF by default
# ============================================================================ #

EXTRACT_FROM_DUMP <- FALSE   # set TRUE to re-read the 478 MB / 2.9 GB local dump

p2p_path  <- file.path(rewrite_dir, "overton_full_p2p_edges.csv")
so_path   <- file.path(rewrite_dir, "overton_full_second_order_docs.csv")
fo_path   <- file.path(rewrite_dir, "overton_full_policy_links.csv")
docs_path <- file.path(rewrite_dir, "overton_full_policy_docs.csv")

if (EXTRACT_FROM_DUMP) {
  overton_dir <- here("Overton_20260305")

  # universe DOIs to match against (first-order = any policy doc citing one)
  universe_dois <- read_csv(
    here("outputs", "02_build_current_grant_doi_universe",
         "table_01_current_grant_doi_pair_union.csv"), show_col_types = FALSE) %>%
    pull(doi) %>% norm_doi() %>% unique()

  # first-order links: policy docs that cite a universe DOI
  overton_links <- as_tibble(fread(file.path(overton_dir, "df_policy_to_doi.csv"),
                                   select = c("policy_document_id", "doi"), encoding = "UTF-8")) %>%
    transmute(policy_document_id, doi = norm_doi(doi)) %>%
    filter(doi %in% universe_dois) %>% distinct()
  first_order_ids <- unique(overton_links$policy_document_id)
  cat("First-order policy docs:", length(first_order_ids), "\n")

  # second-order edges: keep policy_to_policy rows whose CITED side is first-order
  p2p_raw <- fread(file.path(overton_dir, "df_policy_to_policy.csv"), encoding = "UTF-8")
  p2p_edges <- as_tibble(p2p_raw) %>%
    filter(cited_policy_document_id %in% first_order_ids) %>% distinct()
  rm(p2p_raw); invisible(gc())
  second_order_ids <- unique(p2p_edges$policy_document_id)
  cat("P2P edges retained:", nrow(p2p_edges),
      "| distinct second-order docs:", length(second_order_ids), "\n")

  # metadata for the NEW second-order docs (not already first-order)
  new_so_ids <- setdiff(second_order_ids, first_order_ids)
  doc_info <- as_tibble(fread(file.path(overton_dir, "df_policy_doc_info.csv"), encoding = "UTF-8")) %>%
    select(-any_of("authors")) %>% distinct()
  second_order_meta <- doc_info %>% filter(policy_document_id %in% new_so_ids)
  rm(doc_info); invisible(gc())

  write_csv(p2p_edges, p2p_path)
  write_csv(second_order_meta, so_path)
  cat("Refreshed", p2p_path, "and", so_path, "\n")
} else {
  cat("PART 1 skipped (EXTRACT_FROM_DUMP = FALSE); reading cached crosswalk files.\n")
}

# ============================================================================ #
# PART 2 - SECOND-ORDER LINKING (from 02, section 4b)
# ============================================================================ #

# the crosswalk: citing doc -> cited first-order doc
p2p_edges <- read_csv(p2p_path, show_col_types = FALSE)

# the grant -> first-order policy-doc routes (direct + meta), already built by the
# pipeline (stage 05). this is the left side of the second-order hop
routes <- read_csv(
  here("outputs", "05_rebuild_current_policy_routes",
       "table_07_current_grant_policy_doc_routes.csv"), show_col_types = FALSE) %>%
  mutate(grant_id = clean_grant(grant_id),
         policy_document_id = as.character(policy_document_id))

# metadata covering BOTH first- and second-order docs, so the join picks up the
# right title/type/country/year for each second-order (citing) doc
policy_docs_all <- bind_rows(read_policy_docs(docs_path), read_policy_docs(so_path)) %>%
  group_by(policy_document_id) %>%
  summarize(across(everything(), ~ .[!is.na(.)][1]), .groups = "drop")

# follow each grant's first-order doc to the docs that cite it
second_order_links <- routes %>%
  select(grant_id, intermediate_policy_document_id = policy_document_id) %>%
  inner_join(
    p2p_edges %>% mutate(across(everything(), as.character)) %>%
      rename(second_order_policy_document_id = policy_document_id,
             intermediate_policy_document_id = cited_policy_document_id),
    by = "intermediate_policy_document_id", relationship = "many-to-many") %>%
  left_join(policy_docs_all %>% rename(second_order_policy_document_id = policy_document_id),
            by = "second_order_policy_document_id") %>%
  distinct(grant_id, intermediate_policy_document_id, second_order_policy_document_id,
           policy_title, policy_source_type, policy_source_country,
           policy_published_year, policy_document_url)

# per-grant amplification: how many downstream docs each first-order doc reaches
# a grant with one first-order doc cited 50x downstream is far more amplified than
# one with 50 first-order docs each cited once
amplification <- second_order_links %>%
  group_by(grant_id) %>%
  summarize(n_intermediate_docs = n_distinct(intermediate_policy_document_id),
            n_second_order_docs = n_distinct(second_order_policy_document_id),
            amplification_factor = round(n_second_order_docs / n_intermediate_docs, 2),
            .groups = "drop") %>%
  arrange(desc(amplification_factor))

write_csv(p2p_edges,          file.path(out_dir, "table_01_p2p_edges.csv"))
write_csv(second_order_links, file.path(out_dir, "table_02_grant_second_order_links.csv"))
write_csv(amplification,      file.path(out_dir, "table_03_grant_amplification.csv"))

cat("\nPART 2:", nrow(second_order_links), "grant->second-order links across",
    n_distinct(second_order_links$grant_id), "grants;",
    n_distinct(second_order_links$second_order_policy_document_id), "distinct second-order docs.\n")

# ============================================================================ #
# PART 3 - CHRONOLOGICAL WORKFLOW (from 09, section 6b)
# ============================================================================ #
# For each reached grant: did a think tank cite its work before government did, or
# the other way around? this is the temporal "order it was cited" behind the
# think-tanks-as-bridge framing. uses policy_published_year (first hop only;
# second-order excluded). pre-award policy citations are dropped first

year_from_grant_id <- function(gid) {
  yr2 <- suppressWarnings(as.integer(sub(".*[A-Z](\\d{2})\\d{4}$", "\\1", gid, perl = TRUE)))
  ifelse(is.na(yr2), NA_integer_, 2000L + yr2)
}

routes_temporal <- routes %>%
  mutate(award_year = year_from_grant_id(grant_id),
         policy_year = as.integer(policy_published_year)) %>%
  filter(!is.na(policy_year), !is.na(award_year), policy_year >= award_year)

# first year a doc of each major source_type cited each grant
grant_first_year_by_type <- routes_temporal %>%
  filter(policy_source_type %in% c("think tank", "government", "igo")) %>%
  group_by(grant_id, policy_source_type) %>%
  summarize(first_year = min(policy_year, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = policy_source_type, values_from = first_year,
              names_prefix = "first_year_") %>%
  rename(`first_year_think_tank` = `first_year_think tank`)

# classify each grant by which source type led chronologically
workflow <- grant_first_year_by_type %>%
  mutate(
    has_tt  = !is.na(first_year_think_tank),
    has_gov = !is.na(first_year_government),
    has_igo = !is.na(first_year_igo),
    tt_minus_gov_yrs = first_year_think_tank - first_year_government,
    pathway = case_when(
      has_tt & has_gov & first_year_think_tank < first_year_government ~ "Think tank first, then government",
      has_tt & has_gov & first_year_government < first_year_think_tank ~ "Government first, then think tank",
      has_tt & has_gov & first_year_think_tank == first_year_government ~ "Same year (think tank + government)",
      has_tt & !has_gov & has_igo & first_year_think_tank < first_year_igo ~ "Think tank first, then IGO (no government)",
      has_tt & !has_gov & has_igo & first_year_igo < first_year_think_tank ~ "IGO first, then think tank (no government)",
      has_tt & !has_gov & !has_igo ~ "Think tank only",
      !has_tt & has_gov & !has_igo ~ "Government only",
      !has_tt & !has_gov & has_igo ~ "IGO only",
      !has_tt & has_gov & has_igo & first_year_government < first_year_igo ~ "Government first, then IGO (no think tank)",
      !has_tt & has_gov & has_igo & first_year_igo < first_year_government ~ "IGO first, then government (no think tank)",
      TRUE ~ "Other / mixed same year"))

workflow_distribution <- workflow %>%
  count(Pathway = pathway, name = "Grants") %>%
  mutate(`% of reached grants` = sprintf("%.1f%%", 100 * Grants / sum(Grants))) %>%
  arrange(desc(Grants))

# for grants reached by BOTH think tanks and government: the lead/lag in years
tt_gov_lag <- workflow %>%
  filter(has_tt & has_gov) %>%
  summarize(
    `Grants reached by both TT and government` = n(),
    `TT leads (TT year < gov year)`            = sum(tt_minus_gov_yrs < 0),
    `Government leads (gov year < TT year)`     = sum(tt_minus_gov_yrs > 0),
    `Same year`                                 = sum(tt_minus_gov_yrs == 0),
    `Median gap (years, gov - TT)`              = median(-tt_minus_gov_yrs),
    `Mean gap (years, gov - TT)`                = round(mean(-tt_minus_gov_yrs), 1)) %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Value")

write_csv(workflow_distribution, file.path(out_dir, "table_04_workflow_distribution.csv"))
write_csv(tt_gov_lag,            file.path(out_dir, "table_05_workflow_tt_vs_gov_lag.csv"))
write_csv(workflow,              file.path(out_dir, "table_06_workflow_per_grant.csv"))

cat("\nPART 3: workflow classified for", nrow(workflow), "reached grants.\n")
print(workflow_distribution, n = Inf)
cat("\nThink-tank vs government lead (grants reached by both):\n")
print(tt_gov_lag)
cat("\nAll tables written to", out_dir, "\n")
