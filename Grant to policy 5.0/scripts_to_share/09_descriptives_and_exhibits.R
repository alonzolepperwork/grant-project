# the paper's output layer: descriptive tables + the shareable descriptives
# workbook, then the full paper-exhibits workbook. run descriptives first, since
# the exhibits workbook lifts several of its tables
#
#   PART A: descriptive tables for the paper + a shareable workbook -
#     subset views, headline descriptives, institution tables, all of it
#     slide-ready
#   PART B: packages every table the paper might reference into one
#     Excel workbook + a markdown index - headline numbers, the paper-vs-current
#     diff, institution-type reach, timing/lag, policy-doc characteristics,
#     second-order reach, topic mix, top amplifiers, per-grant spotlight lists
#     most sheets are lifted from existing outputs/; a few are computed fresh

#------------------
## 0. INITIALIZE ##
#------------------

library(tidyverse)
library(here)
library(openxlsx)

# ============================================================================ #
# PART A - descriptives_and_workbook
# ============================================================================ #
#load master + key derived tables
master <- read_csv(
  here("outputs", "06_build_grant_policy_master",
       "table_01_grant_policy_master.csv"),
  show_col_types = FALSE
)
pairs <- read_csv(
  here("outputs", "02_build_current_grant_doi_universe",
       "table_01_current_grant_doi_pair_union.csv"),
  show_col_types = FALSE
)
routes <- read_csv(
  here("outputs", "05_rebuild_current_policy_routes",
       "table_07_current_grant_policy_doc_routes.csv"),
  show_col_types = FALSE
)
classifications_path <- here("outputs", "grant_classifications.csv")
if (file.exists(classifications_path)) {
  clf <- read_csv(classifications_path, show_col_types = FALSE)
} else {
  clf <- tibble(grant_id = character(), classification = character())
}

#-----------------------------
## 1. HEADLINE NUMBERS ##
#-----------------------------

#these are the numbers we report in the findings paragraph. recomputing
#them here (rather than trusting the dashboard) gives us a static
#snapshot tied to whatever version of the data we used for the paper

n_grants <- nrow(master)
n_with_pubs <- master %>%
  filter(grant_id %in% unique(pairs$grant_id)) %>% nrow()
n_reached_broad <- sum(master$has_any_current_policy_reach %in%
                          c(TRUE, "TRUE", "true", 1))

#strict reach: only count grants that reached at least one government or
#IGO document. drops grants that only got cited by think tanks
strict_doc_ids <- routes %>%
  filter(replace_na(policy_source_type, "") %in% c("government", "igo")) %>%
  pull(policy_document_id) %>% unique()
strict_reach_grants <- routes %>%
  filter(policy_document_id %in% strict_doc_ids) %>%
  pull(grant_id) %>% unique()
n_reached_strict <- length(strict_reach_grants)

#second-order numbers if available - these come from stage 02's P2P
#amplification layer and the master fields stage 03 attached
so_links_path <- here("outputs", "05_rebuild_current_policy_routes",
                      "table_08_current_grant_to_second_order_links.csv")
has_second_order <- file.exists(so_links_path)
if (has_second_order) {
  so_links <- read_csv(so_links_path, show_col_types = FALSE)
  n_second_order_docs <- n_distinct(so_links$second_order_policy_document_id)
  n_so_grants <- n_distinct(so_links$grant_id)
} else {
  so_links <- tibble()
  n_second_order_docs <- NA_integer_
  n_so_grants <- NA_integer_
}

headline <- tibble(
  metric = c("Total grants in sample",
             "Grants with at least 1 publication",
             "Grants reached - BROAD (any Overton doc)",
             "Grants reached - STRICT (government or IGO only)",
             "Grants with second-order (P2P) reach",
             "Distinct DOIs in universe",
             "Distinct policy documents reached (first-order)",
             "Distinct policy documents reached (second-order)"),
  value = c(n_grants, n_with_pubs, n_reached_broad, n_reached_strict,
            n_so_grants,
            n_distinct(pairs$doi),
            n_distinct(routes$policy_document_id),
            n_second_order_docs)
)
print(headline)

#-----------------------------------
## 2. INSTITUTION-LEVEL TABLES ##
#-----------------------------------

#per-institution-type counts and reach rates. uses the 8-category detail
#from stage 03 (disaggregated Other bucket)

by_inst_type <- master %>%
  group_by(`Institution type` = institution_type_detail) %>%
  summarize(
    `n` = n(),
    `Has publication` = sum(grant_id %in% unique(pairs$grant_id)),
    `% with pub` = paste0(round(100 * mean(grant_id %in%
                                              unique(pairs$grant_id)), 1), "%"),
    `Reached BROAD` = sum(has_any_current_policy_reach %in%
                            c(TRUE, "TRUE", "true", 1)),
    `% reached BROAD` = paste0(round(100 * mean(has_any_current_policy_reach %in%
                                                   c(TRUE, "TRUE", "true", 1)), 1),
                                "%"),
    `Reached STRICT` = sum(grant_id %in% strict_reach_grants),
    `% reached STRICT` = paste0(round(100 * mean(grant_id %in% strict_reach_grants), 1), "%"),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

#top institutions by # grants in the IES portfolio
top_institutions <- master %>%
  count(Institution = institution, name = "Grants") %>%
  arrange(desc(Grants)) %>% head(30)

#top institutions by aggregate policy reach
top_inst_by_reach <- master %>%
  group_by(Institution = institution) %>%
  summarize(
    Grants = n(),
    `Total reaches` = sum(replace_na(n_policy_docs, 0L)),
    `Mean docs per grant` = round(mean(replace_na(n_policy_docs, 0L)), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(`Total reaches`)) %>% head(30)

#---------------------------------------------
## 3. CLASSIFICATION + INTERVENTION TABLES ##
#---------------------------------------------

#reach rate by research design type (Efficacy / Replication / Scale-up / Other)
if (nrow(clf) > 0) {
  by_design <- master %>%
    left_join(clf %>% select(grant_id, classification), by = "grant_id") %>%
    mutate(classification = replace_na(classification, "(unclassified)")) %>%
    group_by(`Design type` = classification) %>%
    summarize(
      Grants = n(),
      Reached = sum(has_any_current_policy_reach %in%
                      c(TRUE, "TRUE", "true", 1)),
      `% reached` = paste0(round(100 * mean(has_any_current_policy_reach %in%
                                              c(TRUE, "TRUE", "true", 1)), 1), "%"),
      .groups = "drop"
    ) %>%
    arrange(desc(Reached))
} else {
  by_design <- tibble()
}

#also intervention type if that file exists
int_path <- here("outputs", "grant_intervention_types.csv")
if (file.exists(int_path)) {
  intv <- read_csv(int_path, show_col_types = FALSE)
  by_intervention <- master %>%
    left_join(intv %>% select(grant_id, intervention_type), by = "grant_id") %>%
    mutate(intervention_type = replace_na(intervention_type, "(unclassified)")) %>%
    group_by(`Intervention type` = intervention_type) %>%
    summarize(
      Grants = n(),
      Reached = sum(has_any_current_policy_reach %in%
                      c(TRUE, "TRUE", "true", 1)),
      `% reached` = paste0(round(100 * mean(has_any_current_policy_reach %in%
                                              c(TRUE, "TRUE", "true", 1)), 1), "%"),
      .groups = "drop"
    ) %>%
    arrange(desc(Reached))
} else {
  by_intervention <- tibble()
}

#---------------------------
## 4. CO-FUNDER ANALYSIS ##
#---------------------------

#how many of the universe DOIs are co-funded by non-IES funders? this is
#the empirical support for the "contributed to" framing in the paper
funder_path <- here("outputs", "openalex_enrichment", "doi_grants.csv")
if (file.exists(funder_path)) {
  funders <- read_csv(funder_path, show_col_types = FALSE)
  ies_pat <- "Institute of Education Sciences|^IES$|Department of Education"
  funders <- funders %>% mutate(
    doi = tolower(doi),
    is_ies = str_detect(funder_name, regex(ies_pat, ignore_case = TRUE))
  )
  per_doi_funders <- funders %>%
    filter(doi %in% tolower(unique(pairs$doi))) %>%
    group_by(doi) %>%
    summarize(n_ies = n_distinct(funder_name[is_ies]),
              n_nonies = n_distinct(funder_name[!is_ies]),
              .groups = "drop")
  cofunder_table <- tibble(
    metric = c("Universe DOIs",
               "With any funder metadata in OpenAlex",
               "Confirmed IES-funded",
               "Co-funded with at least 1 non-IES funder",
               "Co-funded with 2+ non-IES funders"),
    value = c(n_distinct(pairs$doi),
              nrow(per_doi_funders),
              sum(per_doi_funders$n_ies >= 1),
              sum(per_doi_funders$n_ies >= 1 & per_doi_funders$n_nonies >= 1),
              sum(per_doi_funders$n_ies >= 1 & per_doi_funders$n_nonies >= 2))
  )

  #top non-IES funders co-occurring with IES on the same DOIs
  cofunded_dois <- per_doi_funders %>%
    filter(n_ies >= 1, n_nonies >= 1) %>% pull(doi)
  top_nonies <- funders %>%
    filter(doi %in% cofunded_dois, !is_ies) %>%
    mutate(funder_norm = str_remove(funder_name, "Eunice Kennedy Shriver ")) %>%
    distinct(doi, funder_norm) %>%
    count(funder_norm, sort = TRUE, name = "DOIs co-funded") %>%
    head(15)
} else {
  cofunder_table <- tibble()
  top_nonies <- tibble()
}

#-------------------------------
## 4b. PRODUCTIVITY VS REACH ##
#-------------------------------

# the master now carries ERIC productivity counts (00a). compare grant
# productivity against policy reach to see whether more-prolific grants
# also reach policy more often, AND how many "invisible" grant outputs
# we're missing in the citation-graph-based reach metric (products
# without a DOI that we can't trace forward)

if ("n_products_total" %in% names(master)) {

  prod_summary <- tibble(
    metric = c("Grants with any ERIC-indexed product",
               "Total ERIC products (across all grants)",
               "Products with a DOI (trackable through citations)",
               "Products WITHOUT a DOI (invisible to citation graph)",
               "Median products per grant (any kind)",
               "Median products per grant with at least 1 product"),
    value = c(
      sum(master$had_any_eric_output, na.rm = TRUE),
      sum(master$n_products_total, na.rm = TRUE),
      sum(master$n_products_with_doi, na.rm = TRUE),
      sum(master$n_products_without_doi, na.rm = TRUE),
      median(master$n_products_total, na.rm = TRUE),
      median(master$n_products_total[master$n_products_total > 0], na.rm = TRUE)
    )
  )

  # bucketed: does productivity predict reach?
  prod_vs_reach <- master %>%
    mutate(
      reached = has_any_current_policy_reach %in% c(TRUE, "TRUE", "true", 1),
      bucket = case_when(
        n_products_total == 0 ~ "0 products",
        n_products_total %in% 1:2 ~ "1-2 products",
        n_products_total %in% 3:5 ~ "3-5 products",
        n_products_total %in% 6:10 ~ "6-10 products",
        n_products_total > 10 ~ "11+ products",
        TRUE ~ "(unknown)"
      ),
      bucket = factor(bucket, levels = c("0 products", "1-2 products",
                                          "3-5 products", "6-10 products",
                                          "11+ products", "(unknown)"))
    ) %>%
    group_by(`Productivity bucket` = bucket) %>%
    summarize(
      Grants = n(),
      Reached = sum(reached),
      `% reached` = paste0(round(100 * mean(reached), 1), "%"),
      .groups = "drop"
    )

} else {
  prod_summary <- tibble()
  prod_vs_reach <- tibble()
}

#--------------------
## 5. WORKBOOK ##
#--------------------

#export everything to a single Excel file - one tab per table, headline
#first. lets us flip through in Excel without re-running R

wb <- createWorkbook()
addWorksheet(wb, "headline")
writeData(wb, "headline", headline)
addWorksheet(wb, "by_institution_type")
writeData(wb, "by_institution_type", by_inst_type)
addWorksheet(wb, "top_institutions_by_grants")
writeData(wb, "top_institutions_by_grants", top_institutions)
addWorksheet(wb, "top_institutions_by_reach")
writeData(wb, "top_institutions_by_reach", top_inst_by_reach)
if (nrow(by_design) > 0) {
  addWorksheet(wb, "by_design_type")
  writeData(wb, "by_design_type", by_design)
}
if (nrow(by_intervention) > 0) {
  addWorksheet(wb, "by_intervention_type")
  writeData(wb, "by_intervention_type", by_intervention)
}
if (nrow(cofunder_table) > 0) {
  addWorksheet(wb, "cofunder_overview")
  writeData(wb, "cofunder_overview", cofunder_table)
  addWorksheet(wb, "top_nonIES_funders")
  writeData(wb, "top_nonIES_funders", top_nonies)
}
if (nrow(prod_summary) > 0) {
  addWorksheet(wb, "productivity_overview")
  writeData(wb, "productivity_overview", prod_summary)
  addWorksheet(wb, "productivity_vs_reach")
  writeData(wb, "productivity_vs_reach", prod_vs_reach)
}

#second-order (P2P amplification) sheets - top amplifying intermediaries,
#second-order doc source-type breakdown, top grants by amplification
if (has_second_order) {

  # top amplifiers - which first-order docs reach the most downstream docs
  top_amplifiers <- so_links %>%
    group_by(intermediate_policy_document_id) %>%
    summarize(
      n_downstream_docs = n_distinct(second_order_policy_document_id),
      n_grants_amplified = n_distinct(grant_id),
      source_type = first(policy_source_type),
      country = first(policy_source_country),
      published_year = first(policy_published_year),
      title = first(policy_title),
      .groups = "drop"
    ) %>%
    arrange(desc(n_downstream_docs)) %>%
    head(50)

  addWorksheet(wb, "p2p_top_amplifiers")
  writeData(wb, "p2p_top_amplifiers", top_amplifiers)

  # second-order docs by source type - the "think tanks as bridge"
  # comparison table. paste alongside the first-order breakdown to see
  # the type shift from first to second order
  so_by_type <- so_links %>%
    distinct(second_order_policy_document_id, policy_source_type) %>%
    count(`Source type` = replace_na(policy_source_type, "(unspecified)"),
          name = "Second-order docs") %>%
    mutate(`% of SO docs` =
             paste0(round(100 * `Second-order docs` /
                            sum(`Second-order docs`), 1), "%")) %>%
    arrange(desc(`Second-order docs`))

  addWorksheet(wb, "p2p_second_order_by_type")
  writeData(wb, "p2p_second_order_by_type", so_by_type)

  # grants ranked by amplification factor (downstream docs per intermediate)
  # only grants with at least 3 intermediate docs - small intermediates
  # produce noisy ratios
  if ("amplification_factor" %in% names(master)) {
    top_amplified_grants <- master %>%
      filter(n_intermediate_docs >= 3) %>%
      select(grant_id, pi, institution, award_year,
             n_intermediate_docs, n_second_order_docs, amplification_factor) %>%
      arrange(desc(amplification_factor)) %>%
      head(50)

    addWorksheet(wb, "p2p_top_amplified_grants")
    writeData(wb, "p2p_top_amplified_grants", top_amplified_grants)
  }
}

# classification sheets - the "what are these policy docs about?" tables
# from 05's stage 4c. read from output dir so we don't recompute
classif_dir <- here("outputs", "12e_classify_policy_docs")
if (dir.exists(classif_dir)) {
  load_classif <- function(name) {
    p <- file.path(classif_dir, name)
    if (file.exists(p)) read_csv(p, show_col_types = FALSE) else tibble()
  }
  classif_compare <- load_classif("table_03_classifications_first_vs_second.csv")
  sdg_first       <- load_classif("table_04_sdg_first_order.csv")
  sdg_second      <- load_classif("table_05_sdg_second_order.csv")
  topics_first    <- load_classif("table_06_granular_topics_first_order.csv")

  if (nrow(classif_compare) > 0) {
    addWorksheet(wb, "topics_first_vs_second_order")
    writeData(wb, "topics_first_vs_second_order", classif_compare)
  }
  if (nrow(sdg_first) > 0) {
    sdg_compare <- full_join(
      sdg_first  %>% select(sdgcategory, FO_docs = Documents),
      sdg_second %>% select(sdgcategory, SO_docs = Documents),
      by = "sdgcategory"
    ) %>%
      mutate(FO_docs = replace_na(FO_docs, 0L),
             SO_docs = replace_na(SO_docs, 0L)) %>%
      arrange(desc(FO_docs))
    addWorksheet(wb, "sdg_first_vs_second_order")
    writeData(wb, "sdg_first_vs_second_order", sdg_compare)
  }
  if (nrow(topics_first) > 0) {
    addWorksheet(wb, "granular_topics_first_order")
    writeData(wb, "granular_topics_first_order", topics_first)
  }

  # institution-type x classification cross-tab. for each institution type,
  # what share of its grants reach docs tagged with each top IPTC category?
  # this is the "do research firms reach different topics than universities"
  # check the SREE paper wants
  inst_col <- intersect(c("institution_type_final", "institution_type",
                           "ies_institution_type_from_name"),
                         names(master))[1]
  tag_cols <- grep("^n_docs_tagged_", names(master), value = TRUE)

  if (!is.na(inst_col) && length(tag_cols) > 0) {
    inst_x_topic <- master %>%
      filter(has_any_current_policy_reach) %>%
      mutate(inst_type = replace_na(.data[[inst_col]], "(unclassified)")) %>%
      group_by(inst_type) %>%
      summarize(
        n_grants_reached = n(),
        across(all_of(tag_cols),
               ~round(100 * mean(. > 0), 1),
               .names = "{str_remove(.col, '^n_docs_tagged_')}_pct"),
        .groups = "drop"
      ) %>%
      arrange(desc(n_grants_reached))

    addWorksheet(wb, "inst_type_x_topic")
    writeData(wb, "inst_type_x_topic", inst_x_topic)
  }

  # dominant-classification distribution. one-row-per-grant table with
  # the modal IPTC category and the % share - useful for "topic profile"
  # discussion in the paper
  if ("dominant_classification" %in% names(master)) {
    dom_dist <- master %>%
      filter(has_any_current_policy_reach,
             !is.na(dominant_classification)) %>%
      count(`Dominant classification` = dominant_classification,
            name = "Grants") %>%
      mutate(`% of reached grants` =
               paste0(round(100 * Grants / sum(Grants), 1), "%")) %>%
      arrange(desc(Grants))

    addWorksheet(wb, "dominant_classification_dist")
    writeData(wb, "dominant_classification_dist", dom_dist)
  }
}

out_dir <- here("outputs", "_descriptives_workbook")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(out_dir, paste0("descriptives_", format(Sys.Date()), ".xlsx"))
saveWorkbook(wb, out_path, overwrite = TRUE)

cat("Descriptive tables built.\n")
cat("  Headline n =", n_grants, "grants\n")
cat("  Broad reach:", n_reached_broad, "(",
    round(100 * n_reached_broad / n_grants, 1), "%)\n", sep = " ")
cat("  Strict reach:", n_reached_strict, "(",
    round(100 * n_reached_strict / n_grants, 1), "%)\n", sep = " ")
cat("  Workbook:", out_path, "\n")

# ============================================================================ #
# PART B - export_paper_exhibits
# ============================================================================ #

out_dir <- here("outputs", "_paper_exhibits")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# small helper to read a CSV if it exists, return an empty tibble if not
# every input is optional - the workbook just skips sheets it can't fill
read_optional <- function(rel_path) {
  full <- here(rel_path)
  if (!file.exists(full)) {
    cat(sprintf("  (missing) %s\n", rel_path))
    return(tibble())
  }
  read_csv(full, show_col_types = FALSE)
}

# wb is built up sheet by sheet. add() registers a sheet, also tracks
# what we added so the README index at the end stays in sync
wb <- createWorkbook()
index_rows <- tibble(Sheet = character(),
                     Section = character(),
                     Description = character(),
                     Rows = integer(),
                     Source = character())

add <- function(sheet, section, description, df, source) {
  if (is.null(df) || nrow(df) == 0) {
    cat(sprintf("  (skip - no data) %s\n", sheet))
    return(invisible())
  }
  addWorksheet(wb, sheet)
  writeData(wb, sheet, df)
  # auto-width all columns - tighter than openxlsx default
  setColWidths(wb, sheet, cols = seq_len(ncol(df)), widths = "auto")
  index_rows <<- bind_rows(index_rows, tibble(
    Sheet = sheet, Section = section, Description = description,
    Rows = nrow(df), Source = source
  ))
  cat(sprintf("  added: %s (%d rows)\n", sheet, nrow(df)))
}

cat("Loading inputs...\n")


#---------------------
## 1. LOAD INPUTS ##
#---------------------

# master / routes / pairs are already loaded at the top of PART A (identical
# files, read-only there), so we reuse those objects rather than re-reading the
# same three CSVs. the downstream `if (nrow(...) > 0)` guards below still hold

# stage-12 (policy-doc-level analysis)
by_src_type <- read_optional("outputs/12_build_policy_doc_overview/table_02_by_source_type.csv")
by_series   <- read_optional("outputs/12_build_policy_doc_overview/table_03_by_overton_series.csv")
by_year     <- read_optional("outputs/12_build_policy_doc_overview/table_04_by_year.csv")
top_pub     <- read_optional("outputs/12_build_policy_doc_overview/table_05_top_publishers.csv")
by_lang     <- read_optional("outputs/12_build_policy_doc_overview/table_06_by_language.csv")
top_amp     <- read_optional("outputs/12_build_policy_doc_overview/table_07_top_amplifying_intermediaries.csv")
so_by_type  <- read_optional("outputs/12_build_policy_doc_overview/table_08_second_order_by_source_type.csv")

by_country  <- read_optional("outputs/12b_build_policy_doc_geography/table_02_by_country.csv")
by_continent<- read_optional("outputs/12b_build_policy_doc_geography/table_03_by_continent.csv")
us_split    <- read_optional("outputs/12b_build_policy_doc_geography/table_05_us_federal_vs_state.csv")

# stage-12e (classifications)
classif_compare <- read_optional("outputs/12e_classify_policy_docs/table_03_classifications_first_vs_second.csv")
sdg_first       <- read_optional("outputs/12e_classify_policy_docs/table_04_sdg_first_order.csv")
sdg_second      <- read_optional("outputs/12e_classify_policy_docs/table_05_sdg_second_order.csv")
topics_first    <- read_optional("outputs/12e_classify_policy_docs/table_06_granular_topics_first_order.csv")
dom_per_grant   <- read_optional("outputs/12e_classify_policy_docs/table_10_per_grant_dominant_classification.csv")

# stage-13 (cohort + skew)
cohort_reach    <- read_optional("outputs/13_build_cohort_growth_and_skew/table_01_cohort_reach.csv")
time_to_pol     <- read_optional("outputs/13_build_cohort_growth_and_skew/table_02_cohort_time_to_policy.csv")
top_pubs_table  <- read_optional("outputs/13_build_cohort_growth_and_skew/table_03_top_publications.csv")
skew_summary    <- read_optional("outputs/13_build_cohort_growth_and_skew/table_04_grant_skew_summary.csv")
lag_cohort      <- read_optional("outputs/13_build_cohort_growth_and_skew/table_06_lag_by_cohort.csv")

# stage-09 (timing)
lag_award_pub   <- read_optional("outputs/09_build_timing_visuals/table_01_lag_award_to_publication.csv")
lag_award_pol   <- read_optional("outputs/09_build_timing_visuals/table_04_lag_award_to_policy.csv")

# stage-13c (zero-reach diagnosis)
zero_diag       <- read_optional("outputs/13c_build_zero_reach_topic_center/table_01_zero_reach_diagnosis.csv")
zero_topic      <- read_optional("outputs/13c_build_zero_reach_topic_center/table_02_zero_reach_by_topic.csv")
zero_inst       <- read_optional("outputs/13c_build_zero_reach_topic_center/table_03_zero_reach_by_institution_type.csv")
zero_cohort     <- read_optional("outputs/13c_build_zero_reach_topic_center/table_04_zero_reach_by_cohort.csv")


cat("\nBuilding sheets...\n")


#--------------------------
## 2. SECTION A: HEADLINE ##
#--------------------------

# one-screen summary - the numbers that go up front

n_grants       <- nrow(master)
n_with_pubs    <- sum(replace_na(master$n_current_dois, 0L) > 0)
n_reached      <- sum(master$has_any_current_policy_reach %in% c(TRUE, "TRUE", "true", 1))
n_distinct_doi <- if (nrow(pairs) > 0) n_distinct(pairs$doi) else NA
n_policy_docs  <- if (nrow(routes) > 0) n_distinct(routes$policy_document_id) else NA

# strict reach: only counts grants reaching a government/IGO doc
# matches the strict-reach definition in stage 08
strict_doc_ids <- if (nrow(routes) > 0)
  routes %>% filter(replace_na(policy_source_type, "") %in% c("government", "igo")) %>%
    pull(policy_document_id) %>% unique() else character()
n_reached_strict <- if (length(strict_doc_ids) > 0)
  routes %>% filter(policy_document_id %in% strict_doc_ids) %>%
    pull(grant_id) %>% n_distinct() else NA

# in our pipeline "has_doi" and "n_current_dois > 0" are the same thing
# (every DOI-bearing grant has at least one entry in the universe pairs
# table). so we report it as one row, not two confusingly-labeled ones
n_with_doi <- sum(replace_na(master$has_doi, FALSE))
fmt <- function(n) formatC(n, big.mark = ",", format = "d")

headline <- tibble(
  Metric = c(
    "Total grants in 528-grant universe",
    "Grants with at least 1 publication on file (DOI-bearing)",
    "Distinct DOIs in universe",
    "Grants reaching policy (BROAD - any Overton doc)",
    "Reach rate of DOI-bearing grants (broad)",
    "Reach rate of all 528 grants (broad)",
    "Grants reaching policy (STRICT - government / IGO only)",
    "Distinct policy documents reached (first-order)"
  ),
  Value = c(
    fmt(n_grants),
    sprintf("%s (%.1f%%)", fmt(n_with_doi), 100 * n_with_doi / n_grants),
    fmt(n_distinct_doi),
    fmt(n_reached),
    sprintf("%.1f%% (%d / %d)", 100 * n_reached / n_with_doi, n_reached, n_with_doi),
    sprintf("%.1f%% (%d / %d)", 100 * n_reached / n_grants, n_reached, n_grants),
    fmt(n_reached_strict),
    fmt(n_policy_docs)
  )
)

add("01_headline_current",
    "A. Headline",
    "One-screen summary: 528 grants, DOI coverage, reach rates, policy-doc counts.",
    headline,
    "computed here from master + routes")


# numbers diff: paper-as-submitted vs current pipeline. the SREE
# abstract used 270/51.1%; the pipeline now returns 307/58.1%. this
# sheet shows every number that shifted and why. deltas are explicit
# (not "+") so the magnitude of each shift is visible at a glance
pct_pt <- function(new, old) sprintf("+%.1f pp", new - old)

paper_vs_current <- tribble(
  ~Metric, ~`Paper (SREE submission)`, ~`Current (2026-06-03)`, ~Delta, ~`Reason for change`,
  "Total grants",
    "528",
    "528",
    "-",
    "Universe definition unchanged.",
  "Grants with >= 1 DOI",
    "441 (83.5%)",
    sprintf("%d (%.1f%%)", n_with_doi, 100 * n_with_doi / n_grants),
    sprintf("+%d (%s)", n_with_doi - 441, pct_pt(100 * n_with_doi / n_grants, 83.5)),
    "Added 6th DOI source (manual fills + saved canonical) - the r3_grant_doi_pool_augmented_with_missing.csv fix on 2026-05-30.",
  "DOIs total",
    "2,461",
    fmt(n_distinct_doi),
    sprintf("+%s", fmt(n_distinct_doi - 2461)),
    "Same 6th DOI source.",
  "Grants reaching policy (broad)",
    "270 (51.1% of all 528)",
    sprintf("%d (%.1f%% of all 528)", n_reached, 100 * n_reached / n_grants),
    sprintf("+%d (%s)", n_reached - 270, pct_pt(100 * n_reached / n_grants, 51.1)),
    "Integrated Overton 20260305 full-database dump as a fourth policy-link source (stage 00f).",
  "Reach rate (DOI-bearing denominator)",
    "61.2% (270/441)",
    sprintf("%.1f%% (%d/%d)", 100 * n_reached / n_with_doi, n_reached, n_with_doi),
    pct_pt(100 * n_reached / n_with_doi, 61.2),
    "Same as above.",
  "Distinct policy docs reached (first-order)",
    "3,305",
    fmt(n_policy_docs),
    sprintf("+%s", fmt(n_policy_docs - 3305)),
    "Same as above.",
  "Average award->policy lag",
    "13.7 years (mean), 9-19 range",
    "see lag sheets",
    "needs reconciliation",
    "The paper's 13.7 yr was computed on a different cohort restriction; the current pipeline's mean (~5-6 yr) is dominated by recent additions from the Overton 20260305 dump. Needs reconciliation before either figure is cited."
)
add("02_numbers_paper_vs_current",
    "A. Headline",
    "Side-by-side: paper-as-submitted numbers vs the current pipeline. Read this before deciding which figures to use in the paper.",
    paper_vs_current,
    "computed here from master + memory of SREE submission")


#-------------------------------
## 3. SECTION B: GRANT-LEVEL ##
#-------------------------------

# the paper's main finding bucket: institution-type and program splits
# also the "what kinds of grants reach policy" view

if (nrow(master) > 0) {

  # reach by institution_type_detail. percent reached + n grants
  # "Other" in institution_type_detail is the catch-all for grants whose
  # institution couldn't be confidently classified - relabel so the
  # reader doesn't mistake it for a meaningful category
  by_inst <- master %>%
    mutate(itype = case_when(
              is.na(institution_type_detail)       ~ "Other / unclassified",
              institution_type_detail == "Other"   ~ "Other / unclassified",
              TRUE                                  ~ institution_type_detail
           ),
           reached = has_any_current_policy_reach %in% c(TRUE, "TRUE", "true", 1)) %>%
    group_by(`Institution type` = itype) %>%
    summarize(
      `Grants in universe` = n(),
      `Reached policy`     = sum(reached),
      # %.1f%% gives a consistent decimal place (no more "50%" vs "84.7%")
      `% reached`          = sprintf("%.1f%%", 100 * mean(reached)),
      `Median docs (reached only)` = round(median(n_policy_docs[reached], na.rm = TRUE), 1),
      .groups = "drop"
    ) %>% arrange(desc(`Grants in universe`))

  add("03_reach_by_institution",
      "B. Grant-level",
      "Reach rate by institution type. Paper's main empirical finding (universities vs research firms vs other).",
      by_inst, "computed here from master")

  # award-year cohort reach
  by_cohort_full <- master %>%
    mutate(award_year = suppressWarnings(as.integer(award_year)),
           reached = has_any_current_policy_reach %in% c(TRUE, "TRUE", "true", 1)) %>%
    filter(!is.na(award_year)) %>%
    group_by(`Award year` = award_year) %>%
    summarize(
      `Grants` = n(),
      `Reached` = sum(reached),
      `% reached` = paste0(round(100 * mean(reached), 1), "%"),
      .groups = "drop"
    )
  add("04_reach_by_award_year",
      "B. Grant-level",
      "Reach rate by award year - reads the cohort/right-censoring story directly.",
      by_cohort_full, "computed here from master")

  # top 30 reached grants (with PI + institution for spotlight examples)
  top_reach <- master %>%
    filter(has_any_current_policy_reach %in% c(TRUE, "TRUE", "true", 1)) %>%
    select(grant_id, pi, institution, award_year, institution_type_detail,
           n_policy_docs, n_direct_docs, n_meta_docs, n_second_order_docs,
           amplification_factor, dominant_classification) %>%
    arrange(desc(n_policy_docs)) %>%
    head(30)
  add("05_top30_reached_grants",
      "B. Grant-level",
      "Top 30 grants by # first-order policy docs reached. Pull spotlight examples from here.",
      top_reach, "computed here from master")

  # top amplification factor (lower-n threshold to filter noise)
  if ("amplification_factor" %in% names(master)) {
    top_amp_grants <- master %>%
      filter(n_intermediate_docs >= 3) %>%
      select(grant_id, pi, institution, award_year, institution_type_detail,
             n_intermediate_docs, n_second_order_docs, amplification_factor) %>%
      arrange(desc(amplification_factor)) %>%
      head(30)
    add("06_top30_amplified_grants",
        "B. Grant-level",
        "Top 30 grants by amplification factor (downstream docs per intermediate). Filter on >= 3 intermediates to drop one-shot noise. Notable IPCC-artifact caveat - see methods.",
        top_amp_grants, "computed here from master")
  }
}

# zero-reach diagnosis (which grants didn't reach + why)
add("07_zero_reach_diagnosis",
    "B. Grant-level",
    "Bucketing of why each grant either reached or didn't. The 'Reached policy' count here is the headline.",
    zero_diag, "outputs/13c_*/table_01")

add("08_zero_reach_by_topic",
    "B. Grant-level",
    "Topic-area reach rates. Identifies which IES program areas reach policy most/least.",
    zero_topic, "outputs/13c_*/table_02")

# the underlying file's "Reached" and "% reached" columns are zero by
# construction (this is the zero-reach subset). rename the sheet and
# drop the misleading columns - this is the "where do zero-reach grants
# come from?" question, not the reach-rate view (sheet 03 covers that)
zero_inst_clean <- if (nrow(zero_inst) > 0) {
  zero_inst %>%
    select(any_of(c("Institution type", "Total grants"))) %>%
    rename(`Zero-reach grants` = `Total grants`)
} else tibble()
add("09_zero_reach_grants_by_inst",
    "B. Grant-level",
    "Of the 221 grants with zero policy reach, how many fall in each institution-type bucket. NOT a reach-rate view (see 03_reach_by_institution for that).",
    zero_inst_clean, "outputs/13c_*/table_03 (filtered to zero-reach grants only)")

add("10_zero_reach_by_cohort",
    "B. Grant-level",
    "Reach rates by 4-year award cohort. Reads with the right-censoring caveat (2018+ cohorts have had less time).",
    zero_cohort, "outputs/13c_*/table_04")


#---------------------------
## 4. SECTION C: TIMING ##
#---------------------------

# the paper has a "lag" story that needs sourced numbers

# both lag sheets have fewer rows than the universe they're computed
# over (publications: 137 rows of 479 DOI-bearing grants; policy: 305
# rows of 307 reached grants). the missing rows are grants where either
# award_year or first_pub_year / first_policy_year is unrecoverable
# this caveat goes in the sheet descriptions so 137 doesn't get read
# as "only 137 grants have publications."
add("11_lag_award_to_publication",
    "C. Timing / lag",
    sprintf("Per-grant lag from award year to first publication year. %d rows; %d of %d DOI-bearing grants have unrecoverable years (no first_pub_year on file).",
            nrow(lag_award_pub), n_with_doi - nrow(lag_award_pub), n_with_doi),
    lag_award_pub, "outputs/09_*/table_01")

add("12_lag_award_to_policy",
    "C. Timing / lag",
    sprintf("Per-grant lag from award year to first policy citation year. The headline lag stat. %d rows of the %d reached grants; %d have unrecoverable years.",
            nrow(lag_award_pol), n_reached, n_reached - nrow(lag_award_pol)),
    lag_award_pol, "outputs/09_*/table_04")

add("13_lag_by_cohort",
    "C. Timing / lag",
    "Median/mean lag by award-year cohort - shows whether lags are shrinking over time.",
    lag_cohort, "outputs/13_*/table_06")


#----------------------------------------
## 5. SECTION D: POLICY DOC CHARACTERISTICS ##
#----------------------------------------

# what do the cited policy docs look like? type, country, year, language,
# publishers. these are the descriptives the paper's "what reaches IES
# research" section will reference

add("14_docs_by_source_type",
    "D. Policy-doc characteristics",
    "First-order policy docs by source type (government / IGO / think tank). Shows the think-tank concentration that the paper foregrounds.",
    by_src_type, "outputs/12_*/table_02")

add("15_docs_by_country",
    "D. Policy-doc characteristics",
    "First-order docs by source country. Top of the list = USA, then IGO, then UK; Germany over-indexes because of IZA / IfO cluster.",
    by_country, "outputs/12b_*/table_02")

add("16_docs_by_continent",
    "D. Policy-doc characteristics",
    "Continent breakdown for first-order docs.",
    by_continent, "outputs/12b_*/table_03")

add("17_docs_by_year",
    "D. Policy-doc characteristics",
    "First-order docs by publication year. Note the 2023 cutoff (dump's effective end).",
    by_year, "outputs/12_*/table_04")

add("18_docs_by_language",
    "D. Policy-doc characteristics",
    "First-order docs by language. ~89% English.",
    by_lang, "outputs/12_*/table_06")

add("19_docs_by_series",
    "D. Policy-doc characteristics",
    "First-order docs by Overton series type. ~30% are Working papers - relevant to any limitations note about doc-type composition.",
    by_series, "outputs/12_*/table_03")

add("20_us_federal_vs_state",
    "D. Policy-doc characteristics",
    "US first-order docs split federal vs state. The state breakdown is in the underlying CSV if you need it.",
    us_split, "outputs/12b_*/table_05")

add("21_top_publishers",
    "D. Policy-doc characteristics",
    "Top 25 publishers (policy source IDs) for first-order docs. The intermediary list.",
    top_pub, "outputs/12_*/table_05")


#---------------------------------------------
## 6. SECTION E: NEW FINDINGS (post-SREE) ##
#---------------------------------------------

# second-order reach + topic classification stuff that didn't exist at
# paper submission. decide while writing whether these go in the paper
# or get held for a sequel

add("22_topics_first_vs_second",
    "E. New (post-SREE)",
    "IPTC topic mix side-by-side first vs second order. The 'think tanks as bridge' headline table - education drops, economy + politics rise.",
    classif_compare, "outputs/12e_*/table_03")

add("23_sdg_first_order",
    "E. New (post-SREE)",
    "UN SDG distribution for first-order policy docs. SDG 4 (Education) dominates at 40%.",
    sdg_first, "outputs/12e_*/table_04")

add("24_sdg_second_order",
    "E. New (post-SREE)",
    "UN SDG distribution for second-order policy docs. SDG 13 (Climate Action) is inflated - IPCC artifact.",
    sdg_second, "outputs/12e_*/table_05")

add("25_granular_topics_first_order",
    "E. New (post-SREE)",
    "Top 40 granular Overton topic tags for first-order docs. Useful keyword-style description.",
    topics_first, "outputs/12e_*/table_06")

# the underlying top_amplifiers CSV has two columns that are always
# NA in our data (the upstream policy_source_title and the parsed
# policy_published_date) plus a few titles with UTF-8 garbled from
# the dump's mixed encoding (e.g. IPCC SR15's "1.5 ÂºC â")
# strip the empty columns and best-effort fix the encoding before
# writing so the sheet is actually readable
fix_encoding <- function(x) {
  if (!is.character(x)) return(x)
  out <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
  # the common dump-mangled em-dash and degree sign
  out <- gsub("â", "—", out, fixed = TRUE)
  out <- gsub("ÂºC", "°C", out, fixed = TRUE)
  out
}
top_amp_clean <- if (nrow(top_amp) > 0) {
  top_amp %>%
    # drop columns that are entirely NA - no information in them
    select(where(~ !all(is.na(.) | . == ""))) %>%
    mutate(across(where(is.character), fix_encoding))
} else tibble()
add("26_top_amplifiers",
    "E. New (post-SREE)",
    "First-order docs ranked by second-order reach. **IPCC artifact: the top 3 rows are IPCC reports cited by exactly 1 IES grant - this is a citation-graph artifact, not evidence IES research shaped climate policy. Filter to n_grants_amplified >= 2 for a stronger signal.**",
    top_amp_clean, "outputs/12_*/table_07 (all-NA cols dropped, encoding fixed)")

add("27_second_order_by_type",
    "E. New (post-SREE)",
    "Source-type breakdown for second-order docs. Government and IGO catch up to think tanks at second order.",
    so_by_type, "outputs/12_*/table_08")

add("28_grant_dominant_classif",
    "E. New (post-SREE)",
    "Per-grant modal IPTC classification. 62% of reached grants are dominantly education; small tail of arts/health/economy-dominant grants worth flagging.",
    dom_per_grant, "outputs/12e_*/table_10")


#-----------------------------------------------
## 6b. SECTION F: WORKFLOW (chronological flow) ##
#-----------------------------------------------

# question: for each reached grant, did a think tank pick up its work
# before government did, or the other way around? this is the temporal
# sequence behind the "think tanks as bridge" framing. uses
# policy_published_year as the timestamp - the doc's own publication
# year, which is the closest proxy we have for "when did this citation
# happen."
#
# source_type comes from the routes table (one type per doc). "simultaneous"
# just means same calendar year - we don't have publication months. this is the
# first hop only, so second-order reach is excluded. grants with no parseable
# policy year drop out of the classification but still count in the universe

if (nrow(routes) > 0) {

  # 30 of 17,839 (grant, policy_doc) pairs have a policy_published_year
  # BEFORE the grant's award year - these are usually older-PI-work
  # publications mis-attributed to the grant, or policy docs whose
  # published_on field reflects an original-doc date even though the
  # citation was added in a later revision. drop them before computing
  # the temporal sequence so we don't classify a grant as "think tank
  # first" based on a 2000 citation of a 2006 grant
  year_from_grant_id <- function(gid) {
    yr2 <- suppressWarnings(as.integer(
      sub(".*[A-Z](\\d{2})\\d{4}$", "\\1", gid, perl = TRUE)))
    ifelse(is.na(yr2), NA_integer_, 2000L + yr2)
  }

  routes_temporal <- routes %>%
    mutate(award_year = year_from_grant_id(grant_id),
           policy_year = as.integer(policy_published_year)) %>%
    filter(!is.na(policy_year),
           !is.na(award_year),
           policy_year >= award_year)

  # first year a doc of each major source_type cited each grant
  grant_first_year_by_type <- routes_temporal %>%
    filter(policy_source_type %in% c("think tank", "government", "igo")) %>%
    group_by(grant_id, policy_source_type) %>%
    summarize(first_year = min(policy_year, na.rm = TRUE),
              .groups = "drop") %>%
    pivot_wider(names_from = policy_source_type,
                values_from = first_year,
                names_prefix = "first_year_") %>%
    rename(`first_year_think_tank` = `first_year_think tank`)

  # classify each grant by which type led. priority is just lexical:
  # tt vs gov is the headline comparison; igo comes in as a third lane
  # when it leads both
  workflow <- grant_first_year_by_type %>%
    mutate(
      has_tt  = !is.na(first_year_think_tank),
      has_gov = !is.na(first_year_government),
      has_igo = !is.na(first_year_igo),
      tt_minus_gov_yrs = first_year_think_tank - first_year_government,
      pathway = case_when(
        has_tt & has_gov & first_year_think_tank < first_year_government ~
          "Think tank first, then government",
        has_tt & has_gov & first_year_government < first_year_think_tank ~
          "Government first, then think tank",
        has_tt & has_gov & first_year_think_tank == first_year_government ~
          "Same year (think tank + government)",
        has_tt & !has_gov & has_igo & first_year_think_tank < first_year_igo ~
          "Think tank first, then IGO (no government)",
        has_tt & !has_gov & has_igo & first_year_igo < first_year_think_tank ~
          "IGO first, then think tank (no government)",
        has_tt & !has_gov & !has_igo ~ "Think tank only",
        !has_tt & has_gov & !has_igo ~ "Government only",
        !has_tt & !has_gov & has_igo ~ "IGO only",
        !has_tt & has_gov & has_igo & first_year_government < first_year_igo ~
          "Government first, then IGO (no think tank)",
        !has_tt & has_gov & has_igo & first_year_igo < first_year_government ~
          "IGO first, then government (no think tank)",
        TRUE ~ "Other / mixed same year"
      )
    )

  # aggregate distribution - the headline workflow finding
  workflow_distribution <- workflow %>%
    count(Pathway = pathway, name = "Grants") %>%
    mutate(`% of reached grants` =
             sprintf("%.1f%%", 100 * Grants / sum(Grants))) %>%
    arrange(desc(Grants))

  # tt-vs-gov lag summary: for grants reached by BOTH, on average how
  # many years between the first think-tank citation and the first
  # government citation?
  tt_gov_lag <- workflow %>%
    filter(has_tt & has_gov) %>%
    summarize(
      `Grants reached by both TT and government` = n(),
      `TT leads (TT year < gov year)` = sum(tt_minus_gov_yrs < 0),
      `Government leads (gov year < TT year)` = sum(tt_minus_gov_yrs > 0),
      `Same year` = sum(tt_minus_gov_yrs == 0),
      `Median gap (years, gov - TT)` = median(-tt_minus_gov_yrs),
      `Mean gap (years, gov - TT)` = round(mean(-tt_minus_gov_yrs), 1),
      `P25 gap` = quantile(-tt_minus_gov_yrs, 0.25),
      `P75 gap` = quantile(-tt_minus_gov_yrs, 0.75)
    ) %>%
    pivot_longer(everything(), names_to = "Metric", values_to = "Value")

  # workflow split by institution type (the version useful for the paper)
  workflow_by_inst <- workflow %>%
    left_join(master %>% select(grant_id, institution_type_detail),
              by = "grant_id") %>%
    mutate(itype = case_when(
      is.na(institution_type_detail)     ~ "Other / unclassified",
      institution_type_detail == "Other" ~ "Other / unclassified",
      TRUE                                ~ institution_type_detail
    )) %>%
    count(`Institution type` = itype, Pathway = pathway) %>%
    pivot_wider(names_from = Pathway, values_from = n, values_fill = 0L) %>%
    arrange(desc(rowSums(across(where(is.numeric)))))

  add("29_grant_workflow_distribution",
      "F. Workflow (post-SREE)",
      "Per-grant chronological pathway: did a think tank pick up the grant's work before government did, or the other way around? Based on policy_published_year of first-order policy docs.",
      workflow_distribution,
      "computed here from routes")

  add("30_workflow_tt_vs_gov_lag",
      "F. Workflow (post-SREE)",
      "For grants reached by BOTH think tanks and government: how many years between first TT citation and first gov citation? Negative gap means TT cited the grant earlier.",
      tt_gov_lag,
      "computed here from routes")

  add("31_workflow_by_institution",
      "F. Workflow (post-SREE)",
      "Workflow pathway split by grant institution type. Tells the 'do firms reach gov-first more often than universities?' question.",
      workflow_by_inst,
      "computed here from routes + master")

  add("32_grant_workflow_per_grant",
      "F. Workflow (post-SREE)",
      "Per-grant detail: first-year-by-type and pathway label for every reached grant. Filter to find specific examples.",
      workflow,
      "computed here from routes")

  # ---- spotlight grants: concrete examples per pathway ----
  # for each major pathway, pick 3 grants with the richest policy-doc
  # coverage and surface their full chronological citation chain. these
  # are the "here's what this actually looks like" examples the paper
  # can quote directly

  spotlight_pathways <- c(
    "Government first, then think tank",
    "Think tank first, then government",
    "Government only",
    "Think tank only",
    "Same year (think tank + government)"
  )

  spotlight_grants <- workflow %>%
    filter(pathway %in% spotlight_pathways) %>%
    left_join(master %>% select(grant_id, pi, institution,
                                institution_type_detail, n_policy_docs),
              by = "grant_id") %>%
    mutate(n_policy_docs = replace_na(n_policy_docs, 0L)) %>%
    group_by(pathway) %>%
    arrange(desc(n_policy_docs)) %>%
    slice_head(n = 3) %>%
    ungroup()

  spotlight_summary <- spotlight_grants %>%
    select(Pathway = pathway, grant_id, pi, institution, institution_type_detail,
           n_policy_docs, first_year_think_tank, first_year_government,
           first_year_igo)

  add("33_workflow_spotlight_summary",
      "F. Workflow (post-SREE)",
      "Three representative grants per pathway (picked by n_policy_docs). Shows the first year each source type cited each grant - read alongside sheet 34 for the full chain.",
      spotlight_summary,
      "computed here from workflow + master")

  # the full chronological chain for each spotlighted grant. routes
  # itself doesn't carry titles (only the doc IDs and source metadata)
  # so we join policy_doc_master from stage 05 to pick up policy_title
  policy_doc_titles <- read_optional(
    "outputs/12_build_policy_doc_overview/policy_doc_master.csv"
  ) %>%
    select(any_of(c("policy_document_id", "policy_title", "policy_source_title")))

  spotlight_chain <- routes %>%
    filter(grant_id %in% spotlight_grants$grant_id,
           !is.na(policy_published_year)) %>%
    left_join(policy_doc_titles, by = "policy_document_id") %>%
    left_join(spotlight_grants %>% select(grant_id, pathway, pi),
              by = "grant_id") %>%
    arrange(pathway, grant_id, policy_published_year, policy_source_type) %>%
    transmute(
      Pathway            = pathway,
      grant_id           = grant_id,
      PI                 = pi,
      Year               = as.integer(policy_published_year),
      `Source type`      = policy_source_type,
      Country            = policy_source_country,
      `Policy doc title` = policy_title,
      `Publisher`        = policy_source_title,
      `Policy doc URL`   = policy_document_url
    )

  add("34_workflow_spotlight_chains",
      "F. Workflow (post-SREE)",
      "Full chronological citation chain for the spotlighted grants in sheet 33. One row per policy doc, sorted by year. Pull direct quotes for the paper from here.",
      spotlight_chain,
      "computed here from workflow + routes")
}


#-----------------------------------------------
## 6c. SECTION G: OVERTON BASELINE COMPARISON ##
#-----------------------------------------------

# how concentrated is each topic in our IES-cited subset vs the full
# 10.17M-doc Overton baseline? the "lift" column is the headline:
# education shows up at 4.87x its baseline rate, the strongest single
# signal that IES research reaches the right policy literature
# generated by scripts_rewrite/11_overton_baseline_categories.R

baseline_path <- here("outputs", "_overton_baseline", "category_overview.csv")
if (file.exists(baseline_path)) {
  baseline <- read_csv(baseline_path, show_col_types = FALSE) %>%
    transmute(
      Category               = category,
      `Full-dump docs`       = full_dump_docs,
      `% of full Overton`    = sprintf("%.1f%%", full_dump_pct),
      `IES-cited docs`       = ies_cited_docs,
      `% of IES subset`      = sprintf("%.1f%%", ies_cited_pct),
      `Lift (IES vs baseline)` = sprintf("%.2fx", lift)
    )
  add("35_education_lift_vs_overton",
      "G. Baseline comparison",
      "How concentrated each IPTC category is in IES-cited docs vs the full 10.17M-doc Overton baseline. Education has 4.87x lift - over-represented by ~5x relative to the baseline. Labour 2.6x, health 2.1x also notably over; crime/law 0.24x and politics 0.71x under-represented.",
      baseline,
      "outputs/_overton_baseline/category_overview.csv (built by scripts_rewrite/11)")
}

# cascade table - "of all IES-cited docs, where do they fall when we layer
# the dominant categories one at a time?" rows sum to 100% in each column
cascade_path <- here("outputs", "_overton_baseline", "category_cascade.csv")
if (file.exists(cascade_path)) {
  cascade <- read_csv(cascade_path, show_col_types = FALSE)
  add("36_category_cascade",
      "G. Baseline comparison",
      "Cascading decomposition: All -> Education+ -> (Non-Edu) Economy+ -> (Non-Edu Non-Econ) Health+ -> Other. Every doc counted exactly once; residual is explicit. The IES column shows IES research lands disproportionately in education (48.9%) and health (16.0%) after layering away economy and prior categories.",
      cascade,
      "outputs/_overton_baseline/category_cascade.csv (built by scripts_rewrite/11)")
}


#---------------------------------------------
## 6d. SECTION H: REACH SENSITIVITY ##
#---------------------------------------------

# four alternative reach definitions + headline broad reach, with grant
# counts and deltas. shows how much the 307-grant headline moves under
# common reviewer-style restrictions

sensitivity_path <- here("outputs", "_paper_sensitivity", "sensitivity_overview.csv")
if (file.exists(sensitivity_path)) {
  sensitivity <- read_csv(sensitivity_path, show_col_types = FALSE)
  add("37_reach_sensitivity",
      "H. Reach sensitivity",
      "Headline broad reach + four alternative definitions. Multi-grant-doc exclusion is by far the most consequential restriction (-99 grants, -18.7 pp). Working-paper exclusion barely shifts the headline (-3 grants); strict reach (gov/IGO only) costs 32 grants.",
      sensitivity,
      "outputs/_paper_sensitivity/sensitivity_overview.csv (built by scripts_rewrite/12)")
}


#-----------------------------------------------
## 6e. SECTION I: GRANT-LEVEL CROSS-TABS ##
#-----------------------------------------------

# the cross-tabs the paper's "differences in grant-to-policy pipeline"
# section walks through: by grant type, by IES center, by single-vs-
# multi-grant institution. compact tables; computed inline from master

if (nrow(master) > 0) {
  m <- master %>%
    mutate(reached = has_any_current_policy_reach %in% c(TRUE, "TRUE", "true", 1))

  # grant type cross-tab. ies_project_type from the master is NA for ~270
  # grants (the post-2012 IES page layout broke our HTML parser). fall back
  # to the keyword classifier (stage 04) for any grant the IES page didn't
  # label; track which source we used so the table is auditable
  clf <- tryCatch(
    read_csv(here("outputs", "grant_classifications.csv"),
             show_col_types = FALSE) %>%
      select(grant_id, kw_classification = classification),
    error = function(e) tibble(grant_id = character(),
                                kw_classification = character())
  )

  # use kw_classification (already rolled up to Efficacy/Replication/
  # Scale-up/Effectiveness/Other by stage 04's case_when) rather than the
  # raw ies_project_type, which contains compound strings like "Efficacy,
  # Exploration" that would scatter the table across 13 rows. classifier
  # output is now complete for all 528 grants thanks to the pre-rewrite
  # snapshot fallback in 03 + 04
  by_grant_type <- m %>%
    left_join(clf, by = "grant_id") %>%
    mutate(`Grant type` = coalesce(kw_classification, "(unclassified)")) %>%
    group_by(`Grant type`) %>%
    summarize(Grants     = n(),
              Reached    = sum(reached),
              `% reached` = sprintf("%.1f%%", 100 * mean(reached)),
              `Median docs (reached only)` =
                round(median(n_policy_docs[reached], na.rm = TRUE), 1),
              .groups = "drop") %>%
    arrange(desc(Grants))

  add("38_reach_by_grant_type",
      "I. Grant-level cross-tabs",
      "Reach rate by IES research design type. Labels from stage 04's keyword classifier (which collapses compound project_type strings like 'Efficacy, Exploration' into the five headline buckets). Complete coverage of all 528 grants - the pre-rewrite snapshot fills in the 270 grants the current IES page parser misses.",
      by_grant_type,
      "computed here from master + grant_classifications.csv (stages 03 + 04)")

  # NCER vs NCSER center comparison - more detail than the basic split
  by_center <- m %>%
    mutate(Center = case_when(
      startsWith(grant_id, "R305") ~ "NCER (R305)",
      startsWith(grant_id, "R324") ~ "NCSER (R324)",
      TRUE                          ~ "Other")) %>%
    group_by(Center) %>%
    summarize(
      Grants                  = n(),
      `Mean award year`       = round(mean(suppressWarnings(as.integer(award_year)),
                                            na.rm = TRUE), 0),
      `DOIs per grant (mean)` = round(mean(replace_na(n_current_dois, 0L)), 2),
      Reached                 = sum(reached),
      `% reached`             = sprintf("%.1f%%", 100 * mean(reached)),
      `Median docs (reached only)` =
        round(median(n_policy_docs[reached], na.rm = TRUE), 1),
      `Mean docs (reached only)` =
        round(mean(n_policy_docs[reached], na.rm = TRUE), 1),
      `95th percentile docs (reached only)` =
        round(quantile(n_policy_docs[reached], 0.95, na.rm = TRUE), 0),
      .groups = "drop")

  add("39_reach_by_ies_center",
      "I. Grant-level cross-tabs",
      "NCER vs NCSER. Broad reach is nearly identical (58.8% vs 56.1%) but NCER reached grants accumulate ~2x the per-grant policy footprint (median 12 vs 6 docs; 95th percentile 414 vs 39). Consistent with a narrower specialized-policy audience for NCSER work.",
      by_center,
      "computed here from master")

  # single vs multi-grant institutions
  inst_counts <- m %>% count(institution, name = "grants_at_inst")
  by_inst_pattern <- m %>%
    left_join(inst_counts, by = "institution") %>%
    mutate(`Institution group` = case_when(
      is.na(institution)    ~ "(unclassified institution)",
      grants_at_inst == 1   ~ "Single-grant institution",
      grants_at_inst >= 2   ~ "Multi-grant institution (>=2 IES grants)",
      TRUE                  ~ "(other)")) %>%
    group_by(`Institution group`) %>%
    summarize(Grants                  = n(),
              `Distinct institutions` = n_distinct(institution),
              Reached                 = sum(reached),
              `% reached`             = sprintf("%.1f%%", 100 * mean(reached)),
              .groups = "drop") %>%
    arrange(desc(Grants))

  add("40_reach_by_institution_pattern",
      "I. Grant-level cross-tabs",
      "Single-grant vs multi-grant institutions. Once an institution is classified at all, reach rates are nearly identical (81-83%) regardless of how many IES grants it holds. The unclassified bucket reaches at 33%, a data-completeness artifact.",
      by_inst_pattern,
      "computed here from master")
}


#-------------------------------------------------------------------
## 6f. SECTION J: EMPIRICAL VS NON-EMPIRICAL PUBLICATIONS         ##
#-------------------------------------------------------------------

# section J reads the three small tables stage 15 produced. they
# stratify reach by whether the underlying DOI is an empirical RCT
# paper or a review / meta-analysis, so the headline reach figure
# can be reported with both populations and with empirical only

doi_overview_path <- here("outputs", "_paper_sensitivity",
                          "empirical_vs_nonempirical_doi_overview.csv")
if (file.exists(doi_overview_path)) {
  add("41_emp_vs_nonemp_doi_overview",
      "J. Empirical vs non-empirical",
      "Per-DOI: how many empirical (RCT-paper) vs non-empirical (review/meta-analysis) DOIs are in the universe, and what % of each reach policy.",
      read_csv(doi_overview_path, show_col_types = FALSE),
      "outputs/_paper_sensitivity/* (built by scripts_rewrite/15)")

  add("42_emp_vs_nonemp_fan_stats",
      "J. Empirical vs non-empirical",
      "Per-DOI fan stats - among DOIs that reach policy, how many policy docs cite each? Non-empirical (review/meta) DOIs typically pull much higher per-DOI policy fans than empirical RCT papers.",
      read_csv(here("outputs", "_paper_sensitivity",
                    "empirical_vs_nonempirical_fan_stats.csv"),
               show_col_types = FALSE),
      "outputs/_paper_sensitivity/* (built by scripts_rewrite/15)")

  add("43_emp_vs_nonemp_grant_status",
      "J. Empirical vs non-empirical",
      "Per-grant: does the grant reach policy through empirical work, non-empirical work, both, or neither? Lets us quantify how many grants depend on their reviews/syntheses for policy reach.",
      read_csv(here("outputs", "_paper_sensitivity",
                    "empirical_vs_nonempirical_grant_status.csv"),
               show_col_types = FALSE),
      "outputs/_paper_sensitivity/* (built by scripts_rewrite/15)")
}

#newer paper tables (stages 22/23 + the US-split region table from 05)
lattice_path <- here("outputs", "_pathway_lattice", "pathway_lattice.csv")
if (file.exists(lattice_path)) {
  add("44_pathway_lattice",
      "K. Grant-to-policy pathway lattice",
      "Results III.A Step 1: each of the 528 grants in exactly one of 9 terminal paths (furthest, most-direct outcome). Grants column partitions to 528; DOI/Meta/doc columns are per-path funnel counts.",
      read_csv(lattice_path, show_col_types = FALSE),
      "outputs/_pathway_lattice/ (built by scripts_rewrite/23)")
}

region_path <- here("outputs", "12b_build_policy_doc_geography",
                    "table_03b_by_region_us_split.csv")
if (file.exists(region_path)) {
  add("45_geography_by_region",
      "L. Geography",
      "Results III.C: first-order policy docs by region with the US split out of North America, plus government-doc counts and the distinct grants reaching each region.",
      read_csv(region_path, show_col_types = FALSE),
      "outputs/12b_build_policy_doc_geography/ (built by scripts_rewrite/05)")
}

ov_dir <- here("outputs", "22_build_doi_reach_profile")
if (file.exists(file.path(ov_dir, "overlap_matrix_gov_vs_nongov.csv"))) {
  ov <- bind_rows(
    read_csv(file.path(ov_dir, "overlap_matrix_gov_vs_nongov.csv"), show_col_types = FALSE) %>% mutate(Matrix = "Government/IGO vs non-gov"),
    read_csv(file.path(ov_dir, "overlap_matrix_usgov_vs_nonusgov.csv"), show_col_types = FALSE) %>% mutate(Matrix = "US vs non-US government"),
    read_csv(file.path(ov_dir, "overlap_matrix_federal_vs_statelocal.csv"), show_col_types = FALSE) %>% mutate(Matrix = "Federal vs State/Local")
  ) %>% select(Matrix, cell, dois, unique_grants)
  add("46_doi_overlap_matrices",
      "L. Geography",
      "Results III.C: the three 2x2 overlap matrices - distinct grant DOIs (and unique grants) by which policy arenas cite them (direct citations only).",
      ov,
      "outputs/22_build_doi_reach_profile/ (built by scripts_rewrite/22)")
}

subj_master <- here("outputs", "06_build_grant_policy_master", "table_01_grant_policy_master.csv")
if (file.exists(subj_master)) {
  subj_tab <- read_csv(subj_master, show_col_types = FALSE) %>%
    mutate(reached = has_any_current_policy_reach %in% c(TRUE, "TRUE", "true", 1)) %>%
    group_by(`Grant subject area` = grant_subject_area) %>%
    summarise(Grants = n(), Reached = sum(reached), .groups = "drop") %>%
    mutate(`Reach rate` = sprintf("%.1f%%", 100 * Reached / Grants)) %>%
    arrange(desc(Reached / Grants))
  add("47_reach_by_subject_area",
      "C. Reach by subgroup",
      "Results III.B: grant-to-policy reach by consolidated subject area (grant_subject_area). Sums to the 301 reached / 528 total.",
      subj_tab,
      "master grant_subject_area (built by scripts_rewrite/03b)")
}


#--------------------------
## 7. WRITE TOC + SAVE ##
#--------------------------

# index sheet goes first so it's the landing page on workbook open
# also written to README.md as a one-pager

addWorksheet(wb, "00_index", header = c("Paper exhibits - index", "", as.character(Sys.Date())))
writeData(wb, "00_index", index_rows %>% select(Sheet, Section, Description, Rows))
setColWidths(wb, "00_index", cols = 1:4, widths = c(35, 25, 70, 8))
# move 00_index to first position
worksheetOrder(wb) <- c(length(wb$sheet_names), seq_len(length(wb$sheet_names) - 1))

xlsx_path <- file.path(out_dir, sprintf("paper_exhibits_%s.xlsx", format(Sys.Date())))
saveWorkbook(wb, xlsx_path, overwrite = TRUE)


# also dump the index as a markdown file so it's quickly grep-able and
# pasteable into emails or notes during the writing session
readme_path <- file.path(out_dir, "README.md")
readme <- c(
  sprintf("# Paper exhibits (%s)", format(Sys.Date())),
  "",
  sprintf("Workbook: `%s`", basename(xlsx_path)),
  "",
  "One sheet per exhibit. Open in Excel/Sheets and use the `00_index` tab",
  "as a TOC. Below is the same index rendered as markdown, grouped by",
  "section.",
  ""
)
for (sec in unique(index_rows$Section)) {
  readme <- c(readme, sprintf("## %s", sec), "")
  for (i in which(index_rows$Section == sec)) {
    readme <- c(readme,
      sprintf("- **%s** (%d rows) - %s",
              index_rows$Sheet[i], index_rows$Rows[i], index_rows$Description[i]))
  }
  readme <- c(readme, "")
}
writeLines(readme, readme_path)

cat("\nWrote:\n")
cat("  ", xlsx_path, "\n")
cat("  ", readme_path, "\n")
cat("  sheets:", nrow(index_rows), "\n")
