# the pipeline's quality + documentation layer: a validation undercount audit, a
# draft-number reconciliation, and the data dictionary. the dictionary runs last
# because it documents the files everything else produces
#
#   PART A: stratified validation audit estimating how often the
#     pipeline misses publications a grant actually produced - sample 20 grants,
#     pull each PI's full OpenAlex list, flag pubs not in our universe, build the
#     review workbook. THIS PART TOUCHES THE NETWORK (OpenAlex, cached). steps
#     4-5 (manual judging) happen in the workbook
#   PART B: recomputes only the paper-draft figures that moved after
#     the June 2026 cleaning pass (covariate recovery + broadened empirical
#     filter), straight from the refreshed outputs. read-only
#   PART C: builds docs/data_dictionary.md - one markdown block per
#     CSV the pipeline reads or writes, with live schema info (column types,
#     sample values) attached to curated descriptions

#------------------
## 0. INITIALIZE ##
#------------------

library(tidyverse)
library(here)
library(curl)
library(jsonlite)
library(openxlsx)
library(data.table)

# ============================================================================ #
# PART A - validation_audit (NETWORK)
# ============================================================================ #
set.seed(20260528)   #fixed for reproducibility - same 20 grants every run

#load master + universe + OpenAlex authorships
master <- read_csv(
  here("outputs", "06_build_grant_policy_master",
       "table_01_grant_policy_master.csv"),
  show_col_types = FALSE
)
pairs <- read_csv(
  here("outputs", "02_build_current_grant_doi_universe",
       "table_01_current_grant_doi_pair_union.csv"),
  show_col_types = FALSE
) %>% mutate(doi = tolower(doi))
auth <- read_csv(
  here("outputs", "openalex_enrichment", "doi_authorships.csv"),
  show_col_types = FALSE
) %>% mutate(doi = tolower(doi))

#-----------------------------
## 1. STRATIFIED SAMPLE ##
#-----------------------------

#stratify 2 x 4: reach status x award decade. 2-3 grants per stratum,
#total 20. reached cohorts get slightly more from pre-2018 (mature
#enough to reach); zero-reach cohorts more evenly spread

m <- master %>%
  mutate(
    award_year = as.integer(award_year),
    decade = case_when(
      award_year %in% 2003:2007 ~ "2003-2007",
      award_year %in% 2008:2012 ~ "2008-2012",
      award_year %in% 2013:2017 ~ "2013-2017",
      award_year %in% 2018:2022 ~ "2018-2022",
      TRUE ~ NA_character_
    ),
    reached_broad = has_any_current_policy_reach %in% c(TRUE, "TRUE", "true", 1),
    has_pi = !is.na(pi) & nzchar(pi)
  ) %>%
  filter(!is.na(award_year), !is.na(decade), has_pi,
         grant_id %in% unique(pairs$grant_id))

#target n per stratum (matches the original 22_validation_audit allocation)
strata_targets <- tribble(
  ~reached_broad, ~decade, ~target_n,
  TRUE, "2003-2007", 3,
  TRUE, "2008-2012", 3,
  TRUE, "2013-2017", 3,
  TRUE, "2018-2022", 2,
  FALSE, "2003-2007", 2,
  FALSE, "2008-2012", 3,
  FALSE, "2013-2017", 3,
  FALSE, "2018-2022", 1
)

sampled <- m %>%
  group_by(reached_broad, decade) %>%
  group_modify(function(.x, .y) {
    tgt <- strata_targets %>%
      filter(reached_broad == .y$reached_broad, decade == .y$decade) %>%
      pull(target_n)
    if (length(tgt) == 0) tgt <- 0
    n_pick <- min(tgt, nrow(.x))
    if (n_pick == 0) tibble() else .x[sample.int(nrow(.x), n_pick), ]
  }) %>%
  ungroup() %>%
  select(grant_id, award_year, decade, reached_broad,
         pi, institution, title)

cat("Sampled", nrow(sampled), "grants across",
    n_distinct(paste(sampled$reached_broad, sampled$decade)), "strata\n")

#-----------------------------------
## 2. RESOLVE PI OPENALEX IDS ##
#-----------------------------------

#match the PI name in master against author names in the grant's existing
#authorships. when the PI is the most common author across the grant's
#DOIs, that's our author_id. ties are broken by exact-name match

normalize_name <- function(x) {
  x <- str_to_lower(replace_na(x, ""))
  x <- str_replace_all(x, "[^a-z\\s]", " ")
  x <- str_replace_all(x, "\\b[a-z]\\b", "")  #drop middle initials
  x <- str_squish(x)
  x
}

resolve_author <- function(gid, pi_name) {
  dois <- pairs %>% filter(grant_id == gid) %>% pull(doi)
  if (length(dois) == 0) return(NA_character_)
  ah <- auth %>% filter(doi %in% dois) %>%
    mutate(name_norm = normalize_name(author_name)) %>%
    filter(nchar(name_norm) > 0)
  pi_norm <- normalize_name(pi_name)

  #exact match first
  exact <- ah %>% filter(name_norm == pi_norm) %>% count(author_id, sort = TRUE)
  if (nrow(exact) > 0) return(exact$author_id[1])

  #fall back to lastname substring match
  parts <- str_split(pi_norm, " ")[[1]]
  last <- if (length(parts) > 0) parts[length(parts)] else ""
  if (nzchar(last)) {
    sub <- ah %>%
      filter(str_detect(name_norm, paste0("\\b", last, "\\b"))) %>%
      count(author_id, sort = TRUE)
    if (nrow(sub) > 0) return(sub$author_id[1])
  }
  NA_character_
}

sampled$author_id <- map2_chr(sampled$grant_id, sampled$pi, resolve_author)
cat("Resolved author_id for", sum(!is.na(sampled$author_id)),
    "of", nrow(sampled), "PIs\n")

#------------------------------------
## 3. FETCH EACH PI'S PUBLICATIONS ##
#------------------------------------

#query OpenAlex /works?filter=author.id:<id> per PI. each call returns
#up to 200 works; we follow the cursor for the prolific ones
mailto <- "your_email_here@email.com"

reconstruct_abstract <- function(aii) {
  if (is.null(aii) || length(aii) == 0) return("")
  tryCatch({
    pos <- unlist(mapply(
      function(w, p) setNames(rep(w, length(p)), as.character(p)),
      names(aii), aii, SIMPLIFY = FALSE
    ))
    paste(pos[order(as.integer(names(pos)))], collapse = " ")
  }, error = function(e) "")
}

fetch_works <- function(aid) {
  if (is.na(aid) || !nzchar(aid)) return(NULL)
  base <- paste0("https://api.openalex.org/works?filter=author.id:",
                 sub("^https?://openalex.org/", "", aid),
                 "&select=id,doi,title,publication_year,type,",
                 "primary_location,abstract_inverted_index,cited_by_count",
                 "&per-page=200&mailto=", mailto)
  results <- list(); cursor <- "*"
  for (page in 1:5) {
    url <- paste0(base, "&cursor=", cursor)
    h <- new_handle(); handle_setheaders(h, `User-Agent` = paste0("mailto:", mailto))
    resp <- tryCatch(curl_fetch_memory(url, handle = h), error = function(e) NULL)
    if (is.null(resp) || resp$status_code != 200) break
    body <- tryCatch(fromJSON(rawToChar(resp$content), simplifyVector = FALSE),
                     error = function(e) NULL)
    if (is.null(body) || length(body$results) == 0) break
    results <- c(results, body$results)
    cursor <- body$meta$next_cursor
    if (is.null(cursor) || !nzchar(cursor)) break
  }
  results
}

universe_dois <- unique(pairs$doi)
per_grant_pubs <- list()

for (i in seq_len(nrow(sampled))) {
  row <- sampled[i, ]
  cat(sprintf("  [%d/%d] %s - %s ", i, nrow(sampled), row$grant_id, row$pi))
  if (is.na(row$author_id)) {
    cat("(no author_id resolved)\n")
    per_grant_pubs[[row$grant_id]] <- tibble()
    next
  }
  works <- fetch_works(row$author_id)
  cat(sprintf("(%d works)\n", length(works)))
  if (length(works) == 0) {
    per_grant_pubs[[row$grant_id]] <- tibble()
    next
  }
  grant_dois <- pairs %>% filter(grant_id == row$grant_id) %>% pull(doi)
  pubs_df <- map(works, function(w) {
    raw_doi <- w$doi %||% ""
    doi <- tolower(sub("^https?://doi[.]org/", "", raw_doi))
    tibble(
      pub_doi = doi,
      pub_year = as.integer(w$publication_year %||% NA_integer_),
      pub_title = as.character(w$title %||% ""),
      pub_type = as.character(w$type %||% ""),
      pub_cited_by = as.integer(w$cited_by_count %||% 0L),
      pub_abstract = substr(reconstruct_abstract(w$abstract_inverted_index), 1, 600),
      in_grant_universe = doi %in% universe_dois,
      already_attributed = doi %in% grant_dois,
      manual_aligned = "",
      manual_note = ""
    )
  }) %>% bind_rows()

  # guarantee the expected schema even if every per-work tibble dropped
  # out (avoids "object 'already_attributed' not found" later)
  if (nrow(pubs_df) == 0) {
    pubs_df <- tibble(
      pub_doi = character(), pub_year = integer(), pub_title = character(),
      pub_type = character(), pub_cited_by = integer(), pub_abstract = character(),
      in_grant_universe = logical(), already_attributed = logical(),
      manual_aligned = character(), manual_note = character()
    )
  }

  #only keep pubs from the award year forward - earlier ones can't have
  #been produced by this grant
  pubs_df <- pubs_df %>%
    filter(!is.na(pub_year), pub_year >= row$award_year) %>%
    arrange(desc(already_attributed), desc(in_grant_universe), pub_year)
  per_grant_pubs[[row$grant_id]] <- pubs_df
  Sys.sleep(0.1)
}

#-------------------
## 4. WRITE WB ##
#-------------------

wb <- createWorkbook()

#README tab - protocol for the reviewer
addWorksheet(wb, "README")
readme <- c(
  "IES Grant -> Publication Validation Audit",
  paste("Generated:", format(Sys.Date())),
  "",
  "PROTOCOL",
  "  1. Open each per-grant tab (one per row in Summary).",
  "  2. Read the grant purpose / abstract at the top.",
  "  3. For each PI publication listed:",
  "       - already_attributed = TRUE  -> we caught it; no action.",
  "       - in_grant_universe = TRUE   -> attributed to a DIFFERENT IES grant.",
  "       - neither                    -> read title + abstract, decide",
  "         whether substantively aligned. Mark manual_aligned as",
  "         TRUE / FALSE / UNSURE.",
  "  4. Tally on Summary tab: undercount = aligned_but_unattributed / already_attributed."
)
writeData(wb, "README", data.frame(Instructions = readme))

#Summary tab
summary_df <- sampled %>%
  rowwise() %>%
  mutate(
    n_attributed = nrow(pairs %>% filter(grant_id == cur_data()$grant_id)),
    n_total = if (!is.null(per_grant_pubs[[grant_id]])) nrow(per_grant_pubs[[grant_id]]) else 0L,
    n_to_review = if (!is.null(per_grant_pubs[[grant_id]]) &&
                       "already_attributed" %in% names(per_grant_pubs[[grant_id]])) {
      nrow(per_grant_pubs[[grant_id]] %>% filter(!already_attributed))
    } else 0L
  ) %>%
  ungroup() %>%
  select(grant_id, pi, institution, award_year, decade, reached_broad,
         n_attributed, n_total, n_to_review)

addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_df)

#one tab per grant
for (i in seq_len(nrow(sampled))) {
  row <- sampled[i, ]
  sheet <- substr(row$grant_id, 1, 31)
  addWorksheet(wb, sheet)
  hdr <- tibble(
    Field = c("Grant ID", "PI", "Institution", "Award Year", "Reached BROAD",
              "Already-attributed DOIs", "Grant Title"),
    Value = c(row$grant_id, row$pi, row$institution, row$award_year,
              ifelse(row$reached_broad, "Yes", "No"),
              nrow(pairs %>% filter(grant_id == row$grant_id)),
              substr(replace_na(row$title, ""), 1, 1000))
  )
  writeData(wb, sheet, hdr)
  pubs <- per_grant_pubs[[row$grant_id]]
  if (!is.null(pubs) && nrow(pubs) > 0) {
    writeData(wb, sheet,
              "Reviewer columns: manual_aligned (TRUE/FALSE/UNSURE) and manual_note",
              startRow = nrow(hdr) + 2)
    writeData(wb, sheet, pubs, startRow = nrow(hdr) + 4)
  }
}

out_dir <- here("outputs", "_validation_audit")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(out_dir,
                       paste0("validation_audit_", format(Sys.Date()), ".xlsx"))
saveWorkbook(wb, out_path, overwrite = TRUE)

cat("\nWorkbook saved to:", out_path, "\n")
cat("Grants sampled:", nrow(sampled), "\n")
cat("PIs resolved:  ", sum(!is.na(sampled$author_id)), "/", nrow(sampled), "\n")
cat("Total pubs to review:",
    sum(map_int(per_grant_pubs, ~ if (is.null(.x)) 0L else nrow(.x))), "\n")

# ============================================================================ #
# PART B - reconcile_draft_numbers
# ============================================================================ #

master <- read_csv(
  here("outputs", "06_build_grant_policy_master",
       "table_01_grant_policy_master.csv"), show_col_types = FALSE)

universe <- read_csv(
  here("outputs", "02_build_current_grant_doi_universe",
       "table_02_current_grant_universe.csv"), show_col_types = FALSE)

usummary <- read_csv(
  here("outputs", "02_build_current_grant_doi_universe",
       "table_03_universe_summary.csv"), show_col_types = FALSE)

emp_overview <- read_csv(
  here("outputs", "_paper_sensitivity",
       "empirical_vs_nonempirical_doi_overview.csv"), show_col_types = FALSE)
emp_fan <- read_csv(
  here("outputs", "_paper_sensitivity",
       "empirical_vs_nonempirical_fan_stats.csv"), show_col_types = FALSE)
emp_status <- read_csv(
  here("outputs", "_paper_sensitivity",
       "empirical_vs_nonempirical_grant_status.csv"), show_col_types = FALSE)

#small helpers for tidy reporting
rate <- function(x) sprintf("%.1f%%", 100 * mean(x))
figs <- tibble(label = character(), value = character(),
               source = character())
add <- function(tbl, label, value, source)
  bind_rows(tbl, tibble(label = label, value = as.character(value), source = source))

#-------------------------------------
## 1. UNIVERSE COUNTS + DOIs/GRANT ##
#-------------------------------------

with_doi <- universe %>% filter(has_doi)
figs <- figs %>%
  add("distinct DOIs in universe",
      usummary$value[usummary$metric == "distinct DOIs (after filter)"],
      "table_03_universe_summary.csv") %>%
  add("grants with >=1 DOI",
      usummary$value[usummary$metric == "grants with at least 1 DOI"],
      "table_03_universe_summary.csv") %>%
  add("mean DOIs per grant (has_doi)", round(mean(with_doi$n_current_dois), 2),
      "table_02_current_grant_universe.csv :: n_current_dois | has_doi") %>%
  add("median DOIs per grant (has_doi)", median(with_doi$n_current_dois),
      "table_02_current_grant_universe.csv :: n_current_dois | has_doi")

#-------------------------------------
## 2. EMPIRICAL VS NON-EMPIRICAL ##
#-------------------------------------

# overview + fan stats come straight from stage 15's outputs. join them so
# the reconciliation row carries count, reach %, and per-DOI fan together
emp <- emp_overview %>%
  rename(type = Type, n_dois = `Distinct DOIs`,
         n_reached = `DOIs cited in >=1 policy doc`,
         pct_reach = `% of DOIs reaching policy`) %>%
  left_join(emp_fan %>% rename(type = Type,
                               median_fan = `Median policy docs per reached DOI`,
                               mean_fan = `Mean policy docs per reached DOI`),
            by = "type")

for (i in seq_len(nrow(emp))) {
  lab <- emp$type[i]
  figs <- figs %>%
    add(paste0(lab, " - distinct DOIs"), emp$n_dois[i],
        "empirical_vs_nonempirical_doi_overview.csv") %>%
    add(paste0(lab, " - DOIs reaching policy"), emp$n_reached[i],
        "empirical_vs_nonempirical_doi_overview.csv") %>%
    add(paste0(lab, " - % reaching policy"), emp$pct_reach[i],
        "empirical_vs_nonempirical_doi_overview.csv") %>%
    add(paste0(lab, " - median policy docs/reached DOI"), emp$median_fan[i],
        "empirical_vs_nonempirical_fan_stats.csv") %>%
    add(paste0(lab, " - mean policy docs/reached DOI"), emp$mean_fan[i],
        "empirical_vs_nonempirical_fan_stats.csv")
}

# grant-level stratified status (only-empirical / both / only-non-emp / none)
for (i in seq_len(nrow(emp_status))) {
  figs <- figs %>%
    add(paste0("grant status - ", emp_status$`Reach status`[i]),
        paste0(emp_status$Grants[i], " (", emp_status$`% of 528 grants`[i], ")"),
        "empirical_vs_nonempirical_grant_status.csv")
}

#-----------------------------------------------
## 3. INSTITUTION-TYPE-DETAIL REACH (RECOMPUTE) ##
#-----------------------------------------------

# this is the table the covariate recovery most changes: before, ~270 grants
# with no institution name defaulted into "Other", manufacturing a huge
# University-vs-Other reach gap. recompute from the now-complete master
inst_tbl <- master %>%
  group_by(institution_type_detail) %>%
  summarize(grants = n(),
            reached = sum(has_any_current_policy_reach),
            reach_rate = rate(has_any_current_policy_reach),
            median_docs_reached =
              median(n_policy_docs[has_any_current_policy_reach]),
            .groups = "drop") %>%
  arrange(desc(grants))

for (i in seq_len(nrow(inst_tbl))) {
  figs <- figs %>%
    add(paste0("inst-type ", inst_tbl$institution_type_detail[i]),
        sprintf("%d grants, %d reached (%s), median %g docs",
                inst_tbl$grants[i], inst_tbl$reached[i],
                inst_tbl$reach_rate[i], inst_tbl$median_docs_reached[i]),
        "table_01_grant_policy_master.csv :: institution_type_detail")
}

#-----------------------------------------------
## 4. SINGLE VS MULTI-GRANT INSTITUTIONS ##
#-----------------------------------------------

# count grants per institution on a normalized key (so the same org spelled
# two ways isn't split), then split single vs multi. institution is now known
# for 528/528 grants, so there's no "unclassified" bucket anymore
inst_size <- master %>%
  filter(!is.na(institution)) %>%
  mutate(k = str_squish(str_to_upper(institution))) %>%
  count(k, name = "grants_at_inst")

multi_tbl <- master %>%
  mutate(k = str_squish(str_to_upper(institution))) %>%
  left_join(inst_size, by = "k") %>%
  mutate(grp = if_else(grants_at_inst >= 2,
                       "Multi-grant institution (>=2)", "Single-grant institution")) %>%
  group_by(grp) %>%
  summarize(grants = n(), distinct_institutions = n_distinct(k),
            reached = sum(has_any_current_policy_reach),
            reach_rate = rate(has_any_current_policy_reach), .groups = "drop")

for (i in seq_len(nrow(multi_tbl))) {
  figs <- figs %>%
    add(multi_tbl$grp[i],
        sprintf("%d grants across %d institutions, %d reached (%s)",
                multi_tbl$grants[i], multi_tbl$distinct_institutions[i],
                multi_tbl$reached[i], multi_tbl$reach_rate[i]),
        "table_01_grant_policy_master.csv :: institution")
}

#-----------------------------------------------
## 5. CENTER (NCER/NCSER) + TOPIC AREA REACH ##
#-----------------------------------------------

center_tbl <- master %>%
  group_by(center) %>%
  summarize(grants = n(), reached = sum(has_any_current_policy_reach),
            reach_rate = rate(has_any_current_policy_reach),
            dois_per_grant = round(mean(n_current_dois), 2), .groups = "drop")
for (i in seq_len(nrow(center_tbl)))
  figs <- figs %>%
    add(paste0("center ", center_tbl$center[i]),
        sprintf("%d grants, %d reached (%s), %g DOIs/grant",
                center_tbl$grants[i], center_tbl$reached[i],
                center_tbl$reach_rate[i], center_tbl$dois_per_grant[i]),
        "table_01_grant_policy_master.csv :: center")

topic_tbl <- master %>%
  group_by(topic_area) %>%
  summarize(grants = n(), reached = sum(has_any_current_policy_reach),
            reach_rate = rate(has_any_current_policy_reach), .groups = "drop") %>%
  arrange(desc(grants))
for (i in seq_len(nrow(topic_tbl)))
  figs <- figs %>%
    add(paste0("topic ", topic_tbl$topic_area[i]),
        sprintf("%d grants, %d reached (%s)",
                topic_tbl$grants[i], topic_tbl$reached[i], topic_tbl$reach_rate[i]),
        "table_01_grant_policy_master.csv :: topic_area")

#-----------------------------------------------
## 6. OVERTON CATEGORY DISTRIBUTION (STANDARDIZE) ##
#-----------------------------------------------

# the draft carries three slightly different education figures (48.9 / 49.74 /
# 50.68%); standardize on the current baseline table
cat_path <- here("outputs", "_overton_baseline", "category_overview.csv")
if (file.exists(cat_path)) {
  cats <- read_csv(cat_path, show_col_types = FALSE)
  print(cats)
  ies_pct_col <- intersect(c("ies_cited_pct", "ies_pct", "IES_cited_pct"), names(cats))
  lift_col    <- intersect(c("lift"), names(cats))
}

#----------------------
## 7. WRITE + REPORT ##
#----------------------

out_dir <- here("outputs", "_paper_reconciliation")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(figs, file.path(out_dir, "reconciled_figures.csv"))

cat("\n=== reconciled figures (", nrow(figs), "rows) ===\n", sep = "")
print(figs, n = 100)
cat("\n--- institution_type_detail reach (full) ---\n"); print(inst_tbl, n = 20)
cat("\n--- single vs multi-grant ---\n"); print(multi_tbl)
cat("\n--- center ---\n"); print(center_tbl)
cat("\n--- topic area ---\n"); print(topic_tbl, n = 20)
cat("\nWrote", file.path(out_dir, "reconciled_figures.csv"), "\n")

# ============================================================================ #
# PART C - build_data_dictionary
# ============================================================================ #

# counting rows in the multi-GB raw dump files takes ~30 s per file,
# which adds up to a few minutes every time we regenerate the
# dictionary. so we save the counts to a little sidecar CSV - the first
# run computes them, every run after that just reads the cache. each
# cached count is tagged with the source file's modification time, so
# if anyone replaces the dump (or 05 rewrites a derived table) the
# affected entries get recomputed automatically; everything else stays
# cached
cache_path <- here("data", "_rewrite_outputs", "_rowcount_cache.csv")

# load whatever's already cached. empty tibble on first run
rowcount_cache <- if (file.exists(cache_path)) {
  read_csv(cache_path, show_col_types = FALSE)
} else {
  tibble(rel_path = character(),
         n_rows = integer(),
         source_mtime = as.POSIXct(character()))
}


#---------------------
## 1. SMALL HELPERS ##
#---------------------

# how many data rows does this file have? consults the on-disk cache
# returns the cached count if the source file hasn't been modified
# since the cache was written; otherwise counts the lines in the file
# and updates the cache. the cache gets persisted at the end of the
# script (see section 10). we subtract 1 from the line count because
# the header line isn't a data row
count_rows <- function(rel_path) {
  full_path <- here(rel_path)
  if (!file.exists(full_path)) return(NA_integer_)

  source_mtime <- file.info(full_path)$mtime
  cached <- rowcount_cache %>% filter(rel_path == !!rel_path)
  if (nrow(cached) == 1 && !is.na(cached$source_mtime) &&
      cached$source_mtime >= source_mtime) {
    return(as.integer(cached$n_rows))
  }

  # cache miss (file not seen before) or stale (file's been modified
  # since we cached its count). count the lines now and update the
  # in-memory cache; the cache file gets re-written in section 10
  # the <<- is a deliberate exception to "no global mutation" - this
  # function is a memoization cache and updating shared state is what
  # caches do. confining the mutation to count_rows() keeps it bounded
  cat(sprintf("  counting rows in %s ...\n", rel_path))
  n_lines <- as.integer(system(sprintf("wc -l < %s", shQuote(full_path)),
                               intern = TRUE))
  n_rows <- n_lines - 1L

  rowcount_cache <<- rowcount_cache %>%
    filter(rel_path != !!rel_path) %>%
    bind_rows(tibble(rel_path = rel_path,
                     n_rows = n_rows,
                     source_mtime = source_mtime))
  n_rows
}

# read just the first 2000 rows. enough to spot column types and grab
# example values. fread caps at nrows so even 11 GB topics.csv comes
# back in under a second
peek <- function(rel_path, n_sample = 2000) {
  full_path <- here(rel_path)
  if (!file.exists(full_path)) return(NULL)
  tryCatch(
    fread(full_path, nrows = n_sample, encoding = "UTF-8") %>% as_tibble(),
    error = function(e) NULL
  )
}

# format file size with human units. "GB" for big dump files, "KB" for
# tiny summary tables. nothing fancy
format_size <- function(n_bytes) {
  if (is.na(n_bytes) || n_bytes < 0) return("?")
  units <- c("B", "KB", "MB", "GB", "TB")
  i <- 1
  while (n_bytes >= 1024 && i < length(units)) {
    n_bytes <- n_bytes / 1024
    i <- i + 1
  }
  sprintf("%.1f %s", n_bytes, units[i])
}

# format an integer with thousands separators. mostly so 10167033 reads
# as 10,167,033 in the markdown header line
format_int <- function(n) {
  if (is.na(n)) return("?")
  formatC(n, big.mark = ",", format = "d")
}

# turn the first real value from a column into something printable
# defensive about Date and factor columns - the first version of this
# crashed when nchar() hit an NA pulled from a Date
sample_value <- function(x) {
  if (length(x) == 0) return("")
  vals <- tryCatch(as.character(x), error = function(e) character())
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) == 0) return("")
  out <- vals[1]
  if (nchar(out) > 80) out <- paste0(substr(out, 1, 77), "...")
  out
}


#-----------------------------
## 2. SCHEMA TABLE RENDERER ##
#-----------------------------

# build the markdown "Schema" table for one file. one row per column
# `cols` is an optional named vector mapping column name to a curated
# description. columns the sample has but cols doesn't get an empty
# description; columns cols has but the sample doesn't (because we ran
# out of rows in the peek, or the file is missing) get a flagged row at
# the bottom so the descriptions still show up
schema_table <- function(rel_path, cols = NULL) {
  df <- peek(rel_path)
  if (is.null(df) || nrow(df) == 0)
    return("(could not introspect)")

  # the table header. same five columns for every file
  header <- c(
    "| Column | Type | Description | Sample distinct | Sample value |",
    "|---|---|---|---:|---|"
  )

  # one row per column actually present in the sample
  rows_from_sample <- character()
  for (col in names(df)) {
    desc <- if (!is.null(cols) && col %in% names(cols)) cols[[col]] else ""
    type <- class(df[[col]])[1]
    n_distinct_in_sample <- n_distinct(df[[col]])
    val <- sample_value(df[[col]])
    # escape pipe characters so they don't break the markdown table
    val <- gsub("|", "\\|", val, fixed = TRUE)
    rows_from_sample <- c(rows_from_sample,
      sprintf("| `%s` | %s | %s | %s | %s |",
              col, type, desc, format_int(n_distinct_in_sample), val))
  }

  # any curated columns the sample didn't surface. these get a
  # "(curated)" marker instead of a real type, so the reader can tell
  # the description is hand-written but no live data backed it up
  rows_from_curation_only <- character()
  if (!is.null(cols)) {
    missing_cols <- setdiff(names(cols), names(df))
    for (col in missing_cols) {
      rows_from_curation_only <- c(rows_from_curation_only,
        sprintf("| `%s` | _(curated)_ | %s | | |", col, cols[[col]]))
    }
  }

  c(header, rows_from_sample, rows_from_curation_only)
}


#-----------------------------
## 3. ONE-FILE BLOCK RENDER ##
#-----------------------------

# turn everything we know about one file into a markdown block
# returns a character vector of lines (we glue these together at the
# end). every layer-section in this script just keeps calling block()
block <- function(path, blurb, producer, consumers, cols = NULL) {
  full_path <- here(path)
  if (file.exists(full_path)) {
    size_str <- format_size(file.info(full_path)$size)
    rows_str <- format_int(count_rows(path))
  } else {
    size_str <- "MISSING"
    rows_str <- "MISSING"
  }
  consumers_str <- if (length(consumers) == 0) "(none)"
                   else paste(consumers, collapse = ", ")

  c(
    sprintf("### `%s`", path),
    "",
    sprintf("**Rows**: %s   |   **Size**: %s", rows_str, size_str),
    "",
    blurb,
    "",
    sprintf("**Produced by**: %s", producer),
    sprintf("**Consumed by**: %s", consumers_str),
    "",
    "**Schema**",
    "",
    schema_table(path, cols),
    ""
  )
}


#-------------------------
## 4. START THE OUTPUT ##
#-------------------------

# we build the whole markdown doc as one big character vector and
# writeLines() it at the end. doing it this way (instead of cat()'ing
# to a connection as we go) means we only ever see a complete file on
# disk - never a partial one if something errors halfway through
md <- character()

# also track which files we've described by hand. the appendix walks
# the stage-output directories looking for anything we MISSED, so it
# needs this list to know what to skip. updated explicitly after each
# block() call - no global side effects
documented <- character()


#---------------------
## 5. PREAMBLE ##
#---------------------

md <- c(md,
  "# Data dictionary",
  "",
  sprintf("Generated %s by `scripts_rewrite/99_build_data_dictionary.R`.",
          Sys.Date()),
  "",
  "Three layers, top-to-bottom from raw inputs to derived analytic tables.",
  "Edit the descriptions in the producing script and re-run to regenerate;",
  "schema info (types, sample values, distinct counts) is read live from",
  "the files themselves.",
  "",
  "- **Layer 1** is the upstream Overton dump - we don't modify it.",
  "- **Layer 2** is what `00f`/`00g`/`00h` extract from the dump and cache.",
  "- **Layer 3** is what `01`-`08` write.",
  "",
  "The grant-level master has its own column-level dictionary at",
  "`docs/grant_master_data_dictionary.csv` - that's the source of truth for",
  "the ~100 master columns. We document only the headline ones below.",
  ""
)


#----------------------------------
## 6. LAYER 1 - OVERTON RAW DUMP ##
#----------------------------------

# seven files in Overton_20260305/. they're all long-format CSVs keyed
# on policy_document_id. we don't modify the dump itself - everything
# downstream filters subsets of these out

md <- c(md, "## Layer 1 - Overton 20260305 raw dump", "")

# a short orientation note linking our column names to Overton's official
# export terminology, plus a "what's missing" list. when someone shows
# up referencing Overton's docs ("where's the snippet column?") this is
# where they'll find the answer
md <- c(md,
  "Our dump came out of the JSON-database equivalent of Overton's Excel",
  "exports. Most of the columns below correspond directly to fields in",
  "Overton's documented Excel/CSV export schema, so this is a good place",
  "to start when cross-referencing their docs:",
  "",
  "- `policy_document_id` <-> Overton ID",
  "- `policy_source_id` <-> Source ID",
  "- `title` <-> Title (no Translated Title column - English-only here)",
  "- `published_on` <-> Published",
  "- `policy_source_type` <-> Source Organisation Type (government / igo / think tank)",
  "- `policy_source_country` <-> Source country (IGO and EU are treated as countries)",
  "- `overton_policy_document_series` <-> Policy document type",
  "- `policy_document_url` <-> URL",
  "- `language` <-> Languages",
  "- `df_policy_to_doi.doi` <-> Policy document DOIs / Matched DOI",
  "- `df_policy_topics.topic` <-> Top topics",
  "- `df_policy_sdgcategories.sdgcategory` <-> Related to SDGs",
  "- `df_policy_to_policy.cited_policy_document_id` <-> Matched Policy Document ID",
  "",
  "**Fields Overton exposes but our dump does NOT carry**, in case any",
  "of these would be useful and we want to ask for a richer pull:",
  "",
  "- `Snippet` (the doc's abstract / opening paragraph). The upstream",
  "  notebook reads this field from the JSON but never writes it to CSV.",
  "- `AI Generated Document Description` (a longer descriptive summary).",
  "- `Source Sector` (public / private / third sector). Coarser than",
  "  `policy_source_type` but useful for some splits.",
  "- `Source Function` (the specific function of the source - e.g.,",
  "  legislative committee vs executive department).",
  "- `Matched snippet` (the actual text surrounding a citation in the",
  "  policy doc - the strongest evidence that IES research was used,",
  "  not just listed). Would be a substantial upgrade for the paper.",
  "- `Page` (page number where the citation appears).",
  "- `Matched Reference Type` (distinguishes a citation from a person",
  "  mention).",
  "- `Citations (same source)` (lets you exclude self-citations from a",
  "  policy source).",
  "- `Translated Title` (English translation for non-English titles).",
  ""
)

md <- c(md, block(
  path = "Overton_20260305/df_policy_doc_info.csv",
  blurb = "One row per Overton policy document (~10.2M rows). Carries the doc's title, source, publication date, country, and series. Joined to the other dump files via `policy_document_id`.",
  producer = "Overton 2026-03-05 full-database dump, extracted from `kellogg/*.json` by `process_overton_dump_to_df.ipynb`.",
  consumers = c("00f", "00g (metadata for second-order docs)", "05 (via filtered subset)"),
  cols = c(
    policy_document_id    = "Overton's unique policy document ID. Format `<source>-<32-char-hash>`. Stable within a dump; not guaranteed stable across dumps.",
    policy_source_id      = "Slug for the publishing source/institution (e.g. `unitednations`, `worldbank`, `nber`, `izade`). Maps to Overton's 'Source ID'.",
    title                 = "Document title as Overton extracted it. Free text; may include quotes/commas (CSV-quoted). Note: Overton also exposes a 'Translated Title' for non-English docs, but our dump doesn't include it.",
    authors               = "**KNOWN BAD** - the upstream notebook hard-coded every row's authors to the first record's value (`['Pioneer Institute']`). Do not use.",
    published_on          = "Publication date as YYYY-MM-DD. Maps to Overton's 'Published'. Coverage falls off a cliff after 2023 even though the dump is dated 20260305.",
    policy_source_type    = "Maps to Overton's 'Source Organisation Type'. One of `government`, `igo`, `think tank`, `other`. Used heavily in reach analyses.",
    policy_source_country = "Maps to Overton's 'Source country'. Country name or `IGO`/`EU` for supranational orgs (Overton treats these as countries).",
    overton_policy_document_series = "Maps to Overton's 'Policy document type'. Values like `Publication`, `Working paper`, `Blog post`, `Clinical guidance`.",
    policy_document_url   = "Maps to Overton's 'URL' - the canonical web address of the doc.",
    language              = "Maps to Overton's 'Languages'. ISO 639-2 three-letter code (`eng`, `spa`, `fre`, ...). 88.8% English in IES-cited docs."
  )
))
documented <- c(documented, "Overton_20260305/df_policy_doc_info.csv")

md <- c(md, block(
  path = "Overton_20260305/df_policy_doc_links.csv",
  blurb = "PDF location for each policy doc (~10.2M rows). Currently unused by the pipeline but useful for follow-up PDF retrieval.",
  producer = "Same upstream notebook as `df_policy_doc_info`.",
  consumers = c("(none currently)"),
  cols = c(
    policy_document_id = "Overton policy doc ID. Joins to `df_policy_doc_info`.",
    pdf_url            = "Direct PDF URL when Overton found one.",
    pdf_document_id    = "Overton's internal PDF ID; same `<source>-<hash>` shape with the hash appended again."
  )
))
documented <- c(documented, "Overton_20260305/df_policy_doc_links.csv")

md <- c(md, block(
  path = "Overton_20260305/df_policy_to_doi.csv",
  blurb = "Long-format table of DOIs cited in each policy doc (~16.4M rows). **The primary join key for the pipeline** - this is how grant DOIs find their way to policy docs. Equivalent to Overton's 'Matched DOI' column on the 'Matched References' export tab, except we only have the DOI itself - none of the surrounding context fields (Page, Matched snippet, Matched Reference Type, etc.) made it into the JSON dump.",
  producer = "Same upstream notebook; extracted from each policy doc's `dois_cited` array.",
  consumers = c("00f (filters down to IES universe matches)"),
  cols = c(
    policy_document_id = "Citing policy doc.",
    doi                = "Cited DOI. Maps to Overton's 'Matched DOI'. Mixed case in source; normalized to lowercase and stripped of `https?://doi.org/` and `doi:` prefixes by 00f's `norm_doi()`."
  )
))
documented <- c(documented, "Overton_20260305/df_policy_to_doi.csv")

md <- c(md, block(
  path = "Overton_20260305/df_policy_to_policy.csv",
  blurb = "Long-format policy-doc -> policy-doc citations (~5.3M edges). One row per (citing_doc, cited_doc). Used to compute second-order reach in stage 02 section 4b. Overton's exports expose a 'Citations' count per doc; this table is the underlying edge list those counts roll up from.",
  producer = "Same upstream notebook; extracted from each policy doc's `policy_document_ids_cited` array.",
  consumers = c("00g (filters edges where cited side is a first-order IES doc)"),
  cols = c(
    policy_document_id       = "Citing doc. At second order, this is the *new* doc we credit IES research with reaching.",
    cited_policy_document_id = "Cited doc. Maps to Overton's 'Matched Policy Document ID'. In the 00g filter, this side is restricted to the first-order IES set."
  )
))
documented <- c(documented, "Overton_20260305/df_policy_to_policy.csv")

md <- c(md, block(
  path = "Overton_20260305/df_policy_classifications.csv",
  blurb = "Long-format IPTC top-level media topic tags (~66.9M rows). 17 top-level categories (education, health, economy/business/finance, etc.) with hierarchical sub-tags like `education>school>higher education`.",
  producer = "Same upstream notebook; extracted from each policy doc's `classifications` array.",
  consumers = c("00h (filters down to IES-relevant docs)"),
  cols = c(
    policy_document_id = "Policy doc this classification applies to.",
    classification     = "Hierarchical IPTC tag. 05 collapses to top-level by splitting on `>`."
  )
))
documented <- c(documented, "Overton_20260305/df_policy_classifications.csv")

md <- c(md, block(
  path = "Overton_20260305/df_policy_sdgcategories.csv",
  blurb = "Long-format UN Sustainable Development Goal tags (~17.5M rows). Includes top-level SDGs (`SDG 4: Quality Education`) and sub-targets (`SDG Target 4.7`).",
  producer = "Same upstream notebook; extracted from each policy doc's `sdgcategories` array.",
  consumers = c("00h"),
  cols = c(
    policy_document_id = "Policy doc this SDG tag applies to.",
    sdgcategory        = "SDG label. 05 filters to top-level only (`SDG \\d+:` prefix) for headline tables."
  )
))
documented <- c(documented, "Overton_20260305/df_policy_sdgcategories.csv")

md <- c(md, block(
  path = "Overton_20260305/df_policy_topics.csv",
  blurb = "Long-format granular topic tags (~173.9M rows, 11 GB). Thousands of distinct topic strings - useful for keyword analysis but too noisy for headline breakdowns.",
  producer = "Same upstream notebook; extracted from each policy doc's `topics` array.",
  consumers = c("00h"),
  cols = c(
    policy_document_id = "Policy doc this topic applies to.",
    topic              = "Free-form topic string. Examples: `Education`, `Teacher`, `Cognition`, `Poverty`, `Regression analysis`, `Almshouse`."
  )
))
documented <- c(documented, "Overton_20260305/df_policy_topics.csv")


#---------------------------------------
## 7. LAYER 2 - PIPELINE DERIVATIVES ##
#---------------------------------------

# files in data/_rewrite_outputs/. these are the 00f/00g/00h extracts
# of the raw dump, plus the universe anchor from 01. small (MB-sized)
# and live-loadable, unlike the multi-GB raw files

md <- c(md, "## Layer 2 - pipeline derivatives (`data/_rewrite_outputs/`)", "")

md <- c(md, block(
  path = "data/_rewrite_outputs/grant_universe_528_anchor.csv",
  blurb = "The canonical 528-grant analytic frame. Every downstream stage joins back to this.",
  producer = "`01_build_grant_universe.R`, from the original IES grant ID list.",
  consumers = c("01 (writes)", "02-09 (read for filtering)"),
  cols = c(grant_id = "IES award number, e.g. `R305A170250`. Cleaned (uppercase, trimmed).")
))
documented <- c(documented, "data/_rewrite_outputs/grant_universe_528_anchor.csv")

md <- c(md, block(
  path = "data/_rewrite_outputs/overton_full_policy_links.csv",
  blurb = "Policy doc -> universe DOI matches from the Overton 20260305 full dump. ~12k rows, ~8.5k policy docs, ~960 DOIs. Source `(a)` in 02's policy-link union.",
  producer = "`00f_extract_overton_full_dump.R`, filtering `df_policy_to_doi` down to IES universe DOIs.",
  consumers = c("02 (links_overton_full)"),
  cols = c(
    policy_document_id = "First-order policy doc (cites at least one IES DOI).",
    doi                = "Normalized DOI from the IES universe that this doc cites."
  )
))
documented <- c(documented, "data/_rewrite_outputs/overton_full_policy_links.csv")

md <- c(md, block(
  path = "data/_rewrite_outputs/overton_full_policy_docs.csv",
  blurb = "Metadata for each policy doc in `overton_full_policy_links.csv`. Same schema as Overton's `df_policy_doc_info`, minus the corrupted `authors` column.",
  producer = "`00f_extract_overton_full_dump.R`, filtering `df_policy_doc_info` to matched IDs.",
  consumers = c("02 (docs_overton_full, becomes part of policy_docs union)"),
  cols = c(
    policy_document_id    = "First-order doc ID. Joins to `overton_full_policy_links`.",
    title                 = "As in raw dump.",
    policy_source_id      = "As in raw dump.",
    policy_source_type    = "As in raw dump.",
    policy_source_country = "As in raw dump.",
    published_on          = "As in raw dump.",
    overton_policy_document_series = "As in raw dump.",
    policy_document_url   = "As in raw dump.",
    language              = "As in raw dump."
  )
))
documented <- c(documented, "data/_rewrite_outputs/overton_full_policy_docs.csv")

md <- c(md, block(
  path = "data/_rewrite_outputs/overton_full_p2p_edges.csv",
  blurb = "Filtered P2P edges where the cited side is a first-order IES doc. ~59k edges. Drives the second-order reach analysis in 02 section 4b.",
  producer = "`00g_extract_overton_p2p_edges.R`, filtering `df_policy_to_policy`.",
  consumers = c("02 (second-order route construction)"),
  cols = c(
    policy_document_id       = "Citing doc (= second-order).",
    cited_policy_document_id = "First-order doc being cited."
  )
))
documented <- c(documented, "data/_rewrite_outputs/overton_full_p2p_edges.csv")

md <- c(md, block(
  path = "data/_rewrite_outputs/overton_full_second_order_docs.csv",
  blurb = "Metadata for second-order docs not already in `overton_full_policy_docs`. ~38k rows.",
  producer = "`00g_extract_overton_p2p_edges.R`.",
  consumers = c("02 (joined into policy_docs_all for second-order links)"),
  cols = c(
    policy_document_id    = "Second-order doc ID.",
    title                 = "As in raw dump (with the authors-bug field dropped before write).",
    policy_source_id      = "As in raw dump.",
    policy_source_type    = "As in raw dump.",
    policy_source_country = "As in raw dump.",
    published_on          = "As in raw dump.",
    overton_policy_document_series = "As in raw dump.",
    policy_document_url   = "As in raw dump.",
    language              = "As in raw dump."
  )
))
documented <- c(documented, "data/_rewrite_outputs/overton_full_second_order_docs.csv")

md <- c(md, block(
  path = "data/_rewrite_outputs/overton_full_classifications.csv",
  blurb = "IPTC top-level classification tags for IES-relevant docs (first + second order). ~331k rows for ~42.5k docs.",
  producer = "`00h_extract_overton_classifications.R`.",
  consumers = c("05 (section 4c)"),
  cols = c(
    policy_document_id = "IES-relevant doc.",
    classification     = "Hierarchical IPTC tag. Use `str_split(.,'>')[[1]][1]` to get top-level."
  )
))
documented <- c(documented, "data/_rewrite_outputs/overton_full_classifications.csv")

md <- c(md, block(
  path = "data/_rewrite_outputs/overton_full_sdg.csv",
  blurb = "UN SDG tags for IES-relevant docs. ~116k rows for ~38.5k docs.",
  producer = "`00h_extract_overton_classifications.R`.",
  consumers = c("05 (section 4c)"),
  cols = c(
    policy_document_id = "IES-relevant doc.",
    sdgcategory        = "SDG label (top-level + sub-targets mixed)."
  )
))
documented <- c(documented, "data/_rewrite_outputs/overton_full_sdg.csv")

md <- c(md, block(
  path = "data/_rewrite_outputs/overton_full_topics.csv",
  blurb = "Granular Overton topic tags for IES-relevant docs. ~1.9M rows for ~42.5k docs.",
  producer = "`00h_extract_overton_classifications.R`.",
  consumers = c("05 (section 4c)"),
  cols = c(
    policy_document_id = "IES-relevant doc.",
    topic              = "Free-form Overton topic string."
  )
))
documented <- c(documented, "data/_rewrite_outputs/overton_full_topics.csv")


#----------------------------------
## 8. LAYER 3 - PIPELINE OUTPUTS ##
#----------------------------------

# files in outputs/0X_*/. these are the analytic tables produced by
# the rewrite's 01-08 stages. we describe the headline tables by hand;
# secondary tables get auto-summarized in the appendix below

md <- c(md, "## Layer 3 - pipeline outputs (`outputs/`)", "")

md <- c(md, block(
  path = "outputs/02_build_current_grant_doi_universe/table_01_current_grant_doi_pair_union.csv",
  blurb = "The grant <-> DOI universe. ~3.1k rows. Every (grant_id, DOI) pair we believe in, with provenance flags showing which source(s) contributed it. The single input that every downstream stage filters/joins against.",
  producer = "`01_build_grant_universe.R`, unioning the original, ERIC R3, manual fills, and Crossref-recovered sources.",
  consumers = c("02 (pairs)", "00d", "00f"),
  cols = c(
    grant_id      = "Cleaned IES award number.",
    doi           = "Normalized DOI.",
    src_original  = "Pair appeared in the original baseline DOI list.",
    src_eric_r3   = "Pair came from the ERIC R3 evaluation matches (00a).",
    src_manual    = "Pair came from a manual fill (Lydia's missing-DOI sheet).",
    src_canonical = "Pair was in a saved canonical snapshot.",
    src_crossref  = "Pair came from Crossref bibliographic recovery (00e)."
  )
))
documented <- c(documented, "outputs/02_build_current_grant_doi_universe/table_01_current_grant_doi_pair_union.csv")

md <- c(md, block(
  path = "outputs/02_build_current_grant_doi_universe/table_02_current_grant_universe.csv",
  blurb = "One row per grant in the 528 universe, enriched with baseline PI/institution/year/program metadata. The starting point for the master in stage 03.",
  producer = "`01_build_grant_universe.R`.",
  consumers = c("03 (grant_universe)"),
  cols = c(
    grant_id              = "Cleaned IES award number.",
    baseline_pi           = "PI name from the original IES sheet.",
    baseline_institution  = "Awardee institution.",
    baseline_award_year   = "Award year.",
    baseline_program_name = "IES program (e.g. `Education Research Grants`).",
    baseline_title        = "Award title.",
    baseline_goal_text    = "Stated goal of the award (used as input to keyword classifiers)."
  )
))
documented <- c(documented, "outputs/02_build_current_grant_doi_universe/table_02_current_grant_universe.csv")

md <- c(md, block(
  path = "outputs/05_rebuild_current_policy_routes/table_01_current_direct_policy_links.csv",
  blurb = "Long table of direct grant -> policy-doc routes (grant cites a DOI that a policy doc also cites). ~17k rows.",
  producer = "`02_link_grants_to_publications.R` section 2.",
  consumers = c("05 (policy doc master)", "06"),
  cols = c(
    grant_id              = "Grant.",
    doi                   = "The grant's publication DOI that the policy doc cited.",
    policy_document_id    = "Policy doc.",
    policy_title          = "From the docs union.",
    policy_source_type    = "From the docs union.",
    policy_source_country = "From the docs union.",
    policy_published_year = "From the docs union.",
    policy_document_url   = "From the docs union."
  )
))
documented <- c(documented, "outputs/05_rebuild_current_policy_routes/table_01_current_direct_policy_links.csv")

md <- c(md, block(
  path = "outputs/05_rebuild_current_policy_routes/table_07_current_grant_policy_doc_routes.csv",
  blurb = "**The headline routes table.** One row per (grant, policy_doc) with route flags. Every downstream reach analysis reads this.",
  producer = "`02_link_grants_to_publications.R` section 4.",
  consumers = c("05", "06", "08", "all dashboard / paper analyses"),
  cols = c(
    grant_id              = "Grant.",
    policy_document_id    = "Policy doc the grant reaches.",
    has_direct_path       = "Reached via direct DOI citation.",
    has_meta_path         = "Reached via meta-analysis route.",
    policy_source_type    = "government / igo / think tank / other.",
    policy_source_country = "ISO country or `IGO`.",
    policy_published_year = "Doc publication year.",
    policy_document_url   = "Canonical URL."
  )
))
documented <- c(documented, "outputs/05_rebuild_current_policy_routes/table_07_current_grant_policy_doc_routes.csv")

md <- c(md, block(
  path = "outputs/05_rebuild_current_policy_routes/table_08_current_grant_to_second_order_links.csv",
  blurb = "Long table of second-order (P2P) reach: grant -> intermediate first-order doc -> second-order doc. ~110k rows. New as of 2026-06-02.",
  producer = "`02_link_grants_to_publications.R` section 4b.",
  consumers = c("05 (top amplifiers, classification subsetting)", "08 (workbook)"),
  cols = c(
    grant_id                        = "Grant.",
    intermediate_policy_document_id = "First-order doc (cites the grant's DOI directly or via meta).",
    second_order_policy_document_id = "Downstream doc that cites the intermediate.",
    policy_title                    = "Title of the second-order doc.",
    policy_source_type              = "Type of the second-order doc.",
    policy_source_country           = "Country of the second-order doc.",
    policy_published_year           = "Year of the second-order doc.",
    policy_document_url             = "URL of the second-order doc."
  )
))
documented <- c(documented, "outputs/05_rebuild_current_policy_routes/table_08_current_grant_to_second_order_links.csv")

md <- c(md, block(
  path = "outputs/05_rebuild_current_policy_routes/table_09_current_second_order_summary_by_grant.csv",
  blurb = "Per-grant rollup of second-order reach. One row per grant with non-zero second-order reach.",
  producer = "`02_link_grants_to_publications.R` section 4b.",
  consumers = c("03 (master enrichment)", "08"),
  cols = c(
    grant_id             = "Grant.",
    n_intermediate_docs  = "Distinct first-order docs that lead anywhere downstream.",
    n_second_order_docs  = "Distinct second-order docs reached.",
    amplification_factor = "n_second_order_docs / n_intermediate_docs. High = each first-order doc gets many downstream citations."
  )
))
documented <- c(documented, "outputs/05_rebuild_current_policy_routes/table_09_current_second_order_summary_by_grant.csv")

md <- c(md, block(
  path = "outputs/05_rebuild_current_policy_routes/table_10_current_second_order_new_amplification.csv",
  blurb = "Subset of second-order links where the second-order doc isn't *also* a first-order doc for the same grant. The 'pure amplification' set. ~105k rows.",
  producer = "`02_link_grants_to_publications.R` section 4b.",
  consumers = c("(reference only - not yet consumed downstream)"),
  cols = c(
    grant_id                        = "Grant.",
    intermediate_policy_document_id = "First-order intermediate.",
    second_order_policy_document_id = "Second-order doc, not also a first-order match for this grant.",
    policy_title                    = "Title of the second-order doc.",
    policy_source_type              = "Type.",
    policy_source_country           = "Country.",
    policy_published_year           = "Year.",
    policy_document_url             = "URL."
  )
))
documented <- c(documented, "outputs/05_rebuild_current_policy_routes/table_10_current_second_order_new_amplification.csv")

md <- c(md, block(
  path = "outputs/06_build_grant_policy_master/table_01_grant_policy_master.csv",
  blurb = "**The grant-level master table.** 528 rows x ~100 columns. Baseline metadata, route summaries, search scope, institution type, classification profile, amplification. See `docs/grant_master_data_dictionary.csv` for the per-column dictionary - we only call out the headline columns here.",
  producer = "`03_enrich_grant_master.R`.",
  consumers = c("everything: 04, 06, 08, 09, dashboard, paper analyses"),
  cols = c(
    grant_id                     = "Cleaned IES award number. Row key.",
    pi                           = "PI name (post audit fix from May 2026).",
    institution                  = "Awardee institution (post audit fix).",
    award_year                   = "Award year.",
    institution_type             = "Compact bucket: University / Firm / Other / NA.",
    institution_type_detail      = "Long-form: University, Research Organization (Major), Private Firm, etc.",
    n_direct_docs                = "# policy docs reached via direct DOI citation.",
    n_meta_docs                  = "# policy docs reached via meta-analysis route.",
    n_policy_docs                = "n_direct_docs + n_meta_docs.",
    has_any_current_policy_reach = "The broad 'reached policy' flag.",
    n_intermediate_docs          = "# first-order docs that have any downstream citation. From 02.",
    n_second_order_docs          = "# distinct second-order docs reached. From 02.",
    amplification_factor         = "Ratio of second-order to intermediate.",
    has_second_order_reach       = "Has at least one second-order doc.",
    dominant_classification      = "Modal IPTC top-level tag among the grant's first-order docs.",
    dominant_n_docs              = "How many of the grant's docs carry the dominant tag.",
    dominant_pct_docs            = "Share of the grant's docs carrying the dominant tag.",
    `n_docs_tagged_*`            = "One column per top IPTC category (`n_docs_tagged_education`, `n_docs_tagged_health`, ...). Count of first-order docs tagged with that category."
  )
))
documented <- c(documented, "outputs/06_build_grant_policy_master/table_01_grant_policy_master.csv")

md <- c(md, block(
  path = "outputs/12e_classify_policy_docs/table_03_classifications_first_vs_second.csv",
  blurb = "Side-by-side first-order vs second-order IPTC topic mix. The 'think tanks as bridge' headline table.",
  producer = "`05_build_policy_doc_analysis.R` section 4c.",
  consumers = c("08 (workbook sheet `topics_first_vs_second_order`)"),
  cols = c(
    classification = "IPTC tag (hierarchical, separated by `>`).",
    FO_docs        = "# first-order docs tagged with this classification.",
    FO_pct         = "% of first-order docs tagged.",
    SO_docs        = "# second-order docs tagged.",
    SO_pct         = "% of second-order docs tagged."
  )
))
documented <- c(documented, "outputs/12e_classify_policy_docs/table_03_classifications_first_vs_second.csv")

md <- c(md, block(
  path = "outputs/12e_classify_policy_docs/table_09_per_grant_classification_wide.csv",
  blurb = "Per-grant classification profile. One row per grant with `n_docs_tagged_*` columns for the top 10 IPTC top-level categories.",
  producer = "`05_build_policy_doc_analysis.R` section 4c. Joined into the master by 03.",
  consumers = c("03 (master enrichment)", "08 (institution-type x topic cross-tab)"),
  cols = c(
    grant_id                = "Grant.",
    n_first_order_docs      = "Total first-order docs reached by this grant.",
    n_docs_tagged_education = "# docs tagged with IPTC `education` (any sub-tag).",
    n_docs_tagged_health    = "# docs tagged with `health`.",
    `n_docs_tagged_*`       = "Similar count columns for `economy, business and finance`, `labour`, `politics`, `society`, `science and technology`, `environment`, `human interest`, `lifestyle and leisure`."
  )
))
documented <- c(documented, "outputs/12e_classify_policy_docs/table_09_per_grant_classification_wide.csv")

md <- c(md, block(
  path = "outputs/12_build_policy_doc_overview/table_07_top_amplifying_intermediaries.csv",
  blurb = "First-order docs ranked by downstream reach. Identifies which IES-cited policy docs are themselves cited most heavily. **Includes the IPCC artifact** - some climate reports rank high because they share citations with a small number of IES grants, not because IES research shaped climate policy.",
  producer = "`05_build_policy_doc_analysis.R` section 4b.",
  consumers = c("08 (workbook sheet `p2p_top_amplifiers`)"),
  cols = c(
    intermediate_policy_document_id = "First-order doc.",
    n_downstream_docs               = "Distinct second-order docs that cite it.",
    n_grants_amplified              = "Distinct IES grants amplified through this doc. Multi-grant = stronger signal.",
    policy_source_type              = "Type of the intermediate.",
    country_raw                     = "Country.",
    policy_published_year           = "Year."
  )
))
documented <- c(documented, "outputs/12_build_policy_doc_overview/table_07_top_amplifying_intermediaries.csv")


#-------------------------------------------
## 9. APPENDIX - AUTO-SUMMARIZE LEFTOVERS ##
#-------------------------------------------

# walk all the stage-output directories and pick up any CSV we didn't
# describe by hand above. these get a short block with schema-only -
# no curated blurb, no producer line. it's not pretty but it keeps the
# dictionary complete: any time we add a new table to a stage output
# directory, the appendix will surface it on the next regeneration even
# if nobody's gotten around to writing curated docs for it

auto_dirs <- c(
  "outputs/02_build_current_grant_doi_universe",
  "outputs/03_build_policy_doi_linkage_reference",
  "outputs/05_rebuild_current_policy_routes",
  "outputs/05c_build_policy_search_scope",
  "outputs/06_build_grant_policy_master",
  "outputs/12_build_policy_doc_overview",
  "outputs/12b_build_policy_doc_geography",
  "outputs/12c_build_policy_doc_research_use_by_region",
  "outputs/12e_classify_policy_docs"
)

# collect every CSV path under those dirs, then drop the ones we
# already covered above
all_csv_paths <- character()
for (d in auto_dirs) {
  if (!dir.exists(here(d))) next
  for (f in list.files(here(d), pattern = "\\.csv$")) {
    all_csv_paths <- c(all_csv_paths, file.path(d, f))
  }
}
appendix_paths <- setdiff(all_csv_paths, documented)

if (length(appendix_paths) > 0) {
  md <- c(md,
    "## Appendix - auto-summarized",
    "",
    "These tables are intermediate/secondary and don't yet have curated",
    "column descriptions. Run the producing script (see file path) to",
    "regenerate the underlying data. Add a `block()` call in the right",
    "layer section above to promote one out of the appendix.",
    ""
  )

  # one schema-only block per leftover file. uses the same block()
  # helper as the curated section, just without the blurb/producer/
  # consumers info
  for (path in appendix_paths) {
    full <- here(path)
    md <- c(md,
      sprintf("### `%s`", path),
      "",
      sprintf("**Rows**: %s   |   **Size**: %s",
              format_int(count_rows(path)),
              format_size(file.info(full)$size)),
      "",
      "(auto-summarized - no curated description)",
      "",
      "**Schema**",
      "",
      schema_table(path),
      ""
    )
  }
}


#------------------------
## 10. WRITE THE FILE ##
#------------------------

# write everything in one shot. doing it this way means partial writes
# (if anything errored mid-script) never leave a broken docs/data_dictionary.md
# on disk - we either have the previous version or the new one, never
# a half-built one
dir.create(here("docs"), showWarnings = FALSE, recursive = TRUE)
out_path <- here("docs", "data_dictionary.md")
writeLines(md, out_path)

# persist the row-count cache. next run reads this and skips the line
# count for every file whose mtime hasn't changed since this run. first
# run takes a few minutes for the seven big Overton files; subsequent
# runs are instant
dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
write_csv(rowcount_cache, cache_path)

# small report so re-runs print something useful
cat("\n")
cat("Wrote data dictionary to:", out_path, "\n")
cat("  files documented by hand:", length(documented), "\n")
cat("  files in appendix:       ", length(appendix_paths), "\n")
cat("  total markdown lines:    ", length(md), "\n")
cat("  row-count cache entries: ", nrow(rowcount_cache), "\n")
