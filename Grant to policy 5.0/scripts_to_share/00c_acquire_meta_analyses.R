# builds the meta route: the meta-analyses and systematic reviews that cite a
# grant publication and then themselves get cited in policy, finishing at the
# is_meta_analysis flag on table_11
#
# four steps. first, optionally re-derive the meta-citer link file from OpenAlex
# cites:DOI. then fetch OpenAlex abstracts for the citing works and flag the
# true meta-analyses off their title + abstract. finally, layer on the
# Europe PMC publication-type tags and settle is_meta_analysis
#
# the meta set is broader than just meta-analyses: section 1 collects OpenAlex
# review-type works citing grant DOIs, which also catches plain systematic /
# literature reviews and can accidentally include "meta-cognition". sections 3-4 narrow that down
# to true meta-analyses using title + abstract self-reference (OpenAlex) and the
# curated "Meta-Analysis" pubType + abstract (Europe PMC). the final flag is the
# OR of those four signals
#
# note: section 1 is optional and off by default. the OpenAlex cites:DOI
# re-derivation is a ~long run, and its output is a standalone re-derivation
# that is not wired into sections 2-4: those read the curated legacy link file
# (meta_analysis_doi_links_to_searched_dois_and_grants.csv) as the source 
# flip REFETCH_META_CITERS only to rebuild that re-derivation for its own
# sake; the per-DOI cache makes a refresh incremental
#
# inputs:
#   data/02_legacy_existing_outputs/meta_analysis_doi_links_to_searched_dois_and_grants.csv
#     (the curated meta-link file: meta_analysis_doi + citing_openalex_id)
#   outputs/02_build_current_grant_doi_universe/table_01_current_grant_doi_pair_union.csv
#     (only when REFETCH_META_CITERS = TRUE)
# outputs:
#   outputs/05_rebuild_current_policy_routes/table_11_meta_work_metadata.csv
#     (one row per distinct meta DOI, with abstract + is_meta_analysis + evidence)
#   outputs/05_rebuild_current_policy_routes/table_12_meta_epmc_raw.csv
#   data/_rewrite_outputs/meta_analysis_doi_links_to_searched_dois_and_grants.csv
#     (only when REFETCH_META_CITERS = TRUE)
#
# based on _archive/root_legacy_scripts/Final working script january.R
# and newest_cleanest_meta_workflow.R

#------------------
## 0. INITIALIZE ##
#------------------

library(tidyverse)
library(here)
library(curl)
library(jsonlite)

# flip to TRUE only to rebuild the OpenAlex cites:DOI link file in section 1
# default FALSE: skip the ~48 min fetch and use the curated legacy link file
REFETCH_META_CITERS <- FALSE

mailto     <- "your_email_here@email.com"
user_agent <- "al-grant-to-policy/1.0 (mailto:your_email_here@email.com)"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# all writes go to data/_rewrite_outputs/ so the read-only legacy outputs stay
# untouched; reads prefer the rewrite output when it exists
rewrite_dir <- here("data", "_rewrite_outputs")
legacy_dir  <- here("data", "02_legacy_existing_outputs")
dir.create(rewrite_dir, showWarnings = FALSE, recursive = TRUE)
pick_input <- function(filename) {
  p <- file.path(rewrite_dir, filename)
  if (file.exists(p)) p else file.path(legacy_dir, filename)
}

norm_doi <- function(x) {
  x %>% as.character() %>% str_to_lower() %>%
    str_remove("^https?://(dx\\.)?doi\\.org/") %>%
    str_remove("^doi:") %>% str_trim() %>%
    na_if("") %>% na_if("na")
}

# polite-pool GET with exponential back-off on errors. shared by the OpenAlex
# and Europe PMC passes below
fetch_url <- function(url, retries = 4) {
  for (attempt in seq_len(retries)) {
    h <- new_handle()
    handle_setheaders(h, `User-Agent` = user_agent)
    resp <- tryCatch(curl_fetch_memory(url, handle = h), error = function(e) NULL)
    if (!is.null(resp) && resp$status_code == 200) return(rawToChar(resp$content))
    Sys.sleep(2 ^ attempt)
  }
  NULL
}

# OpenAlex returns abstracts as an inverted index {token: [positions]}; invert
# it back to position-ordered running text
abstract_from_index <- function(inv) {
  if (is.null(inv) || length(inv) == 0) return(NA_character_)
  positions <- integer(0); tokens <- character(0)
  for (tok in names(inv)) {
    locs <- unlist(inv[[tok]])
    positions <- c(positions, locs)
    tokens <- c(tokens, rep(tok, length(locs)))
  }
  paste(tokens[order(positions)], collapse = " ")
}

# the abstract self-reference rule, shared by the OpenAlex and EPMC passes: the
# work describes ITS OWN method as meta-analytic, not merely citing a prior one
title_meta_re <- "meta-?analy|meta-?regression"
abstract_meta_re <- paste(c(
  "(conduct|perform|present|report|provide|use|carri|undert)[a-z ]{0,18}meta-?analy",
  "meta-?analy[a-z]* (was|were) (conduct|perform|used|carri|appli)",
  "(a|this) meta-?analy", "systematic review and meta-?analy",
  "meta-?analytic (review|approach|method|techni|procedure|model|strateg)",
  "random[ -]?effects? (model|meta)", "a meta-?analysis of [0-9]",
  "we meta-?analy", "pooled (effect|estimate|standardized|mean)",
  "meta-?regression"), collapse = "|")

#--------------------------------------------------------------
## 1. (OPTIONAL) RE-DERIVE META-CITER LINKS FROM OPENALEX ##
#--------------------------------------------------------------

# OFF by default. this rebuilds meta_analysis_doi_links_..._and_grants.csv from
# scratch by asking OpenAlex, for every universe DOI, which review-type works
# cite it. NOTE: the output is a standalone re-derivation - sections 2-4 read the
# curated legacy link file, not this one - so running it does not change the
# downstream result. kept here so the derivation is reproducible on demand
if (REFETCH_META_CITERS) {

  oa_per_page  <- 100L
  oa_max_pages <- 25L    # cap so a viral paper doesn't run forever
  oa_sleep     <- 0.1    # polite-pool friendly

  clean_grant <- function(x) {
    x %>% as.character() %>% str_trim() %>% str_to_upper() %>%
      na_if("") %>% na_if("NA")
  }

  # progress-aware map_dfr drop-in: prints elapsed time, rate, and ETA each
  # iteration so a stalled API loop is obvious
  progress_map_dfr <- function(items, fn, label = "items", pause = 0) {
    start_time <- Sys.time()
    total <- length(items)
    map_dfr(seq_along(items), function(i) {
      out <- fn(items[[i]])
      if (pause > 0) Sys.sleep(pause)
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      rate <- i / elapsed
      remaining <- (total - i) / rate
      eta_str <- if (remaining > 60) {
        sprintf("%dm %ds", as.integer(remaining / 60), as.integer(remaining %% 60))
      } else {
        sprintf("%ds", as.integer(remaining))
      }
      label_id <- if (is.character(items[[i]])) items[[i]] else as.character(i)
      cat(sprintf("  [%d/%d] %s - %.1fs elapsed, %.1f %s/s, eta ~%s\n",
                  i, total, label_id, elapsed, rate, label, eta_str))
      out
    })
  }

  oa_get <- function(url, retries = 3) {
    for (attempt in seq_len(retries)) {
      h <- new_handle()
      handle_setheaders(h, `User-Agent` = paste0("mailto:", mailto))
      resp <- tryCatch(curl_fetch_memory(url, handle = h), error = function(e) NULL)
      if (!is.null(resp) && resp$status_code == 200) {
        return(tryCatch(fromJSON(rawToChar(resp$content), simplifyVector = FALSE),
                        error = function(e) NULL))
      }
      Sys.sleep(2 ^ attempt)
    }
    NULL
  }

  # OpenAlex work ID for a DOI - needed to query "cites:"
  oa_work_by_doi <- function(doi) {
    oa_get(paste0("https://api.openalex.org/works/doi:",
                  URLencode(doi, reserved = TRUE), "?mailto=", mailto))
  }

  # all works that cite a given OpenAlex work ID, paginated
  oa_all_citers <- function(work_id) {
    short_id <- sub("^https?://openalex\\.org/", "", as.character(work_id))
    out <- list()
    for (page in seq_len(oa_max_pages)) {
      payload <- oa_get(paste0("https://api.openalex.org/works",
                               "?filter=cites:", short_id,
                               "&per-page=", oa_per_page, "&page=", page,
                               "&mailto=", mailto))
      if (is.null(payload) || length(payload$results) == 0) break
      out <- c(out, payload$results)
      if (length(payload$results) < oa_per_page) break
      Sys.sleep(oa_sleep)
    }
    out
  }

  # inclusive review-type patterns - keep anything that looks like synthesis;
  # downstream sections cut it further
  review_pat <- paste(c(
    "meta[- ]analy", "systematic review", "research synthesis",
    "best[- ]evidence synthesis", "evidence synthesis",
    "scoping review", "narrative review", "review of the literature",
    "literature review", "umbrella review"), collapse = "|")

  abstract_to_text <- function(aii) {
    if (is.null(aii) || length(aii) == 0) return("")
    paste(names(aii), collapse = " ")
  }
  is_review_like <- function(work) {
    title <- str_to_lower(coalesce(as.character(work$title %||% ""), ""))
    abs   <- str_to_lower(abstract_to_text(work$abstract_inverted_index))
    type  <- str_to_lower(coalesce(as.character(work$type %||% ""), ""))
    type == "review" || str_detect(title, review_pat) || str_detect(abs, review_pat)
  }

  # universe pairs to search citers for
  pairs <- read_csv(
    here("outputs", "02_build_current_grant_doi_universe",
         "table_01_current_grant_doi_pair_union.csv"),
    show_col_types = FALSE
  ) %>%
    transmute(grant_id = clean_grant(grant_id), doi = norm_doi(doi)) %>%
    filter(!is.na(grant_id), !is.na(doi)) %>%
    distinct()

  target_dois <- unique(pairs$doi)
  cat("Target DOIs to find citers for:", length(target_dois), "\n")

  cache_dir <- here("outputs", "_cache", "openalex_meta_citers")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  fetch_meta_citers_one_doi <- function(doi) {
    cache_path <- file.path(cache_dir, paste0(digest::digest(doi), ".csv"))
    if (file.exists(cache_path)) return(read_csv(cache_path, show_col_types = FALSE))

    work <- oa_work_by_doi(doi)
    if (is.null(work) || is.null(work$id)) {
      out <- tibble(); write_csv(out, cache_path); return(out)
    }
    citers <- oa_all_citers(work$id)
    if (length(citers) == 0) {
      out <- tibble(); write_csv(out, cache_path); return(out)
    }
    reviews <- keep(citers, is_review_like)
    if (length(reviews) == 0) {
      out <- tibble(); write_csv(out, cache_path); return(out)
    }
    out <- map_dfr(reviews, function(r) {
      tibble(
        searched_doi = doi,
        meta_doi   = norm_doi(r$doi %||% ""),
        meta_title = as.character(r$title %||% NA_character_),
        meta_year  = as.integer(r$publication_year %||% NA_integer_),
        meta_type  = as.character(r$type %||% NA_character_),
        meta_oa_id = as.character(r$id %||% NA_character_)
      )
    }) %>% filter(!is.na(meta_doi))
    write_csv(out, cache_path)
    Sys.sleep(oa_sleep)
    out
  }

  cat("Fetching citers (cache hits are instant)...\n")
  all_citers <- progress_map_dfr(target_dois, fetch_meta_citers_one_doi,
                                 label = "DOIs")

  # every searched_doi ties back to its grant(s) via the pairs table; a single
  # meta-analysis can therefore link to multiple grants
  meta_grant_links <- all_citers %>%
    inner_join(pairs, by = c("searched_doi" = "doi")) %>%
    distinct(grant_id, searched_doi, meta_doi, meta_title, meta_year, meta_type) %>%
    rename(citing_year = meta_year)

  write_csv(meta_grant_links,
            file.path(rewrite_dir,
                      "meta_analysis_doi_links_to_searched_dois_and_grants.csv"))

  cat("  Reviews found:", nrow(all_citers),
      "| distinct meta DOIs:", n_distinct(all_citers$meta_doi),
      "| grants reached:", n_distinct(meta_grant_links$grant_id), "\n")
} else {
  cat("Section 1 skipped (REFETCH_META_CITERS = FALSE) - using curated link file.\n")
}

#-----------------------------------
## 2. DISTINCT META WORKS TO FETCH ##
#-----------------------------------

# source of truth for the meta route: the curated link file (meta_analysis_doi +
# citing_openalex_id, one row per grant-DOI -> meta edge). it identifies the
# citing works and their OpenAlex IDs; sections 3-4 add the abstracts/pubtypes
# needed to separate true meta-analyses from plain reviews and meta-cognition
meta_src <- read_csv(
  here("data", "02_legacy_existing_outputs",
       "meta_analysis_doi_links_to_searched_dois_and_grants.csv"),
  show_col_types = FALSE
)

works <- meta_src %>%
  transmute(meta_doi = str_to_lower(str_trim(meta_analysis_doi)),
            openalex_id = str_remove(citing_openalex_id, "^https?://openalex\\.org/")) %>%
  filter(!is.na(openalex_id), openalex_id != "") %>%
  distinct(openalex_id, .keep_all = TRUE)

cat("Distinct meta works to fetch:", nrow(works), "\n")

#-------------------------------------------------
## 3. OPENALEX ABSTRACTS + TITLE/ABSTRACT FLAGS ##
#-------------------------------------------------

# many works titled "A Systematic Review of ..." are in fact meta-analyses and
# only say so in the abstract ("we conducted a meta-analysis of 14 studies"), so
# a title-only filter both misses those and can't separate meta-analysis from
# meta-cognition. fetch 50 IDs per request, cached as raw JSON so re-runs are
# free
cache_dir <- here("outputs", "_cache", "meta_abstracts")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

ids <- works$openalex_id
batches <- split(ids, ceiling(seq_along(ids) / 50))
select_fields <- "id,doi,title,type,publication_year,abstract_inverted_index"

parsed_rows <- list()
for (i in seq_along(batches)) {
  cache_path <- file.path(cache_dir, sprintf("batch_%03d.json", i))

  if (file.exists(cache_path)) {
    raw <- read_file(cache_path)
  } else {
    # OpenAlex ORs IDs with the pipe character, up to 50 per request
    url <- paste0("https://api.openalex.org/works?filter=openalex_id:",
                  paste(batches[[i]], collapse = "|"),
                  "&per-page=50&select=", select_fields, "&mailto=", mailto)
    raw <- fetch_url(URLencode(url))
    if (is.null(raw)) { cat("  batch", i, "failed, skipping\n"); next }
    write_file(raw, cache_path)
    Sys.sleep(0.1)
  }

  doc <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(doc) || is.null(doc$results)) next

  for (w in doc$results) {
    parsed_rows[[length(parsed_rows) + 1]] <- tibble(
      openalex_id = str_remove(w$id %||% NA_character_, "^https?://openalex\\.org/"),
      oa_doi   = str_to_lower(str_remove(w$doi %||% NA_character_, "^https?://doi\\.org/")),
      oa_title = w$title %||% NA_character_,
      oa_type  = w$type %||% NA_character_,
      oa_year  = w$publication_year %||% NA_integer_,
      abstract = abstract_from_index(w$abstract_inverted_index)
    )
  }
  if (i %% 5 == 0) cat("  fetched batch", i, "of", length(batches), "\n")
}

fetched <- bind_rows(parsed_rows)

# key on the OpenAlex ID (the link table's DOI is the source of truth for
# meta_doi; OpenAlex's own doi is kept as oa_doi for a cross-check)
meta_meta <- works %>%
  left_join(fetched, by = "openalex_id") %>%
  transmute(meta_doi, openalex_id,
            title = oa_title, type = oa_type, year = oa_year, abstract)

# flag works that are actually meta-analyses: title says so (precise), or the
# abstract self-identifies. meta-cognition is excluded automatically - a
# metacognition review that isn't a meta-analysis matches neither signal
meta_meta <- meta_meta %>%
  mutate(
    .t = str_to_lower(coalesce(title, "")),
    .a = str_to_lower(coalesce(abstract, "")),
    title_says_meta    = str_detect(.t, title_meta_re),
    abstract_says_meta = str_detect(.a, abstract_meta_re),
    mentions_metacog   = str_detect(str_c(.t, " ", .a), "meta-?cognit|metacognit")
  ) %>%
  select(-.t, -.a)

#-------------------------------------------------------
## 4. EUROPE PMC PUBTYPES -> FINAL is_meta_analysis ##
#-------------------------------------------------------

# Europe PMC tags works with an explicit "Meta-Analysis" publication type
# (MeSH-derived, abstract-independent) AND returns an abstract, so one pass
# recovers meta-analyses that OpenAlex either gave no abstract for or titled
# "systematic review". EPMC indexes biomedical and many social-science journals;
# education-only journals may be absent, so this is additive, never a
# replacement. the final flag is the OR of all four signals
dois <- meta_meta$meta_doi %>% na.omit() %>% unique()
cat("Meta DOIs to look up in Europe PMC:", length(dois), "\n")

cache_dir <- here("outputs", "_cache", "meta_epmc")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

batches <- split(dois, ceiling(seq_along(dois) / 25))
epmc_rows <- list()
for (i in seq_along(batches)) {
  cache_path <- file.path(cache_dir, sprintf("batch_%03d.json", i))
  if (file.exists(cache_path)) {
    raw <- read_file(cache_path)
  } else {
    # OR the batch's DOIs into one query; core resultType carries pubTypeList
    # and abstractText. pageSize must cover the batch size
    query <- paste(sprintf('DOI:"%s"', batches[[i]]), collapse = " OR ")
    url <- paste0("https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=",
                  URLencode(query, reserved = TRUE),
                  "&format=json&resultType=core&pageSize=25")
    raw <- fetch_url(url)
    if (is.null(raw)) { cat("  batch", i, "failed, skipping\n"); next }
    write_file(raw, cache_path)
    Sys.sleep(0.2)   # EPMC asks for <= a few requests/sec
  }

  doc <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  results <- doc$resultList$result
  if (is.null(results)) next
  for (w in results) {
    pt <- w$pubTypeList$pubType
    pt <- if (is.null(pt)) character(0) else unlist(pt)
    epmc_rows[[length(epmc_rows) + 1]] <- tibble(
      epmc_doi = str_to_lower(w$doi %||% NA_character_),
      epmc_pubtypes = paste(pt, collapse = "; "),
      epmc_is_meta = any(str_to_lower(pt) == "meta-analysis"),
      epmc_abstract = w$abstractText %||% NA_character_
    )
  }
  if (i %% 10 == 0) cat("  EPMC batch", i, "of", length(batches), "\n")
}

epmc <- bind_rows(epmc_rows) %>%
  filter(!is.na(epmc_doi)) %>%
  distinct(epmc_doi, .keep_all = TRUE)

out_dir <- here("outputs", "05_rebuild_current_policy_routes")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(epmc, file.path(out_dir, "table_12_meta_epmc_raw.csv"))

# fold EPMC into the classification and finalize is_meta_analysis + evidence
meta_meta <- meta_meta %>%
  left_join(epmc, by = c("meta_doi" = "epmc_doi")) %>%
  mutate(
    epmc_is_meta  = replace_na(epmc_is_meta, FALSE),
    epmc_abs_meta = str_detect(str_to_lower(coalesce(epmc_abstract, "")),
                               abstract_meta_re),
    is_meta_analysis = title_says_meta | abstract_says_meta |
                       epmc_is_meta | epmc_abs_meta,
    meta_evidence = case_when(
      title_says_meta    ~ "title",
      abstract_says_meta ~ "abstract self-reference (OpenAlex)",
      epmc_is_meta       ~ "Europe PMC pubType: Meta-Analysis",
      epmc_abs_meta      ~ "abstract self-reference (Europe PMC)",
      is.na(abstract) & is.na(epmc_abstract) ~ "no abstract - title only (not meta)",
      TRUE               ~ "review, not meta-analysis"
    )
  )

write_csv(meta_meta, file.path(out_dir, "table_11_meta_work_metadata.csv"))

#---------------------
## 5. COVERAGE REPORT ##
#---------------------

cat("\nMeta-work pipeline done.\n")
cat("  meta works:                 ", nrow(meta_meta), "\n")
cat("  with an abstract (OpenAlex):", sum(!is.na(meta_meta$abstract)),
    sprintf("(%.1f%%)\n", 100 * mean(!is.na(meta_meta$abstract))))
cat("  found in Europe PMC:        ", sum(!is.na(meta_meta$epmc_pubtypes)), "\n")
cat("  EPMC abstracts added (were NA):",
    sum(is.na(meta_meta$abstract) & !is.na(meta_meta$epmc_abstract)), "\n\n")
cat("  TRUE meta-analyses:         ", sum(meta_meta$is_meta_analysis),
    "of", nrow(meta_meta), "\n")
cat("    via title:                ", sum(meta_meta$title_says_meta), "\n")
cat("    via OpenAlex abstract:    ",
    sum(meta_meta$abstract_says_meta & !meta_meta$title_says_meta), "\n")
cat("    added by Europe PMC:      ",
    sum(meta_meta$is_meta_analysis &
        !(meta_meta$title_says_meta | meta_meta$abstract_says_meta)), "\n")
cat("  meta-cognition (excluded):  ",
    sum(meta_meta$mentions_metacog & !meta_meta$is_meta_analysis), "\n")
cat("  evidence breakdown:\n")
print(meta_meta %>% filter(is_meta_analysis) %>% count(meta_evidence, sort = TRUE))
