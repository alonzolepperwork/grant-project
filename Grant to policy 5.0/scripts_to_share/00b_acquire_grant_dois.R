# tries to find a DOI for every one of the 528 universe grants, falling back
# through three sources as each one runs dry
#
# first stop is ERIC: the education catalog indexes IES grants by contract number,
# so one query per grant returns its products and any DOIs they carry. whatever
# ERIC misses goes to OpenAlex - a funder+award lookup, then a fuzzy title match
# against the IES award-page title. anything still without a DOI, but with a
# product citation sitting in the page text, gets a Crossref bibliographic search,
# kept only when the year matches exactly and the title match Jaccard is >= 0.5
#
# every response is cached, so re-runs are nothing and fire no fresh API calls
# the 528-grant frame is built once in section 0 and reused, and the ERIC pairs
# from section 1 stay in memory so section 2 doesn't re-read them
#
# needs 00a to have run first: section 2's title search reads table_05 (award-page
# titles) and section 3 reads table_06 (product citations)
#
# key outputs (all under data/_rewrite_outputs/ unless noted):
#   r3_pairs_eval_only.csv                       - ERIC grant-DOI pairs (-> stage 01)
#   missing_grants_with_manual_dois.csv          - OpenAlex-recovered pairs (-> 01)
#   missing_grants_still_no_doi.csv              - grants with no DOI after all 3
#   data/_eric_download/eric_all_records.csv, eric_products_without_doi.csv,
#     eric_per_grant_productivity.csv
#   outputs/06h_lookup_product_dois_via_crossref/table_01_citation_doi_candidates.csv,
#     table_02_high_confidence_matches.csv
#
# based on the legacy all_eric_data.R, "finding more dois with open alex.R",
# and crossref_api_script.R

#------------------
## 0. INITIALIZE ##
#------------------

library(tidyverse)
library(here)
library(curl)
library(jsonlite)
library(httr2)

mailto        <- "your_email_here@email.com"
user_agent    <- "al-grant-to-policy/1.0 (mailto:your_email_here@email.com)"
ies_funder_id <- "F4320332210"   # IES funder OpenAlex ID, fixed (found online)

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

clean_grant <- function(x) {
  x %>% as.character() %>% str_trim() %>% str_to_upper() %>%
    na_if("") %>% na_if("NA")
}
norm_doi <- function(x) {
  x %>% as.character() %>% str_to_lower() %>%
    str_remove("^https?://(dx\\.)?doi\\.org/") %>%
    str_remove("^doi:") %>% str_trim() %>%
    na_if("") %>% na_if("na")
}

# title overlap as Jaccard on tokens; dropping short words avoids inflated overlap
# from articles and prepositions. shared by the OpenAlex title search (section 2)
# and the Crossref citation match (section 3)
norm_tokens <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return(character(0))
  s %>% str_to_lower() %>% str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish() %>% str_split(" ") %>% unlist() %>% discard(~ nchar(.x) < 3)
}
jaccard_titles <- function(a, b) {
  ta <- unique(norm_tokens(a)); tb <- unique(norm_tokens(b))
  if (length(ta) == 0 || length(tb) == 0) return(0)
  length(intersect(ta, tb)) / length(union(ta, tb))
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

# OpenAlex GET with exponential back-off on rate limits
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

# THE SHARED GRANT FRAME. the eval-set file alone is only ~456 grants (IES's
# eligibility-tagged subset); the 528-grant analytic universe is the UNION of:
# eval set, the baseline DOI file, the canonical recovered map, manual fills, and
# the grants recovery still couldn't find DOIs for. ERIC (section 1) and the
# Crossref filter (section 3) both anchor against this exact union
read_grants_col <- function(path, col_name) {
  if (!file.exists(path)) return(character(0))
  read_csv(path, show_col_types = FALSE) %>%
    pull({{ col_name }}) %>% clean_grant() %>% unique() %>% na.omit()
}

universe_grants <- unique(c(
  read_grants_col(pick_input("grant_universe_528_anchor.csv"), grant_id),
  read_grants_col(pick_input("eval_grants_UNION_DOIfile_plus_ERIC_R3.csv"), grant_id),
  read_grants_col(
    here("data", "01_legacy_original_inputs",
         "IES at 20 Publications(List of DOIs) with grant numbers.csv"),
    `Study Grant number`),
  read_grants_col(pick_input("missing_grants_with_manual_dois.csv"), grant_id),
  read_grants_col(pick_input("grant_doi_map_CANONICAL_with_recovered.csv"), grant_id),
  read_grants_col(pick_input("missing_grants_still_no_doi.csv"), grant_id)
))
cat("Grant universe (union of all sources):", length(universe_grants), "\n")

#--------------------------------
## 1. ERIC PER-GRANT DOWNLOAD ##
#--------------------------------

# previous award IDs (for renumbered grants) were extracted from the IES awards
# spreadsheet in the original legacy pipeline; we don't have that file linked, so renumbered
# grants just produce empty results and fall through to section 2 as missing
ies_awards <- tibble(AwardNum = universe_grants, AwardNumPrior = NA_character_)

eric_cache <- here("outputs", "_cache", "eric")
dir.create(eric_cache, showWarnings = FALSE, recursive = TRUE)

# fetch ERIC records for one grant ID (cached per grant), merging in the previous
# award ID's results when there is one
get_eric <- function(award_id) {
  cache_path <- file.path(eric_cache, paste0(award_id, ".csv"))
  if (file.exists(cache_path)) return(read_csv(cache_path, show_col_types = FALSE))

  message("Retrieving ", award_id, "...")

  fetch_one <- function(id) {
    # ERIC's default CSV is only 12 columns and omits the url field we need for
    # DOI extraction, so ask explicitly for everything useful. ERIC silently
    # ignores unknown field names, so over-requesting is safe
    fields <- paste(c(
      "id", "title", "author", "description", "source",
      "publicationdateyear", "publicationtype", "publisher",
      "url", "peerreviewed", "language",
      "issn", "isbn", "eissn", "eisbn",
      "audience", "intendedaudience", "educationlevel", "abstractor",
      "subject", "identifiersgeo", "identifiersassessmentandsurveys",
      "identifierslaws",
      "sponsor", "sponsorcode", "iesgrantcontractnum",
      "iesgranttype", "iescenter"
    ), collapse = ",")

    url <- paste0("https://api.ies.ed.gov/eric/",
                  "?search=iesgrantcontractnum%3A", id,
                  "&format=csv&rows=2000&fields=", fields)
    df <- tryCatch(read.csv(url), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(df)
    # ERIC returns publicationdateyear as numeric for some grants and string for
    # others; flatten every column to character so bind_rows doesn't choke
    df %>% mutate(across(everything(), as.character))
  }

  results <- fetch_one(award_id)

  alt <- ies_awards %>% filter(AwardNum == award_id) %>% pull(AwardNumPrior)
  if (length(alt) && !is.na(alt) && nzchar(alt)) {
    alt_results <- fetch_one(alt)
    if (!is.null(alt_results)) results <- bind_rows(results, alt_results)
  }

  if (is.null(results) || nrow(results) == 0) {
    results <- tibble(IES_award = character())   # empty file -> don't retry
  } else {
    results$IES_award <- rep(award_id, nrow(results))
    results <- distinct(results)
  }
  write_csv(results, cache_path)
  results
}

cat("Querying ERIC for", length(universe_grants), "grants...\n")
all_records <- progress_map_dfr(universe_grants, get_eric, label = "grants",
                                pause = 0.05)

# fix character encodings in the title (important for downstream dedup later)
all_records$title <- all_records$title %>%
  str_replace_all("&quot;", '"') %>%
  str_replace_all("&apos;", "'") %>%
  str_replace_all("&amp;apos;", "'") %>%
  str_replace_all("&amp;", "&")

# ERIC returns authors separated by "\,"; reformat to "; "
all_records$author <- all_records$author %>%
  str_replace_all("(?<!\\\\),", "; ") %>%
  str_replace_all("\\\\", "")

eric_out <- here("data", "_eric_download")
dir.create(eric_out, showWarnings = FALSE, recursive = TRUE)
write_csv(all_records, file.path(eric_out, "eric_all_records.csv"))

# ERIC has no `doi` field - it embeds DOIs in the url field when the URL points
# to a DOI resolver. pull them out with the standard DOI pattern
doi_regex <- "10\\.[0-9]{4,9}/[^\\s\\?&\"']+"
all_with_doi <- all_records %>%
  filter(!is.na(IES_award)) %>%
  mutate(doi = norm_doi(str_extract(url, doi_regex)))

# the linkable subset - has a DOI we can join into the citation graph. this is
# what feeds r3_pairs_eval_only.csv that stage 01 consumes
r3_pairs <- all_with_doi %>%
  filter(!is.na(doi)) %>%
  transmute(grant_id = clean_grant(IES_award), doi) %>%
  filter(!is.na(grant_id), !is.na(doi)) %>%
  distinct()
write_csv(r3_pairs, file.path(rewrite_dir, "r3_pairs_eval_only.csv"))

# products WITHOUT a DOI - real outputs (working papers, ERIC-only documents,
# government reports) we can't link to the citation graph, but that still count
# for grant-productivity analyses
no_doi_products <- all_with_doi %>%
  filter(is.na(doi)) %>%
  transmute(
    grant_id = clean_grant(IES_award),
    eric_id = id, title, author, publicationtype, publicationdateyear, publisher,
    source = if ("source" %in% names(.)) source else NA_character_,
    url
  ) %>%
  filter(!is.na(grant_id)) %>%
  distinct()
write_csv(no_doi_products, file.path(eric_out, "eric_products_without_doi.csv"))

# per-grant productivity rollup (linkable vs non-linkable counts)
productivity <- bind_rows(
  r3_pairs %>% transmute(grant_id, has_doi = TRUE),
  no_doi_products %>% transmute(grant_id, has_doi = FALSE)
) %>%
  group_by(grant_id) %>%
  summarize(n_products_total = n(),
            n_products_with_doi = sum(has_doi),
            n_products_without_doi = sum(!has_doi),
            .groups = "drop")
write_csv(productivity, file.path(eric_out, "eric_per_grant_productivity.csv"))

cat("  ERIC grant-DOI pairs:", nrow(r3_pairs),
    "| grants:", n_distinct(r3_pairs$grant_id),
    "| no-DOI products:", nrow(no_doi_products), "\n")

#-------------------------------------
## 2. OPENALEX DOI RECOVERY (GAPS) ##
#-------------------------------------

# grants in the universe ERIC gave no DOI for. r3_pairs is in memory from
# section 1, so no re-read
missing_grants <- setdiff(universe_grants, unique(r3_pairs$grant_id))
cat("\nGrants needing DOI recovery:", length(missing_grants), "\n")

oa_cache <- here("outputs", "_cache", "openalex_doi_recovery")
dir.create(oa_cache, showWarnings = FALSE, recursive = TRUE)

## 2a. METHOD 1 - OpenAlex by funder + award id. OpenAlex sometimes carries the
## IES award number as an award field on the work; when it does, this returns the
## works directly. (the fields were renamed in 2025: awards.funder_id /
## awards.funder_award_id; the old grants.* names now return 400.)
search_by_funder_award <- function(grant_id) {
  cache_path <- file.path(oa_cache, paste0("funder_", grant_id, ".csv"))
  if (file.exists(cache_path)) return(read_csv(cache_path, show_col_types = FALSE))

  payload <- oa_get(paste0(
    "https://api.openalex.org/works",
    "?filter=awards.funder_id:", ies_funder_id,
    ",awards.funder_award_id:", grant_id,
    "&per-page=200&mailto=", mailto))
  if (is.null(payload) || length(payload$results) == 0) {
    out <- tibble(grant_id = character(), doi = character())
  } else {
    out <- map_dfr(payload$results, function(w) {
      doi <- norm_doi(w$doi %||% "")
      if (is.na(doi)) return(NULL)
      tibble(grant_id = grant_id, doi = doi)
    })
  }
  write_csv(out, cache_path)
  out
}

funder_results <- progress_map_dfr(missing_grants, search_by_funder_award,
                                   label = "grants", pause = 0.1)
cat("  DOIs via funder+award lookup:", nrow(funder_results), "\n")
# DOIs via funder+award lookup: 9 

## 2b. METHOD 2 - fuzzy title search. for the rest, take the grant's title from
## the IES award page and search OpenAlex text, keeping matches whose relevance
## score clears 30. catches grants where OpenAlex's award mapping is missing/wrong
still_missing <- setdiff(missing_grants, unique(funder_results$grant_id))
cat("  Still missing after funder lookup:", length(still_missing), "\n")

ies_path <- here("outputs", "06b_enrich_master_from_ies_live_scrape",
                 "table_05_ies_award_page_parsed.csv")
if (file.exists(ies_path)) {
  ies_titles <- read_csv(ies_path, show_col_types = FALSE) %>%
    transmute(grant_id = clean_grant(ies_award_number),
              title = ies_title, year = as.integer(ies_year)) %>%
    filter(!is.na(grant_id), !is.na(title))
} else {
  ies_titles <- tibble(grant_id = character(), title = character(), year = integer())
}

search_by_title <- function(grant_id, title, expected_year) {
  cache_path <- file.path(oa_cache, paste0("title_", grant_id, ".csv"))
  # a no-match result caches as a header-only CSV, which read_csv then types as
  # character (no data rows to infer from) - that breaks bind_rows against the
  # numeric-score caches. coerce score on read so it's always double
  if (file.exists(cache_path)) {
    cached <- tryCatch(suppressWarnings(read_csv(cache_path, show_col_types = FALSE)),
                       error = function(e) NULL)
    if (is.null(cached) || nrow(cached) == 0)
      return(tibble(grant_id = character(), doi = character(), score = double()))
    return(dplyr::mutate(cached, score = as.numeric(score)))
  }

  # guard against missing title - URLencode(NA) returns "NA" and wastes retries
  if (is.na(title) || nchar(title) < 5) {
    out <- tibble(grant_id = character(), doi = character(), score = double())
    write_csv(out, cache_path); return(out)
  }

  # OpenAlex rejects publication_year:>=YYYY (HTTP 400); use from_publication_date
  # NA year falls back to 2000
  year_floor <- if (!is.na(expected_year)) max(expected_year - 1, 2000) else 2000
  payload <- oa_get(paste0(
    "https://api.openalex.org/works",
    "?search=", URLencode(title, reserved = TRUE),
    "&filter=from_publication_date:", year_floor, "-01-01",
    "&per-page=10&mailto=", mailto))
  if (is.null(payload) || length(payload$results) == 0) {
    out <- tibble(grant_id = character(), doi = character(), score = double())
  } else {
    out <- map_dfr(head(payload$results, 5), function(w) {
      doi <- norm_doi(w$doi %||% "")
      if (is.na(doi)) return(NULL)
      tibble(grant_id = grant_id, doi = doi,
             score = as.numeric(w$relevance_score %||% 0))
    })
  }
  write_csv(out, cache_path)
  out
}

title_results_raw <- progress_map_dfr(still_missing, function(gid) {
  row <- ies_titles %>% filter(grant_id == gid) %>% head(1)
  if (nrow(row) == 0) return(NULL)
  search_by_title(gid, row$title, row$year)
}, label = "grants", pause = 0.1)

# threshold at relevance_score > 30 (OpenAlex scores are log-scaled; 30+ is
# roughly "title closely matches")
title_results <- title_results_raw %>% filter(score > 30) %>% select(grant_id, doi)
cat("  DOIs via title search (score > 30):", nrow(title_results), "\n")

# the file stage 01 reads as "manual fills" (kept that name for compatibility -
# it isn't manual anymore)
manual_pairs <- bind_rows(funder_results, title_results) %>%
  filter(!is.na(grant_id), !is.na(doi)) %>%
  distinct()
write_csv(manual_pairs, file.path(rewrite_dir, "missing_grants_with_manual_dois.csv"))

still_no_doi <- setdiff(missing_grants, unique(manual_pairs$grant_id))
write_csv(tibble(grant_id = still_no_doi),
          file.path(rewrite_dir, "missing_grants_still_no_doi.csv"))
cat("  Total grants recovered via OpenAlex:", n_distinct(manual_pairs$grant_id),
    "| still no DOI:", length(still_no_doi), "\n")

#----------------------------------------
## 3. CROSSREF DOI RECOVERY (CITATIONS) ##
#----------------------------------------

# for product citations with no DOI in the text, search Crossref's bibliographic
# endpoint. we KEEP a match only when the publication year matches the citation
# year exactly AND title overlap (Jaccard on tokens) is >= 0.5 - thresholds a
# 30-grant manual audit put at 0% false matches
ies_prod_path <- here("outputs", "06b_enrich_master_from_ies_live_scrape",
                      "table_06_dois_extracted_from_products.csv")
if (!file.exists(ies_prod_path)) {
  stop("Need IES product citations (table_06). Run 00a_scrape_ies_award_pages.R first.")
}
ies_citations <- read_csv(ies_prod_path, show_col_types = FALSE)

# citations missing a DOI, restricted to the 528-grant frame so we don't spend
# the Crossref budget on grants dropped downstream anyway
no_doi_citations <- ies_citations %>%
  filter(is.na(doi) | !nzchar(doi)) %>%
  transmute(grant_id = clean_grant(grant_id),
            citation,
            citation_year = as.integer(str_extract(citation, "\\b(19|20)[0-9]{2}\\b")))
n_before <- nrow(no_doi_citations)
no_doi_citations <- no_doi_citations %>% filter(grant_id %in% universe_grants)
cat("\nCitations missing a DOI:", n_before,
    "| in universe (sent to Crossref):", nrow(no_doi_citations), "\n")

cr_cache <- here("outputs", "_cache", "crossref")
dir.create(cr_cache, showWarnings = FALSE, recursive = TRUE)

crossref_search <- function(citation_text) {
  cache_path <- file.path(cr_cache, paste0(digest::digest(citation_text), ".json"))
  if (file.exists(cache_path)) return(jsonlite::fromJSON(cache_path, simplifyVector = FALSE))
  url <- paste0("https://api.crossref.org/works",
                "?query.bibliographic=", URLencode(citation_text, reserved = TRUE),
                "&rows=3&mailto=", mailto)
  resp <- tryCatch(request(url) %>% req_user_agent(user_agent) %>% req_perform(),
                   error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) return(NULL)
  payload <- tryCatch(resp_body_json(resp), error = function(e) NULL)
  if (is.null(payload)) return(NULL)
  jsonlite::write_json(payload, cache_path, auto_unbox = TRUE)
  payload
}

top_candidate <- function(payload) {
  items <- payload$message$items
  if (is.null(items) || length(items) == 0) return(NULL)
  top <- items[[1]]
  ttl <- if (length(top$title)) top$title[[1]] else NA_character_
  year <- if (!is.null(top$issued$`date-parts`) && length(top$issued$`date-parts`)) {
    as.integer(top$issued$`date-parts`[[1]][[1]])
  } else NA_integer_
  list(doi = norm_doi(top$DOI %||% ""), title = ttl, year = year,
       crossref_score = as.numeric(top$score %||% 0))
}

candidates <- progress_map_dfr(seq_len(nrow(no_doi_citations)), function(i) {
  row <- no_doi_citations[i, ]
  payload <- crossref_search(row$citation)
  if (is.null(payload)) return(tibble())
  top <- top_candidate(payload)
  if (is.null(top) || is.na(top$doi)) return(tibble())
  tibble(grant_id = row$grant_id, citation = row$citation,
         citation_year = row$citation_year,
         candidate_doi = top$doi, candidate_title = top$title,
         candidate_year = top$year, crossref_score = top$crossref_score,
         title_jaccard = jaccard_titles(row$citation, top$title))
}, label = "citations", pause = 0.1)

# year must match exactly AND title overlap >= 0.5
high_conf <- candidates %>%
  filter(!is.na(candidate_year), !is.na(citation_year),
         candidate_year == citation_year, title_jaccard >= 0.5) %>%
  mutate(confidence = "high")

cr_out <- here("outputs", "06h_lookup_product_dois_via_crossref")
dir.create(cr_out, showWarnings = FALSE, recursive = TRUE)
write_csv(candidates, file.path(cr_out, "table_01_citation_doi_candidates.csv"))
write_csv(high_conf,  file.path(cr_out, "table_02_high_confidence_matches.csv"))

cat("  Crossref candidates:", nrow(candidates),
    "| accepted (high confidence):", nrow(high_conf),
    "| grants gaining DOIs:", n_distinct(high_conf$grant_id), "\n")

cat("\nGrant-DOI acquisition done (ERIC + OpenAlex + Crossref).\n")
