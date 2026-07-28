# enriches the grant-publication (non-meta) universe DOIs with abstracts and
# publication-type tags. it walks three sources from best coverage to worst -
# Europe PMC, then Semantic Scholar, then Crossref - handing a
# DOI down to the next, sparser source only if the ones before it still left it
# without an abstract
#
# this matters because the empirical/non-empirical split in stage 01 is a
# residual - a DOI is "empirical" unless something flags it as a review /
# protocol / commentary - so the empirical DOIs with no abstract were never
# actually checked, just defaulted. each source here closes two gaps at once: it
# fills missing abstractText (more text for stage 01's heuristic), and it carries
# curated pubType tags that give a positive signal both ways (RCT / trial /
# comparative / evaluation confirm empirical; review / meta / editorial / comment
# flag non-empirical the OpenAlex-type filter may miss)
#
# what each brings: Europe PMC has curated pubTypes + abstracts (biomedical and
# some soc-sci); Semantic Scholar covers education/soc-sci broadly at 500 DOIs a
# call; Crossref fills in publisher-deposited JATS abstracts for the stragglers
#
# this script only FETCHES + REPORTS. it does NOT rewrite is_empirical - that
# decision (and which flips to accept) is made in stage 01
#
# inputs:
#   outputs/02_build_current_grant_doi_universe/table_01_current_grant_doi_pair_union.csv
#   outputs/openalex_enrichment/doi_metadata.csv  (which abstracts OpenAlex already had)
# outputs (under outputs/_universe_filter_audit/):
#   universe_epmc_pubtypes.csv, universe_s2_pubtypes.csv, universe_crossref_abstracts.csv

#------------------
## 0. INITIALIZE ##
#------------------

library(tidyverse)
library(here)
library(curl)
library(jsonlite)
library(digest)

mailto     <- "your_email_here@email.com"
user_agent <- "al-grant-to-policy/1.0 (mailto:your_email_here@email.com)"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# polite-pool GET with back-off; treats 404 as a definitive "no such DOI"
fetch_url <- function(url, retries = 4) {
  for (attempt in seq_len(retries)) {
    h <- new_handle()
    handle_setheaders(h, `User-Agent` = user_agent)
    resp <- tryCatch(curl_fetch_memory(url, handle = h), error = function(e) NULL)
    if (!is.null(resp) && resp$status_code == 200) return(rawToChar(resp$content))
    if (!is.null(resp) && resp$status_code == 404) return(NA_character_)
    Sys.sleep(2 ^ attempt)
  }
  NULL
}

# POST a JSON body with back-off (Semantic Scholar's keyless pool returns 429s)
post_json <- function(url, body, retries = 6) {
  for (attempt in seq_len(retries)) {
    h <- new_handle()
    handle_setheaders(h, "Content-Type" = "application/json", "User-Agent" = user_agent)
    handle_setopt(h, customrequest = "POST", postfields = body)
    resp <- tryCatch(curl_fetch_memory(url, handle = h), error = function(e) NULL)
    if (!is.null(resp) && resp$status_code == 200) return(rawToChar(resp$content))
    Sys.sleep(2 ^ attempt)
  }
  NULL
}

# strip JATS/XML tags and tidy a Crossref abstract into running text
clean_abstract <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_character_)
  x %>%
    str_remove_all("<[^>]+>") %>%
    str_replace_all("&amp;", "&") %>%
    str_replace_all("&lt;", "<") %>% str_replace_all("&gt;", ">") %>%
    str_remove("^\\s*Abstract\\s*[:.]?\\s*") %>%
    str_squish() %>% na_if("")
}

# Europe PMC pubType tags. empirical confirm primary research; non-empirical flag
# non-primary work. "Historical Article" is deliberately EXCLUDED - EPMC applies
# it unreliably here (it mistagged an empirical RD study and a methods paper), so
# it would manufacture false flips
empirical_pubtypes <- str_to_lower(c(
  "Randomized Controlled Trial", "Controlled Clinical Trial", "Clinical Trial",
  "Pragmatic Clinical Trial", "Comparative Study", "Evaluation Study",
  "Evaluation Studies", "Multicenter Study", "Observational Study",
  "Validation Study", "Twin Study", "Clinical Study"))
nonempirical_pubtypes <- str_to_lower(c(
  "Review", "Systematic Review", "Meta-Analysis", "Editorial", "Comment",
  "Letter", "Erratum", "Published Erratum", "Practice Guideline", "Guideline",
  "Consensus Development Conference", "News", "Biography",
  "Introductory Journal Article", "Retraction of Publication"))

# Semantic Scholar types. "Study" is ignored for positive confirmation - it's
# noisy and co-occurs with Review/MetaAnalysis
s2_nonempirical_types <- c("Review", "MetaAnalysis", "Editorial",
                           "LettersAndComments", "News")
s2_empirical_types     <- c("ClinicalTrial")

#----------------------------------------------
## 1. UNIVERSE DOIS + CURRENT ABSTRACT STATE ##
#----------------------------------------------

pairs <- read_csv(
  here("outputs", "02_build_current_grant_doi_universe",
       "table_01_current_grant_doi_pair_union.csv"), show_col_types = FALSE)

universe <- pairs %>% distinct(doi, is_empirical) %>% mutate(doi = str_to_lower(doi))

# OpenAlex (stage 07) already supplied most abstracts; track which DOIs it left
# without one so the three sources below only chase the genuine gaps
oa <- read_csv(here("outputs", "openalex_enrichment", "doi_metadata.csv"),
               show_col_types = FALSE) %>%
  mutate(doi = str_to_lower(str_remove(str_remove(doi, "^https?://doi\\.org/"), "^doi:"))) %>%
  distinct(doi, .keep_all = TRUE) %>%
  transmute(doi, oa_has_abstract = !is.na(abstract) & str_trim(abstract) != "")

universe <- universe %>% left_join(oa, by = "doi") %>%
  mutate(oa_has_abstract = replace_na(oa_has_abstract, FALSE))

dois <- universe$doi %>% na.omit() %>% unique()
cat("Universe DOIs to enrich:", length(dois), "\n")

#--------------------------------------------
## 2. EUROPE PMC PUBTYPES + ABSTRACTS ##
#--------------------------------------------

epmc_cache <- here("outputs", "_cache", "universe_epmc")
dir.create(epmc_cache, showWarnings = FALSE, recursive = TRUE)

batches <- split(dois, ceiling(seq_along(dois) / 25))
epmc_rows <- list()
for (i in seq_along(batches)) {
  cache_path <- file.path(epmc_cache, sprintf("batch_%04d.json", i))
  if (file.exists(cache_path)) {
    raw <- read_file(cache_path)
  } else {
    query <- paste(sprintf('DOI:"%s"', batches[[i]]), collapse = " OR ")
    url <- paste0("https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=",
                  URLencode(query, reserved = TRUE),
                  "&format=json&resultType=core&pageSize=25")
    raw <- fetch_url(url)
    if (is.null(raw)) { cat("  EPMC batch", i, "failed, skipping\n"); next }
    write_file(raw, cache_path)
    Sys.sleep(0.2)
  }
  doc <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  results <- doc$resultList$result
  if (is.null(results)) next
  for (w in results) {
    pt <- w$pubTypeList$pubType
    pt <- if (is.null(pt)) character(0) else str_to_lower(unlist(pt))
    epmc_rows[[length(epmc_rows) + 1]] <- tibble(
      doi = str_to_lower(w$doi %||% NA_character_),
      epmc_pubtypes = paste(unlist(w$pubTypeList$pubType), collapse = "; "),
      epmc_emp = any(pt %in% empirical_pubtypes),
      epmc_nonemp = any(pt %in% nonempirical_pubtypes),
      epmc_abstract = w$abstractText %||% NA_character_
    )
  }
  if (i %% 20 == 0) cat("  EPMC batch", i, "of", length(batches), "\n")
}
epmc <- bind_rows(epmc_rows) %>% filter(!is.na(doi)) %>% distinct(doi, .keep_all = TRUE)

epmc_result <- universe %>%
  left_join(epmc, by = "doi") %>%
  mutate(
    in_epmc = !is.na(epmc_pubtypes),
    epmc_emp = replace_na(epmc_emp, FALSE),
    epmc_nonemp = replace_na(epmc_nonemp, FALSE),
    epmc_abstract_added = !oa_has_abstract & !is.na(epmc_abstract) &
                          str_trim(replace_na(epmc_abstract, "")) != "",
    # flip candidate: currently empirical, but EPMC's curated tag says
    # non-empirical and does NOT also tag it as a trial/empirical study
    flip_to_nonempirical = is_empirical & epmc_nonemp & !epmc_emp
  )

out_dir <- here("outputs", "_universe_filter_audit")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(epmc_result %>% select(doi, is_empirical, in_epmc, epmc_pubtypes,
                                 epmc_emp, epmc_nonemp, epmc_abstract_added,
                                 flip_to_nonempirical, epmc_abstract),
          file.path(out_dir, "universe_epmc_pubtypes.csv"))

# per-DOI EPMC abstract flag, kept in memory for sections 3-4
epmc_abs <- epmc_result %>%
  transmute(doi, epmc_abs = !is.na(epmc_abstract) &
            str_trim(replace_na(epmc_abstract, "")) != "")

#--------------------------------------------
## 3. SEMANTIC SCHOLAR PUBTYPES + ABSTRACTS ##
#--------------------------------------------

# S2's paper/batch endpoint takes 500 DOIs per request (the whole universe is
# ~6 calls) and returns abstract + publicationTypes together
s2_cache <- here("outputs", "_cache", "universe_s2")
dir.create(s2_cache, showWarnings = FALSE, recursive = TRUE)

s2_url <- paste0("https://api.semanticscholar.org/graph/v1/paper/batch",
                 "?fields=abstract,publicationTypes,title,externalIds")
batches <- split(dois, ceiling(seq_along(dois) / 500))
s2_rows <- list()
for (i in seq_along(batches)) {
  cache_path <- file.path(s2_cache, sprintf("batch_%03d.json", i))
  ids <- paste0("DOI:", batches[[i]])
  if (file.exists(cache_path)) {
    raw <- read_file(cache_path)
  } else {
    raw <- post_json(s2_url, toJSON(list(ids = ids), auto_unbox = FALSE))
    if (is.null(raw)) { cat("  S2 batch", i, "failed, skipping\n"); next }
    write_file(raw, cache_path)
    Sys.sleep(1)   # keyless pool is ~1 req/sec
  }
  doc <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(doc)) next
  # response is same length/order as the input ids; nulls for not-found
  for (k in seq_along(doc)) {
    w <- doc[[k]]
    if (is.null(w)) next
    pt <- if (is.null(w$publicationTypes)) character(0) else unlist(w$publicationTypes)
    s2_rows[[length(s2_rows) + 1]] <- tibble(
      doi = str_to_lower(str_remove(batches[[i]][k] %||% NA_character_, "^doi:")),
      s2_pubtypes = paste(pt, collapse = "; "),
      s2_nonemp = any(pt %in% s2_nonempirical_types),
      s2_emp = any(pt %in% s2_empirical_types),
      s2_abstract = w$abstract %||% NA_character_
    )
  }
  cat("  S2 batch", i, "of", length(batches), "done\n")
}
s2 <- bind_rows(s2_rows) %>% filter(!is.na(doi)) %>% distinct(doi, .keep_all = TRUE)

# had_abstract uses OpenAlex (universe) + Europe PMC (epmc_abs), both in memory
s2_result <- universe %>%
  select(doi, is_empirical, oa_has_abstract) %>%
  left_join(s2, by = "doi") %>%
  left_join(epmc_abs, by = "doi") %>%
  mutate(across(c(s2_nonemp, s2_emp, epmc_abs), ~replace_na(., FALSE)),
         had_abstract = oa_has_abstract | epmc_abs,
         s2_abstract_added = !had_abstract & !is.na(s2_abstract) &
                             str_trim(replace_na(s2_abstract, "")) != "",
         flip_to_nonempirical = is_empirical & s2_nonemp & !s2_emp)

write_csv(s2_result %>% select(doi, is_empirical, s2_pubtypes, s2_nonemp, s2_emp,
                               s2_abstract_added, flip_to_nonempirical, s2_abstract),
          file.path(out_dir, "universe_s2_pubtypes.csv"))

# per-DOI S2 abstract flag, kept in memory for section 4
s2_abs <- s2_result %>%
  transmute(doi, s2_abs = !is.na(s2_abstract) &
            str_trim(replace_na(s2_abstract, "")) != "")

#--------------------------------------------
## 4. CROSSREF ABSTRACT BACKFILL (GAPS) ##
#--------------------------------------------

# whatever still has no abstract after OpenAlex + EPMC + S2 - mostly the
# education-journal tail. Crossref returns the publisher-deposited JATS abstract
# when present (Wiley, SAGE/AERA deposit; Elsevier, Springer, APA, T&F mostly
# don't), so this fills what it can and the rest stay abstract-less
have <- universe %>%
  left_join(epmc_abs, by = "doi") %>%
  left_join(s2_abs, by = "doi") %>%
  mutate(across(c(epmc_abs, s2_abs), ~replace_na(., FALSE)),
         any_abs = oa_has_abstract | epmc_abs | s2_abs) %>%
  filter(any_abs) %>% pull(doi)

missing <- setdiff(dois, have)
cat("Universe DOIs still missing an abstract after EPMC + S2:", length(missing), "\n")

cr_cache <- here("outputs", "_cache", "universe_crossref_abs")
dir.create(cr_cache, showWarnings = FALSE, recursive = TRUE)

rows <- vector("list", length(missing))
for (i in seq_along(missing)) {
  doi <- missing[i]
  cache_path <- file.path(cr_cache, paste0(digest(doi, algo = "sha1"), ".json"))
  if (file.exists(cache_path)) {
    raw <- read_file(cache_path)
  } else {
    url <- paste0("https://api.crossref.org/works/", URLencode(doi, reserved = TRUE),
                  "?mailto=", mailto)
    raw <- fetch_url(url)
    if (is.null(raw)) next                 # transient failure - skip, retry next run
    write_file(if (is.na(raw)) "{}" else raw, cache_path)
    Sys.sleep(0.05)
  }
  doc <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  abs <- clean_abstract(doc$message$abstract %||% NA_character_)
  rows[[i]] <- tibble(doi = doi, crossref_abstract = abs)
  if (i %% 100 == 0) cat("  Crossref", i, "of", length(missing), "\n")
}

crossref <- bind_rows(rows)
filled <- crossref %>% filter(!is.na(crossref_abstract))
write_csv(crossref, file.path(out_dir, "universe_crossref_abstracts.csv"))

#---------------------
## 5. REPORT ##
#---------------------

cat("\nUniverse enrichment done (Europe PMC + Semantic Scholar + Crossref).\n")
cat("  universe DOIs:               ", nrow(universe), "\n")
cat("  found in Europe PMC:         ", sum(epmc_result$in_epmc),
    sprintf("(%.1f%%)", 100 * mean(epmc_result$in_epmc)), "\n")
cat("  found in Semantic Scholar:   ", sum(!is.na(s2_result$s2_pubtypes)), "\n")
cat("  EPMC abstracts added:        ", sum(epmc_result$epmc_abstract_added), "\n")
cat("  S2 abstracts added:          ", sum(s2_result$s2_abstract_added), "\n")
cat("  Crossref abstracts recovered:", nrow(filled),
    "of", length(missing), "still-missing\n")
cat("  still without abstract:      ", length(missing) - nrow(filled), "\n\n")
cat("  flip-to-non-empirical candidates: EPMC", sum(epmc_result$flip_to_nonempirical),
    "| S2", sum(s2_result$flip_to_nonempirical), "\n")
