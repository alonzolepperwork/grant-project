# scrapes the public IES award pages for every grant in the universe and parses
# the cached HTML into grant metadata + product citations 
#
# what each page gives us: PI name, awardee institution, award year, project
# type, full title, program topic/name, and the product citations that feed
# Crossref DOI recovery (00b). the IES sitemap at /sitemap/awards.xml lists the
# award URLs; we fetch each, cache the HTML, and parse
#
# we parse the cache rather than the live sitemap because a per-sitemap-URL
# parse missed ~270 of the 528 grants. two reasons: (a) the post-2021 redesign
# moved the award number from a labeled field to a value-pair MODIFIER class, so
# a label-only parser returned NA and dropped the row, and (b) the live sitemap
# now returns a truncated list (~1,427 URLs) that omits many older awards whose
# HTML we already fetched and cached (luckily). the HTML cache (outputs/_cache/06b/page_html)
# is the authoritative record - ~3,078 pages covering 527 of 528 grants - so we
# fetch any new sitemap URLs into it, then parse every cached file with the
# modifier-aware reader
#
# one quirk: this sets ies_purpose to NA when it re-parsed the cache (covariates
# now come from the program topic/name via stage/part 03b, not the purpose text). the
# per-page purpose block is still in the cached HTML, so it could be recovered
# later if a keyword classifier needs it again
#
# inputs:
#   https://ies.ed.gov/sitemap/awards.xml?page=N   (paginated, cached)
#   outputs/_cache/06b/page_html/*.html            (HTML cache, authoritative)
#   outputs/02_build_current_grant_doi_universe/table_02_current_grant_universe.csv
# outputs:
#   outputs/06b_enrich_master_from_ies_live_scrape/table_05_ies_award_page_parsed.csv
#   outputs/06b_enrich_master_from_ies_live_scrape/table_06_dois_extracted_from_products.csv

#------------------
## 0. INITIALIZE ##
#------------------

library(tidyverse)
library(here)
library(rvest)
library(xml2)
library(curl)
library(digest)

mailto     <- "your_email_here@email.com"  # per polite-pool guidelines, set this to your email
user_agent <- "al-grant-to-policy/1.0 (mailto:your_email_here@email.com)"

clean_grant <- function(x) {
  x %>% as.character() %>% str_trim() %>% str_to_upper() %>%
    na_if("") %>% na_if("NA")
}

# progress-aware map_dfr drop-in: prints elapsed time, rate, and ETA each
# iteration so a stalled fetch loop is obvious
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

# fetch a URL with retries and a polite-pool user agent
fetch_html <- function(url, retries = 3) {
  for (attempt in seq_len(retries)) {
    h <- new_handle()
    handle_setheaders(h, `User-Agent` = user_agent)
    resp <- tryCatch(curl_fetch_memory(url, handle = h), error = function(e) NULL)
    if (!is.null(resp) && resp$status_code == 200) return(rawToChar(resp$content))
    Sys.sleep(2 ^ attempt)
  }
  NULL
}

# hero-block value off its modifier class (current layout, post-2021 redesign)
hero_by_mod <- function(doc, mod) {
  val <- html_text2(html_element(
    doc, sprintf(".ies-hero__value-pair--%s .ies-hero__value", mod)))
  if (length(val) == 0 || is.na(val) || !nzchar(str_trim(val))) return(NA_character_)
  str_squish(val)
}

# hero-block value off its label text (older layout fallback)
hero_by_label <- function(doc, label_text) {
  nodes <- html_elements(doc, ".ies-hero__value-pair")
  for (n in nodes) {
    lbl <- html_text2(html_element(n, ".ies-hero__label"))
    if (!is.na(lbl) && str_trim(lbl) == label_text) {
      return(str_squish(html_text2(html_element(n, ".ies-hero__value"))))
    }
  }
  NA_character_
}

# coarse Uni / Firm / Other from the awardee name
classify_inst <- function(name) {
  if (is.na(name) || !nzchar(name)) return(NA_character_)
  n <- str_to_lower(name)
  if (str_detect(n, "\\buniversity\\b|\\bcollege\\b")) return("Uni")
  if (str_detect(n, "\\binc\\.|\\bllc\\b|\\bcorporation\\b|\\bassociates\\b"))
    return("Firm")
  "Other"
}

#-----------------------------
## 1. CRAWL THE IES SITEMAP ##
#-----------------------------

# the sitemap is paginated under /sitemap/awards.xml?page=N; follow the pages
# until one returns no /use-work/awards/ URLs
sitemap_cache <- here("outputs", "_cache", "06b", "sitemap")
dir.create(sitemap_cache, showWarnings = FALSE, recursive = TRUE)

fetch_sitemap_page <- function(page) {
  cache_path <- file.path(sitemap_cache, paste0("page", page, ".xml"))
  if (file.exists(cache_path)) return(read_xml(cache_path))
  raw <- fetch_html(paste0("https://ies.ed.gov/sitemap/awards.xml?page=", page))
  if (is.null(raw)) return(NULL)
  writeLines(raw, cache_path)
  read_xml(cache_path)
}

all_award_urls <- character()
for (page in 1:30) {  # cap on pagination
  doc <- fetch_sitemap_page(page)
  if (is.null(doc)) break
  ns <- xml_ns(doc)
  urls <- xml_text(xml_find_all(doc, "//d1:loc", ns))
  urls <- urls[str_detect(urls, "/use-work/awards/")]
  if (length(urls) == 0) break
  all_award_urls <- c(all_award_urls, urls)
}
all_award_urls <- unique(all_award_urls)

# IES's sitemap adds its internal AWS hostname for some award pages
# those URLs aren't traceable from the internet and retry each before failing. drop them upfront
all_award_urls <- all_award_urls[str_detect(all_award_urls, "^https://ies\\.ed\\.gov/")]
cat("Award URLs discovered:", length(all_award_urls), "\n")

#-------------------------------------
## 2. FETCH + CACHE EACH AWARD PAGE ##
#-------------------------------------

# cache HTML per URL. cache hits are instant; fresh fetches pause and
# print progress + ETA. this grows the cache that section 4 parses
html_cache <- here("outputs", "_cache", "06b", "page_html")
dir.create(html_cache, showWarnings = FALSE, recursive = TRUE)

cache_one_page <- function(url) {
  html_path <- file.path(html_cache, paste0(digest(url, algo = "sha1"), ".html"))
  if (!file.exists(html_path)) {
    raw <- fetch_html(url)
    if (is.null(raw)) return(tibble(url = url, cached = FALSE))
    writeLines(raw, html_path)
    Sys.sleep(0.1)
  }
  tibble(url = url, cached = TRUE)
}

cat("Fetching/caching", length(all_award_urls), "award pages...\n")
fetch_log <- progress_map_dfr(all_award_urls, cache_one_page, label = "pages")
cat("  Cached OK:", sum(fetch_log$cached), "of", nrow(fetch_log), "\n")

#-----------------------------------
## 3. EXTRACT PRODUCT CITATIONS ##
#-----------------------------------

# the Products section lists the publications IES recorded as outputs of the
# grant. some carry a DOI in the citation text, some don't (those go to Crossref
# via 00b). pull these straight from the cached HTML
extract_product_citations <- function(url) {
  html_path <- file.path(html_cache, paste0(digest(url, algo = "sha1"), ".html"))
  if (!file.exists(html_path)) return(NULL)
  doc <- tryCatch(read_html(html_path), error = function(e) NULL)
  if (is.null(doc)) return(NULL)

  # find the Products/Publications heading then grab the citations below it
  sections <- html_elements(doc, "h2, h3")
  prod_section <- NULL
  for (s in sections) {
    if (str_detect(str_to_lower(html_text2(s)), "products|publications|publication")) {
      prod_section <- s
      break
    }
  }
  if (is.null(prod_section)) return(NULL)

  following <- html_elements(prod_section, xpath = "following::p")
  citations <- html_text2(following) %>% str_squish() %>% discard(~ nchar(.x) < 30)
  if (length(citations) == 0) return(NULL)

  award_num <- coalesce(hero_by_mod(doc, "contract-number"),
                        hero_by_label(doc, "Award number:"))
  tibble(
    grant_id = clean_grant(award_num),
    citation = citations,
    doi = str_extract(citations, "10\\.[0-9]{4,9}/[^\\s,]+")
  )
}

cat("Extracting product citations...\n")
product_citations <- progress_map_dfr(all_award_urls, extract_product_citations,
                                      label = "pages")

#----------------------------------
## 4. PARSE EVERY CACHED PAGE ##
#----------------------------------

# parse the FULL cache (not just this run's sitemap URLs) with the modifier-aware
# reader so we recover the older awards the live sitemap drops
parse_cached_file <- function(path) {
  doc <- tryCatch(read_html(path), error = function(e) NULL)
  if (is.null(doc)) return(NULL)

  award_num <- coalesce(hero_by_mod(doc, "contract-number"),
                        hero_by_label(doc, "Award number:"))
  if (is.na(award_num)) return(NULL)   # not an award page

  award_period <- coalesce(hero_by_mod(doc, "award-period"),
                           hero_by_label(doc, "Award period:"))
  awardee <- coalesce(hero_by_mod(doc, "awardee"), hero_by_label(doc, "Awardee:"))

  # canonical URL straight off the page so the master keeps a working link
  url <- html_attr(html_element(doc, "link[rel='canonical']"), "href")
  if (is.na(url))
    url <- html_attr(html_element(doc, "meta[property='og:url']"), "content")

  tibble(
    grant_id = clean_grant(award_num),
    ies_title = str_squish(html_text2(html_element(doc, "h1.ies-hero__heading"))),
    ies_pi = coalesce(hero_by_mod(doc, "prime-investigator"),
                      hero_by_label(doc, "Principal investigator:")),
    ies_awardee = awardee,
    ies_project_type = coalesce(hero_by_mod(doc, "project-type"),
                                hero_by_label(doc, "Project type:")),
    ies_award_number = award_num,
    ies_year = suppressWarnings(as.integer(
      coalesce(hero_by_label(doc, "Year:"), str_extract(award_period, "20[0-2][0-9]")))),
    ies_award_period = award_period,
    ies_purpose = NA_character_,   # see header note - dropped on re-parse
    ies_award_amount_text = coalesce(hero_by_mod(doc, "award-amount"),
                                     hero_by_label(doc, "Award amount:")),
    ies_program_topic = coalesce(hero_by_mod(doc, "program-topic"),
                                 hero_by_label(doc, "Program topic(s):")),
    ies_program_name = coalesce(hero_by_mod(doc, "program"),
                                hero_by_label(doc, "Program:")),
    ies_award_page_url = url,
    ies_institution_type_from_name = classify_inst(awardee)
  )
}

files <- list.files(html_cache, pattern = "html$", full.names = TRUE)
cat("Parsing", length(files), "cached award pages...\n")
parsed <- map_dfr(files, parse_cached_file)

# some award numbers were fetched more than once; keep one row per grant,
# preferring the most complete
parsed <- parsed %>%
  filter(!is.na(grant_id)) %>%
  mutate(n_filled = rowSums(!is.na(across(everything())))) %>%
  arrange(grant_id, desc(n_filled)) %>%
  distinct(grant_id, .keep_all = TRUE) %>%
  select(-n_filled)

#-------------------
## 5. WRITE OUT ##
#-------------------

# restrict the parsed table to the analytic universe so it matches what stage 03
# joins against, but report total parsed for sanity
universe <- read_csv(
  here("outputs", "02_build_current_grant_doi_universe",
       "table_02_current_grant_universe.csv"),
  show_col_types = FALSE
) %>% mutate(grant_id = clean_grant(grant_id))

parsed_universe <- parsed %>% semi_join(universe, by = "grant_id")

out_dir <- here("outputs", "06b_enrich_master_from_ies_live_scrape")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(parsed_universe, file.path(out_dir, "table_05_ies_award_page_parsed.csv"))
write_csv(product_citations,
          file.path(out_dir, "table_06_dois_extracted_from_products.csv"))

cat("\nIES award-page scrape + parse done.\n")
cat("  Distinct award pages parsed:", nrow(parsed), "\n")
cat("  Universe grants covered:    ", nrow(parsed_universe), "of", nrow(universe), "\n")
cat("  With awardee:               ", sum(!is.na(parsed_universe$ies_awardee)), "\n")
cat("  With PI:                    ", sum(!is.na(parsed_universe$ies_pi)), "\n")
cat("  With project type:          ", sum(!is.na(parsed_universe$ies_project_type)), "\n")
cat("  Product citations:          ", nrow(product_citations), "\n")
cat("  Citations with a DOI:       ", sum(!is.na(product_citations$doi)), "\n")
