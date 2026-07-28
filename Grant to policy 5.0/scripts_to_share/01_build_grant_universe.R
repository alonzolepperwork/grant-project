# builds the grant-to-DOI universe used in every downstream analysis - combines
# the original IES DOI file with the ERIC R3 recovery, manual missing-DOI fills,
# and a Crossref pass for citations on IES award pages that didn't have a DOI in
# the text. then attaches publication metadata from a local mega file (with
# optional OpenAlex fallback) and trims out meta-analyses and other
# non-empirical publication types

#--------------------------------
## 0. INITIALIZE ##
#--------------------
 
#load libraries
library(tidyverse)
library(here)
library(stringi)
library(readxl)

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

# tiny helpers we use throughout
  
# normalize grant IDs - strip whitespace, uppercase, drop any "."
clean_grant <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_to_upper() %>%
    na_if("") %>%
    na_if("NA")
}

# normalize DOIs - lowercase, strip the doi.org/ prefix if present
norm_doi <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_remove("^https?://doi\\.org/") %>%
    str_remove("^doi:") %>%
    str_trim() %>%
    na_if("") %>%
    na_if("na")
}

# pick the first column name that exists in a df (data files have changed
# column names across snapshots - this is defensive against that)
pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

# extract a 4-digit year from a string or numeric
extract_year <- function(x) {
  as.character(x) %>%
    str_extract("19[5-9][0-9]|20[0-3][0-9]") %>%
    as.integer()
}

# coalesce-first - take the first non-missing value across grouped rows
first_nonmissing <- function(x) {
  hit <- x[!is.na(x) & x != ""]
  if (length(hit) == 0) return(NA)
  hit[1]
}

#------------------------------------
## 1. ORIGINAL BASELINE PAIRS ##
#---------------------------------

# start with the original IES "DOI list with grant numbers" file. this
# is row-level (one row per study) and gets us our anchor pairs for grant + doi. the column
# names have been edited by hand in past data sets so we pick defensively
  
orig_raw <- read_csv(
  here("data", "01_legacy_original_inputs",
       "IES at 20 Publications(List of DOIs) with grant numbers.csv"),
  show_col_types = FALSE
)

# column names changed across data sets - "Study Grant number" vs "study_grant_number"
grant_col <- pick_col(orig_raw, c("Study Grant number", "study_grant_number"))
doi_col <- pick_col(orig_raw, c("DOI", "doi"))
stopifnot(!is.na(grant_col), !is.na(doi_col))

baseline_pairs <- orig_raw %>%
  transmute(
    grant_id = clean_grant(.data[[grant_col]]),
    doi = norm_doi(.data[[doi_col]])
  ) %>%
  filter(!is.na(grant_id), !is.na(doi)) %>%
  distinct()

# now the column names are standardized to: "grant_id" and "doi" 
#nrow(baseline_pairs) #1044

#also pull grant attributes from the master PI sheet. later stages join on
#grant_id for PI / institution / year / program data. column names vary so
#we again pick defensively

pi_raw <- read_csv(
  here("data", "01_legacy_original_inputs",
       "Copy of Master_PI_Sheet(Mail Merge).csv"),
  show_col_types = FALSE
)

#col_or_na returns the named column if it exists, else a vector of NAs
#this lets the registry build proceed even when an optional column is
#missing from a particular snapshot of the PI sheet
col_or_na <- function(df, col) {
  if (is.na(col) || !(col %in% names(df))) rep(NA, nrow(df)) else df[[col]]
}

# the actual PI sheet only has 5 columns: Last Name, First Name,
# Grant Number, Grant Name, Year Funded Grant. so we can only pull
# grant_id, title (from Grant Name), award_year (from Year Funded Grant),
# and pi (combining Last + First). everything else (institution,
# program, goal_text, institution_type) we leave as NA here and let
# the IES live-scrape in stage 03 fill them in

baseline_registry <- pi_raw %>%
  transmute(
    grant_id = clean_grant(col_or_na(pi_raw, pick_col(pi_raw,
              c("Grant Number", "grant_id", "Grant ID", "AwardNum")))),
    title = col_or_na(pi_raw, pick_col(pi_raw,
              c("Grant Name", "Title", "Project Title"))),
    award_year = extract_year(col_or_na(pi_raw, pick_col(pi_raw,
              c("Year Funded Grant", "Year", "Award Year")))),
    # PI is split across two columns - paste them together
    pi = {
      first_name <- col_or_na(pi_raw, pick_col(pi_raw, c("First Name", "FirstName")))
      last_name  <- col_or_na(pi_raw, pick_col(pi_raw, c("Last Name", "LastName")))
      combined <- paste(first_name, last_name) %>% str_squish()
      combined[combined == "NA NA" | combined == ""] <- NA
      combined
    },
    # placeholders for fields we'll fill in from IES scrape later
    institution = NA_character_,
    program_name = NA_character_,
    goal_text = NA_character_,
    institution_type = NA_character_
  ) %>%
  filter(!is.na(grant_id)) %>%
  group_by(grant_id) %>%
  summarize(across(everything(), first_nonmissing), .groups = "drop")

#sanity check that we got most grants
 baseline_registry %>% summarize(across(everything(), ~ sum(!is.na(.))))

#-------------------------------------------------
## 2. EXPAND TO THE CURRENT 528-GRANT UNIVERSE ##
#-------------------------------------------------

#the current universe is the union of:
#  - the baseline pairs above
#  - ERIC R3 recovery (a re-run of ERIC against all eval-eligible grants)
#  - manual fills for grants ERIC couldn't find
#  - a Crossref bibliographic-search pass on citations from IES award pages
#    that didn't have a DOI in the citation text
#
#the canonical reference is the saved CANONICAL map that gets re-saved at
#the end of each iteration. we read it last and trust it as authoritative

#the "eval set" file is one of several sources of grant IDs. the
#authoritative 528-grant anchor is the UNION of grant IDs across all
#sources: the eval file, the baseline 1044 file, the manual fills, the
#augmented pool, the saved canonical map, and the Crossref recoveries
#we union them all below; the eval file just gives us the first chunk
eval_set <- read_csv(
  pick_input("eval_grants_UNION_DOIfile_plus_ERIC_R3.csv"),
  show_col_types = FALSE
) %>%
  transmute(grant_id = clean_grant(.data[[pick_col(.,
            c("grant_id", "AwardNum", "Grant Number"))]])) %>%
  filter(!is.na(grant_id)) %>%
  distinct() %>%
  pull(grant_id)
#length(eval_set) #456 - chunk one

r3_pairs <- read_csv(
  pick_input("r3_pairs_eval_only.csv"),
  show_col_types = FALSE
) %>%
  transmute(grant_id = clean_grant(.data[[pick_col(., c("grant_id", "AwardNum"))]]),
            doi = norm_doi(.data[[pick_col(., c("doi", "DOI"))]])) %>%
  filter(!is.na(grant_id), !is.na(doi)) %>%
  distinct()

manual_pairs <- read_csv(
  pick_input("missing_grants_with_manual_dois.csv"),
  show_col_types = FALSE
) %>%
  transmute(grant_id = clean_grant(.data[[pick_col(., c("grant_id", "AwardNum"))]]),
            doi = norm_doi(.data[[pick_col(., c("doi", "DOI"))]])) %>%
  filter(!is.na(grant_id), !is.na(doi)) %>%
  distinct()

#some grants STILL had no DOI even after manual recovery. we keep them in
#the grant universe (so coverage analyses count them) but they
#contribute no pairs

no_doi_grants <- read_csv(
  pick_input("missing_grants_still_no_doi.csv"),
  show_col_types = FALSE
) %>%
  transmute(grant_id = clean_grant(.data[[pick_col(., c("grant_id", "AwardNum"))]])) %>%
  filter(!is.na(grant_id)) %>% pull(grant_id) %>% unique()

canonical_pairs <- read_csv(
  pick_input("grant_doi_map_CANONICAL_with_recovered.csv"),
  show_col_types = FALSE
) %>%
  transmute(grant_id = clean_grant(.data[[pick_col(., c("grant_id", "AwardNum"))]]),
            doi = norm_doi(.data[[pick_col(., c("doi", "DOI"))]])) %>%
  filter(!is.na(grant_id), !is.na(doi)) %>%
  distinct()

#Crossref bibliographic-search recovery: for citations on IES award pages
#that didn't have a DOI in the text, we sent the citation text to Crossref's
#bibliographic search and accepted only year-matched + Jaccard >= 0.5 matches
#that's stage 06h's output - 517 high-confidence pairs. we treat them as
#additive (only added if not already in the union)

crossref_path <- here("outputs", "06h_lookup_product_dois_via_crossref",
                      "table_02_high_confidence_matches.csv")
if (file.exists(crossref_path)) {
  crossref_pairs <- read_csv(crossref_path, show_col_types = FALSE) %>%
    transmute(grant_id = clean_grant(grant_id),
              doi = norm_doi(candidate_doi)) %>%
    filter(!is.na(grant_id), !is.na(doi)) %>%
    distinct()
} else {
  crossref_pairs <- tibble(grant_id = character(), doi = character())
}

#augmented pool: manual "MISSING_GRANT_LYDIA_DOI" fills (77 rows) for ~16
#older grants where automated recovery (ERIC, OpenAlex, Crossref) came up
#empty, PLUS a snapshot of legacy ERIC R3 pairs (2,556 rows) that includes
#some grants/DOIs the current IES ERIC fetch no longer returns. legacy 02
#reads the full file; without it our universe loses ~16 reached grants and
#~290 policy docs. dedup against our other sources happens in the bind_rows
#group_by below, so being broad here is safe
augmented_pairs <- read_csv(
  pick_input("r3_grant_doi_pool_augmented_with_missing.csv"),
  show_col_types = FALSE
) %>%
  transmute(grant_id = clean_grant(.data[[pick_col(., c("grant_id", "AwardNum"))]]),
            doi = norm_doi(.data[[pick_col(., c("doi", "DOI"))]])) %>%
  filter(!is.na(grant_id), !is.na(doi)) %>%
  distinct()

#union everything, tagging each pair with the source(s) it came from. the
#source flags are useful later for audit ("which pairs are we trusting only
#Crossref for?") and for the source_route column in the 528-anchor table

pair_union <- bind_rows(
  baseline_pairs %>% mutate(src_original = TRUE),
  r3_pairs %>% mutate(src_eric_r3 = TRUE),
  manual_pairs %>% mutate(src_manual = TRUE),
  canonical_pairs %>% mutate(src_canonical = TRUE),
  crossref_pairs %>% mutate(src_crossref = TRUE),
  augmented_pairs %>% mutate(src_lydia_fill = TRUE)
) %>%
  group_by(grant_id, doi) %>%
  summarize(
    src_original = any(src_original, na.rm = TRUE),
    src_eric_r3 = any(src_eric_r3, na.rm = TRUE),
    src_manual = any(src_manual, na.rm = TRUE),
    src_canonical = any(src_canonical, na.rm = TRUE),
    src_crossref = any(src_crossref, na.rm = TRUE),
    src_lydia_fill = any(src_lydia_fill, na.rm = TRUE),
    .groups = "drop"
  )

#the 528-grant universe is anchored to grant_universe_528_anchor.csv
#(extracted from the legacy 528-grant universe table). this is the
#canonical analytic frame - DOIs from any source are then layered in
#below. 72 of the 528 don't have a DOI in any current source; they sit
#in the universe with has_doi=FALSE for honest coverage accounting
anchor_path <- pick_input("grant_universe_528_anchor.csv")
if (file.exists(anchor_path)) {
  anchor_grants <- read_csv(anchor_path, show_col_types = FALSE) %>%
    pull(grant_id) %>% clean_grant() %>% unique() %>% na.omit()
} else {
  anchor_grants <- character(0)
}

#union the anchor with every observed source so that any newly recovered
#grant (Crossref, OpenAlex, etc.) automatically expands the universe
all_known_grants <- unique(c(
  anchor_grants,
  eval_set,
  baseline_pairs$grant_id,
  r3_pairs$grant_id,
  manual_pairs$grant_id,
  canonical_pairs$grant_id,
  crossref_pairs$grant_id,
  augmented_pairs$grant_id,
  no_doi_grants
))
#length(all_known_grants) #528

pair_union <- pair_union %>% filter(grant_id %in% all_known_grants)
#nrow(pair_union) #3212 before filters

#----------------------------------
## 3. ATTACH PUBLICATION METADATA ##
#----------------------------------

#every DOI in the universe gets title / source title / publication year /
#type / citation count from a local mega file (merged_df_IES_mega). this
#used to support a live API fallback but we keep it local-only here -
#OpenAlex enrichment lives in its own stage (07_enrich_via_openalex)

mega_raw <- read_csv(
  here("data", "01_legacy_original_inputs", "merged_df_IES_mega.csv"),
  show_col_types = FALSE
)

mega_doi <- pick_col(mega_raw, c("doi", "dois_cited"))
mega_year <- pick_col(mega_raw, c("year_published", "date_published_year"))
mega_type <- pick_col(mega_raw, c("type", "publication_type", "doc_type"))

pub_metadata <- mega_raw %>%
  transmute(
    doi = norm_doi(.data[[mega_doi]]),
    publication_title = .data[[pick_col(mega_raw, "title")]],
    publication_source_title = .data[[pick_col(mega_raw, "source_title")]],
    publication_year = extract_year(.data[[mega_year]]),
    publication_type = .data[[mega_type]],
    scholarly_citations_count = .data[[pick_col(mega_raw,
                                "scholarly_citations_count")]] %>% as.integer()
  ) %>%
  filter(!is.na(doi)) %>%
  group_by(doi) %>%
  summarize(across(everything(), first_nonmissing), .groups = "drop") %>%
  semi_join(pair_union, by = "doi")

#-----------------------------------------
## 3b. MANUAL DOI METADATA FILL-INS ##
#-----------------------------------------

# OpenAlex and Lens together cover ~99% of universe DOIs but a handful
# slip through - usually older NBER working papers, Zenodo preprints, or
# non-standard DOI registrars OpenAlex hasn't indexed. for each one we
# hand-enter the metadata so downstream code (timing lags, classifier,
# dashboard tooltips) has a complete record
#
# how to add a new entry: hit the publisher page for the DOI, copy the
# title / year / type, jot a note explaining why we had to hand-fill it

manual_pub_metadata <- tribble(
  ~doi,             ~publication_title,                                                                                                                  ~publication_source_title,    ~publication_year, ~publication_type, ~scholarly_citations_count, ~note,
  "10.3386/w17112", "High-School Exit Examinations and the Schooling Decisions of Teenagers: A Multi-Dimensional Regression-Discontinuity Analysis", "NBER Working Paper Series", 2011L,             "report",          4L,                          "NBER working paper, not in OpenAlex - Dee & Jacob 2011, grant R305E100013"
  # add more rows below as gaps come up; one row per missing DOI
)

# fold the manual fills into the OpenAlex/Lens-derived metadata. manual
# entries take precedence on conflicts since we curated them by hand
pub_metadata <- pub_metadata %>%
  filter(!doi %in% manual_pub_metadata$doi) %>%
  bind_rows(manual_pub_metadata %>% select(-note))

cat("Manual metadata entries applied:", nrow(manual_pub_metadata), "\n")

#-----------------------------------
## 4. FILTER NON-EMPIRICAL DOIS ##
#-----------------------------------

#drop publications that aren't original empirical research from the universe
#we do this in two passes:
#  (a) title-pattern: catches things IES itself listed as products that are
#      actually meta-analyses or systematic reviews. researchers sometimes
#      list their own syntheses as products of the grant
#  (b) OpenAlex type field: catches OpenAlex-typed reviews/letters/editorials
#      that slipped past the title filter
#
#we deliberately do NOT drop type == "book" / "book-chapter" / "report" /
#"preprint" - in education research many full empirical studies show up
#under those types (RAND empirical reports, NBER working papers, RCTs as
#book chapters in edited volumes). OpenAlex's typing is unreliable here

#non-empirical detection. the goal here is recall on the long tail of
#first-round products that aren't primary empirical findings papers. the
#original filter only caught explicit syntheses (meta-analyses, systematic
#reviews) plus whatever OpenAlex happened to type "review"/"letter". that
#undercounts badly: a commentary, a study protocol, or a purely conceptual
#piece typed "article" by OpenAlex wrongly got placed into the empirical
#bucket. since the paper contrasts empirical vs non-empirical reach, the
#non-empirical count has to be meaningful, so we widen the net
#
#we screen titles AND abstracts against four families of non-empirical
#writing and combine that with OpenAlex's type field. to keep precision
#high (we must NOT mislabel a real RCT report), the three "soft" families
#- protocol, commentary, conceptual - only fire when the abstract ALSO
#lacks primary-study markers (random assignment, a sample, reported
#results, effect sizes). explicit syntheses and OpenAlex review/letter
#types are reliable enough to fire on their own, as before

#(1) syntheses - secondary research that reviews or pools existing
#    evidence. the original paper non-empirical class; fires alone
synthesis_patterns <- c(
  "meta[- ]?analy", "systematic review", "best[- ]?evidence synthesis",
  "scoping review", "narrative review", "umbrella review",
  "literature review", "research synthesis", "evidence synthesis",
  "rapid review", "review of (the )?(literature|research|evidence)"
)
#(2) protocols / study designs - describe a planned study, no findings yet
protocol_patterns <- c(
  "study protocol", "trial protocol", "protocol for (a|an|the)",
  "rationale and design",
  "design of (a|an|the) (study|trial|intervention|evaluation|experiment)",
  "study design and (rationale|methods)", "design and rationale",
  "registered report", "describes the design of"
)
#(3) commentaries / editorials / perspectives - opinion or framing pieces
commentary_patterns <- c(
  "\\bcommentary\\b", "this editorial", "\\beditorial\\b", "book review",
  "invited (commentary|response)", "this (perspective|viewpoint|essay)",
  "we comment on", "\\brejoinder\\b", "\\bforeword\\b",
  "introduction to the special (issue|section)"
)
#(4) conceptual / theoretical - propose a framework with no new data
#    note we require the paper to SELF-IDENTIFY as conceptual ("this
#    theoretical article", "a conceptual framework for ...", "position
#    paper") rather than just mentioning a framework. bare "conceptual
#    framework" is too weak - plenty of empirical studies use one - and
#    flagging on it produced false positives (intervention and measurement
#    studies whose garbled OpenAlex abstract hid the empirical markers)
conceptual_patterns <- c(
  "this (conceptual|theoretical) (article|paper|piece|essay|review)",
  "is a (conceptual|theoretical) (article|paper|piece|essay)",
  "we (propose|present|offer|develop|advance) a (conceptual|theoretical)",
  "a conceptual framework for", "a theoretical framework for",
  "position paper", "think piece"
)

#primary-study markers. if any show up in title or abstract we treat the
#paper as empirical regardless of the soft signals above - this is the
#precision guard that keeps real studies (which sometimes ALSO mention a
#"framework" or "design") in the empirical bucket. kept broad on purpose:
#I would rather leave a borderline non-empirical paper in than pull a
#real study out
empirical_markers <- c(
  "random(ly)? (assign|allocat)", "randomi[sz]ed", "\\bparticipants\\b",
  "we (recruited|enrolled|sampled|surveyed|administered|examined|tested|investigated|assessed|measured|analy[sz]ed|compared|evaluated)",
  "sample of", "\\bn ?= ?[0-9]", "treatment (group|condition)",
  "control (group|condition)", "intervention group", "comparison group",
  "pre[- ]?test", "post[- ]?test", "effect size", "data were (collected|coded|analy)",
  "we (find|found|observe|estimate)", "were (recruited|enrolled|randomi)",
  "results (show|showed|indicate|reveal|suggest|demonstrate)",
  "findings (show|showed|indicate|reveal|suggest)",
  "regression", "students were", "were interviewed", "were observed",
  "coded", "standardi[sz]ed (test|measure|assessment)"
)
re_of <- function(v) regex(paste(v, collapse = "|"), ignore_case = TRUE)

#abstracts live in the OpenAlex enrichment output (stage 07). keep NA where
#missing so the Europe PMC / Semantic Scholar abstracts can fill the gap below
abstract_path <- here("outputs", "openalex_enrichment", "doi_metadata.csv")
if (file.exists(abstract_path)) {
  abstracts <- read_csv(abstract_path, show_col_types = FALSE) %>%
    transmute(doi = norm_doi(doi),
              abstract = na_if(str_trim(replace_na(as.character(abstract), "")), ""),
              oa_type  = replace_na(as.character(type), "")) %>%
    distinct(doi, .keep_all = TRUE)
} else {
  abstracts <- tibble(doi = character(), abstract = character(),
                       oa_type = character())
}

#Europe PMC (stage 00i) and Semantic Scholar (stage 00j) add two things for the
#universe DOIs: abstracts OpenAlex was missing, and curated publication-type
#tags. we use the tags as an authoritative non-empirical signal, but only the
#RELIABLE ones - EPMC's curated MeSH pubtypes, and S2's MetaAnalysis / Editorial
#/ LettersAndComments. we deliberately DROP S2's bare "Review" tag: spot-checks
#showed it slaps "Review" on plenty of primary studies (charter-school turnover,
#ed-tech reading effects), so it manufactures false non-empirical flips. a
#trial/clinical-trial tag from either source is a positive empirical
#confirmation that vetoes a flip
#Europe PMC: curated MeSH pubtypes are reliable in both directions
epmc_path <- here("outputs", "_universe_filter_audit", "universe_epmc_pubtypes.csv")
if (file.exists(epmc_path)) {
  epmc_pt <- read_csv(epmc_path, show_col_types = FALSE) %>%
    transmute(doi = norm_doi(doi), ext_abstract = epmc_abstract,
              pubtype_emp = epmc_emp, pubtype_nonemp = epmc_nonemp & !epmc_emp,
              pubtype_source = if_else(epmc_nonemp & !epmc_emp,
                str_c("Europe PMC pubType: ", epmc_pubtypes), NA_character_))
} else epmc_pt <- NULL

#Semantic Scholar: reliable tags only (MetaAnalysis / Editorial /
#LettersAndComments). bare "Review" is dropped - it over-tags primary studies
s2_path <- here("outputs", "_universe_filter_audit", "universe_s2_pubtypes.csv")
if (file.exists(s2_path)) {
  s2_pt <- read_csv(s2_path, show_col_types = FALSE) %>%
    mutate(doi = norm_doi(doi),
           s2_reliable = str_detect(replace_na(s2_pubtypes, ""),
                                    "MetaAnalysis|Editorial|LettersAndComments")) %>%
    transmute(doi, ext_abstract = s2_abstract, pubtype_emp = s2_emp,
              pubtype_nonemp = s2_reliable & !s2_emp,
              pubtype_source = if_else(s2_reliable & !s2_emp,
                str_c("Semantic Scholar pubType: ", s2_pubtypes), NA_character_))
} else s2_pt <- NULL

#Crossref (stage 00k): abstracts only - Crossref's type field is too coarse to
#classify, so it carries no pubtype signal, just more text for the heuristic
crossref_path <- here("outputs", "_universe_filter_audit", "universe_crossref_abstracts.csv")
if (file.exists(crossref_path)) {
  crossref_pt <- read_csv(crossref_path, show_col_types = FALSE) %>%
    filter(!is.na(crossref_abstract)) %>%
    transmute(doi = norm_doi(doi), ext_abstract = crossref_abstract,
              pubtype_emp = FALSE, pubtype_nonemp = FALSE,
              pubtype_source = NA_character_)
} else crossref_pt <- NULL

#consolidate per DOI. a positive trial tag from any source vetoes the flag
#abstract source priority follows bind order: Europe PMC, then S2, then Crossref
ext_meta <- bind_rows(epmc_pt, s2_pt, crossref_pt)
if (nrow(ext_meta) > 0) {
  ext_meta <- ext_meta %>%
    group_by(doi) %>%
    summarize(ext_abstract = first(ext_abstract[!is.na(ext_abstract)]),
              pubtype_emp = any(pubtype_emp, na.rm = TRUE),
              pn = any(pubtype_nonemp, na.rm = TRUE),
              pubtype_source = first(pubtype_source[!is.na(pubtype_source)]),
              .groups = "drop") %>%
    mutate(pubtype_nonemp = pn & !pubtype_emp) %>%
    select(doi, ext_abstract, pubtype_nonemp, pubtype_emp, pubtype_source)
} else {
  ext_meta <- tibble(doi = character(), ext_abstract = character(),
                     pubtype_nonemp = logical(), pubtype_emp = logical(),
                     pubtype_source = character())
}

#OpenAlex types we treat as non-empirical on their own. typing is reliable
#for these even though it's unreliable for book/report/preprint (which we
#deliberately keep, since many full empirical studies appear under them)
nonempirical_types <- c("review", "letter", "editorial",
                        "paratext", "reference-entry")

#one classification row per universe DOI: title + abstract + type, each
#non-empirical family flagged, the empirical guard, and a single reason
#string for the audit. the soft families are ANDed with !has_emp_marker
#
#anchor on the full universe DOI set (pair_union), NOT pub_metadata - some
#DOIs are in OpenAlex but not in the local mega metadata file, and we still
#need their OpenAlex type to catch review/letter papers. titles come from
#pub_metadata where present, abstract + type from the OpenAlex enrichment
doi_class <- pair_union %>%
  distinct(doi) %>%
  left_join(pub_metadata %>% select(doi, publication_title), by = "doi") %>%
  left_join(abstracts, by = "doi") %>%
  left_join(ext_meta, by = "doi") %>%
  mutate(
    title = replace_na(publication_title, ""),
    #fill a missing OpenAlex abstract from Europe PMC / Semantic Scholar so the
    #text heuristics below run on as many DOIs as possible
    abstract = coalesce(abstract, ext_abstract, ""),
    oa_type = replace_na(oa_type, ""),
    pubtype_nonemp = replace_na(pubtype_nonemp, FALSE),
    pubtype_emp = replace_na(pubtype_emp, FALSE),
    txt = str_c(title, " ", abstract),
    has_emp_marker = str_detect(abstract, re_of(empirical_markers)),
    is_synthesis  = str_detect(txt, re_of(synthesis_patterns)),
    is_protocol   = str_detect(txt, re_of(protocol_patterns))   & !has_emp_marker,
    is_commentary = str_detect(txt, re_of(commentary_patterns)) & !has_emp_marker,
    is_conceptual = str_detect(txt, re_of(conceptual_patterns)) & !has_emp_marker,
    is_oa_nonemp  = oa_type %in% nonempirical_types,
    #positive empirical confirmation (EPMC RCT/trial or S2 ClinicalTrial)
    empirical_confirmed = pubtype_emp,
    nonempirical_reason = case_when(
      is_synthesis    ~ "synthesis / review",
      is_oa_nonemp    ~ str_c("OpenAlex type: ", oa_type),
      pubtype_nonemp  ~ coalesce(pubtype_source, "curated pubType: non-empirical"),
      is_protocol     ~ "study protocol / design",
      is_commentary   ~ "commentary / editorial",
      is_conceptual   ~ "conceptual / theoretical",
      TRUE            ~ NA_character_
    ),
    #narrow meta-analysis flag for the SREE-paper's stricter cut. note this
    #is now distinct from the broad non-empirical class - a systematic
    #review is non-empirical but is NOT a meta-analysis
    is_meta_analysis = str_detect(txt, regex("meta[- ]?analy", ignore_case = TRUE))
  )

meta_dois <- doi_class %>% filter(is_meta_analysis) %>% pull(doi)
nonempirical_dois <- doi_class %>% filter(!is.na(nonempirical_reason)) %>% pull(doi)
reason_lookup <- doi_class %>% distinct(doi, nonempirical_reason, empirical_confirmed)

#manual off-topic exclusion list. built by stage 14 from the
#offtopic-DOI audit, and editable by hand to add/remove specific DOIs
#anything with decision == "exclude" (the default) is dropped here
#set decision == "keep" on a row to put it back into the universe
manual_excl_path <- here("data", "_manual_review",
                         "offtopic_exclusion_candidates.csv")
if (file.exists(manual_excl_path)) {
  manual_excl <- read_csv(manual_excl_path, show_col_types = FALSE) %>%
    filter(decision == "exclude") %>%
    pull(doi) %>% unique()
} else {
  manual_excl <- character()
}

#pre-award publication exclusions: grant-DOI pairs whose publication is dated
#before the grant's award year - a structural impossibility (a paper cannot
#predate its own funding), so the pairing is a data error. built by the
#pre-award audit into a committed list (award_year/pub_year are not available
#this early in the pipeline, so the list is precomputed from the master +
#OpenAlex). removed at the PAIR level: a multi-grant DOI that is post-award for
#another grant is only de-attributed from the grant it predates, not deleted
preaward_path <- here("data", "_rewrite_outputs", "preaward_pub_exclusions.csv")
if (file.exists(preaward_path)) {
  preaward_pairs <- read_csv(preaward_path, show_col_types = FALSE) %>%
    transmute(grant_id = clean_grant(grant_id), doi = norm_doi(doi))
} else {
  preaward_pairs <- tibble(grant_id = character(), doi = character())
}

#every DOI gets:
#  is_meta_analysis     - title/abstract literally says "meta-analysis"
#                         (the narrow SREE-paper definition)
#  is_empirical         - FALSE if the paper is a synthesis, protocol,
#                         commentary, conceptual piece, or OpenAlex
#                         review/letter type. TRUE for everything else -
#                         primary studies, RCT reports, observational work
#  nonempirical_reason  - which rule fired (NA when empirical), carried
#                         through so downstream and the audit can see why
#
#we keep both kinds in the universe (only the manual off-topic list
#hard-drops DOIs) so the paper can report reach stratified rather than
#silently excluding non-empirical work

pair_union_filtered <- pair_union %>%
  filter(!doi %in% manual_excl) %>%
  anti_join(preaward_pairs, by = c("grant_id", "doi")) %>%
  left_join(reason_lookup, by = "doi") %>%
  mutate(is_meta_analysis = doi %in% meta_dois,
         is_empirical = !doi %in% nonempirical_dois)

#DOIs that left the universe ENTIRELY via the pre-award filter (all their
#pairings were pre-award) - drop these from the per-DOI metadata too. the
#de-attributed multi-grant DOIs survive here because they remain in pair_union
removed_preaward_dois <- setdiff(
  intersect(preaward_pairs$doi, pair_union$doi),
  pair_union_filtered$doi
)
pub_metadata_filtered <- pub_metadata %>%
  filter(!doi %in% manual_excl, !doi %in% removed_preaward_dois) %>%
  left_join(reason_lookup, by = "doi") %>%
  mutate(is_meta_analysis = doi %in% meta_dois,
         is_empirical = !doi %in% nonempirical_dois)

#audit table: per pair, the keep/drop decision and the reason that fired
dropped_audit <- pair_union %>%
  mutate(is_preaward = paste(grant_id, doi) %in%
           paste(preaward_pairs$grant_id, preaward_pairs$doi)) %>%
  left_join(doi_class %>% select(doi, publication_title, oa_type,
                                 nonempirical_reason),
            by = "doi") %>%
  mutate(
    decision = case_when(
      is_preaward                 ~ "DROPPED - pre-award publication",
      doi %in% manual_excl        ~ "DROPPED - off-topic (manual)",
      !is.na(nonempirical_reason) ~ str_c("KEPT - non-empirical (",
                                          nonempirical_reason, ")"),
      TRUE                        ~ "KEPT - empirical (primary study)"
    )
  )

#-----------------------------------
## 5. GRANT-LEVEL UNIVERSE TABLE ##
#-----------------------------------

#one row per grant in the eval set. includes grants with zero pairs (so
#that downstream coverage analyses count them)

grant_universe <- tibble(grant_id = all_known_grants) %>%
  left_join(
    pair_union_filtered %>%
      group_by(grant_id) %>%
      summarize(n_current_dois = n_distinct(doi), .groups = "drop"),
    by = "grant_id"
  ) %>%
  mutate(
    n_current_dois = replace_na(n_current_dois, 0L),
    has_doi = n_current_dois > 0,
    has_no_doi_after_recovery = grant_id %in% no_doi_grants
  ) %>%
  left_join(baseline_registry, by = "grant_id")

#sum(grant_universe$has_doi) #480 grants with a DOI

#----------------------
## 6. WRITE OUTPUTS ##
#----------------------

#the downstream pipeline reads from these exact paths so we preserve them
#even though the script is consolidated

out_dir <- here("outputs", "02_build_current_grant_doi_universe")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(pair_union_filtered,
          file.path(out_dir, "table_01_current_grant_doi_pair_union.csv"))
write_csv(grant_universe,
          file.path(out_dir, "table_02_current_grant_universe.csv"))

#metadata goes to the 02b path (downstream stages join from there)
pubmeta_dir <- here("outputs", "02b_build_publication_metadata")
dir.create(pubmeta_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(pub_metadata_filtered,
          file.path(pubmeta_dir, "table_02_publication_metadata_combined.csv"))

#filter audit gets its own folder so the chronology is visible
filter_dir <- here("outputs", "_universe_filter_audit")
dir.create(filter_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(dropped_audit, file.path(filter_dir, "dropped_pairs_audit.csv"))

#focused review artifact: one row per DOI flagged non-empirical, with the
#title, OpenAlex type, and the rule that fired. this is the file to skim
#when sanity-checking the empirical/non-empirical split - every reclassify
#is one line, sorted by reason so each family reads together
nonempirical_review <- doi_class %>%
  filter(!is.na(nonempirical_reason)) %>%
  semi_join(pair_union_filtered, by = "doi") %>%
  transmute(doi, publication_title, oa_type, nonempirical_reason,
            is_meta_analysis) %>%
  arrange(nonempirical_reason, publication_title)
write_csv(nonempirical_review,
          file.path(filter_dir, "nonempirical_classification.csv"))

#count by reason so the console run shows how the non-empirical bucket
#breaks down (and how much the wider net added over the synthesis-only cut)
cat("\nNon-empirical DOIs by reason:\n")
print(nonempirical_review %>% count(nonempirical_reason, sort = TRUE))

#summary printed and saved
summary_tbl <- tibble(
  metric = c("grant-DOI pairs (before filter)",
             "distinct DOIs (before filter)",
             "grant-DOI pairs (after filter)",
             "distinct DOIs (after filter)",
             "distinct grants in universe",
             "grants with at least 1 DOI",
             "grants with no DOI after recovery"),
  value = c(nrow(pair_union),
            n_distinct(pair_union$doi),
            nrow(pair_union_filtered),
            n_distinct(pair_union_filtered$doi),
            length(all_known_grants),
            sum(grant_universe$has_doi),
            sum(grant_universe$has_no_doi_after_recovery))
)
write_csv(summary_tbl, file.path(out_dir, "table_03_universe_summary.csv"))
print(summary_tbl)
