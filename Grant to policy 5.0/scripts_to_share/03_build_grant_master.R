# builds the master grant-level table that almost everything downstream joins
# against - one row per grant in the 528-anchor universe - and then backfills the
# covariates the first pass leaves half-empty. order matters between the two
# parts: PART B rewrites the same canonical master paths PART A writes, so they
# run together, PART A first
#
#   PART A: assemble the master from baseline metadata (PI,
#     institution, year, program, goal), the direct + meta route summaries from
#     stage 02, Overton search-scope flags, parsed institution type, and the
#     handful of title fixups the validation audit found. no scraping happens
#     here - it reads the cached IES pages + override files if present
#
#   PART B: the IES live-scrape only resolved 258 of 528 grants, so
#     institution / type / PI / program / goal are missing for ~270 - and stage
#     03 silently dropped those into "Other", biasing any University-vs-Other
#     comparison. the metadata already exists on disk (the legacy enrichment file
#     covers 262 of the 270), so this merges it back, adds `center` (NCER/NCSER
#     off the award number) and `topic_area` (IES program -> the paper's broad
#     buckets), reconciles institution_type, and stops defaulting unknowns to
#     "Other". no network calls

#------------------
## 0. INITIALIZE ##
#------------------

library(tidyverse)
library(here)
library(rvest)

# ============================================================================ #
# PART A - enrich_grant_master
# ============================================================================ #
#tiny helpers
clean_grant <- function(x) {
  x %>% as.character() %>% str_trim() %>% str_to_upper() %>%
    na_if("") %>% na_if("NA")
}
first_nonmissing <- function(x) {
  hit <- x[!is.na(x) & x != ""]
  if (length(hit) == 0) NA else hit[1]
}
pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[1]
}

#col_or_na returns the column if present, else a same-length NA vector
col_or_na <- function(df, col) {
  if (is.na(col) || !(col %in% names(df))) rep(NA, nrow(df)) else df[[col]]
}

#-------------------------------
## 1. JOIN THE BASE TABLES ##
#-------------------------------

#start from the 528-grant universe and pull in all the per-grant summaries
#built upstream. preserving original output filenames is intentional so
#downstream code doesn't break

grant_universe <- read_csv(
  here("outputs", "02_build_current_grant_doi_universe",
       "table_02_current_grant_universe.csv"),
  show_col_types = FALSE
) %>% mutate(grant_id = clean_grant(grant_id))

#the universe table has baseline_* columns from the PI sheet. downstream
#code (dashboard, paper-side analyses) expects bare names. rename here
#so we have a clean canonical schema after the join + audit fix
canonical_renames <- c(
  pi = "baseline_pi",
  title = "baseline_title",
  institution = "baseline_institution",
  award_year = "baseline_award_year",
  program_name = "baseline_program_name",
  goal_text = "baseline_goal_text"
)
existing <- canonical_renames[canonical_renames %in% names(grant_universe)]
if (length(existing) > 0) {
  grant_universe <- grant_universe %>%
    rename(!!!setNames(existing, names(existing)))
}

direct_summary <- read_csv(
  here("outputs", "05_rebuild_current_policy_routes",
       "table_02_current_direct_policy_summary_by_grant.csv"),
  show_col_types = FALSE
) %>% mutate(grant_id = clean_grant(grant_id))

meta_summary <- read_csv(
  here("outputs", "05_rebuild_current_policy_routes",
       "table_04_current_grant_meta_summary.csv"),
  show_col_types = FALSE
) %>% mutate(grant_id = clean_grant(grant_id))

meta_policy_summary <- read_csv(
  here("outputs", "05_rebuild_current_policy_routes",
       "table_06_current_meta_policy_summary_by_grant.csv"),
  show_col_types = FALSE
) %>% mutate(grant_id = clean_grant(grant_id))

#second-order summary is optional - older runs don't have it. read it
#if present and join in; the master picks up amplification_factor +
#downstream-doc counts so downstream analyses can compare grants on
#"how far does this grant's research propagate."
so_summary_path <- here("outputs", "05_rebuild_current_policy_routes",
                        "table_09_current_second_order_summary_by_grant.csv")
if (file.exists(so_summary_path)) {
  second_order_summary <- read_csv(so_summary_path, show_col_types = FALSE) %>%
    mutate(grant_id = clean_grant(grant_id))
} else {
  second_order_summary <- tibble(grant_id = character(),
                                 n_intermediate_docs = integer(),
                                 n_second_order_docs = integer(),
                                 amplification_factor = double())
}

#per-grant classification profile (from 05). adds n_docs_tagged_* count
#columns for the top IPTC categories and a dominant_classification field
#downstream analyses can ask "what topics does this grant land in?"
classif_wide_path <- here("outputs", "12e_classify_policy_docs",
                          "table_09_per_grant_classification_wide.csv")
classif_dom_path  <- here("outputs", "12e_classify_policy_docs",
                          "table_10_per_grant_dominant_classification.csv")
if (file.exists(classif_wide_path)) {
  classif_wide <- read_csv(classif_wide_path, show_col_types = FALSE) %>%
    mutate(grant_id = clean_grant(grant_id)) %>%
    select(-any_of("n_first_order_docs"))   #already on master
} else {
  classif_wide <- tibble(grant_id = character())
}
if (file.exists(classif_dom_path)) {
  classif_dom <- read_csv(classif_dom_path, show_col_types = FALSE) %>%
    mutate(grant_id = clean_grant(grant_id))
} else {
  classif_dom <- tibble(grant_id = character())
}

search_scope <- read_csv(
  here("outputs", "05c_build_policy_search_scope",
       "table_01_grant_policy_search_scope.csv"),
  show_col_types = FALSE
) %>% mutate(grant_id = clean_grant(grant_id))

#combined master with route counts and reach flags. note that n_meta_dois
#exists in both meta_summary and search_scope - we rename one before
#joining so we don't get .x/.y suffixed columns downstream
master <- grant_universe %>%
  left_join(direct_summary, by = "grant_id") %>%
  left_join(meta_summary %>% rename(n_meta_analyses = n_meta_dois),
            by = "grant_id") %>%
  left_join(meta_policy_summary, by = "grant_id") %>%
  left_join(search_scope %>% select(-any_of(c("n_meta_dois", "n_current_dois"))),
            by = "grant_id") %>%
  left_join(second_order_summary, by = "grant_id") %>%
  left_join(classif_wide, by = "grant_id") %>%
  left_join(classif_dom,  by = "grant_id") %>%
  mutate(
    n_direct_docs = replace_na(n_direct_docs, 0L),
    n_meta_docs = replace_na(n_meta_policy_docs, 0L),
    n_meta_analyses = replace_na(n_meta_analyses, 0L),
    n_policy_docs = n_direct_docs + n_meta_docs,
    n_intermediate_docs = replace_na(n_intermediate_docs, 0L),
    n_second_order_docs = replace_na(n_second_order_docs, 0L),
    amplification_factor = replace_na(amplification_factor, 0),
    has_only_direct_reach = n_direct_docs > 0 & n_meta_docs == 0,
    has_only_meta_mediated_reach = n_direct_docs == 0 & n_meta_docs > 0,
    has_both_routes_reach = n_direct_docs > 0 & n_meta_docs > 0,
    has_any_current_policy_reach = n_direct_docs > 0 | n_meta_docs > 0,
    has_second_order_reach = n_second_order_docs > 0
  ) %>%
  # zero-fill the per-grant classification counts so unreached grants
  # show 0 rather than NA in the master
  mutate(across(starts_with("n_docs_tagged_"), ~replace_na(., 0L)))

#-----------------------------
## 2. IES LIVE-SCRAPE DATA ##
#-----------------------------

#the IES award pages have authoritative metadata: PI name, awardee
#institution, project type, year, full title, and the textual purpose
#we use for the keyword classifiers. these get scraped + cached in
#outputs/_cache/06b/ by a separate one-time script (not run here)
#we read whatever's in the parsed table

ies_path <- here("outputs", "06b_enrich_master_from_ies_live_scrape",
                 "table_05_ies_award_page_parsed.csv")

if (file.exists(ies_path)) {
  ies <- read_csv(ies_path, show_col_types = FALSE) %>%
    mutate(grant_id = clean_grant(grant_id))

  master <- master %>%
    left_join(
      ies %>% select(grant_id, ies_title, ies_pi, ies_awardee,
                     ies_project_type, ies_year, ies_program_name,
                     ies_program_topic, ies_award_page_url,
                     ies_institution_type_from_name),
      by = "grant_id"
    )
}

#the current IES award-page parser leaves ies_project_type NA for ~270 of
#the 528 grants (the post-2012 page layout broke it). before the rewrite
#we had a complete parse - the snapshot at outputs_snapshot_pre_rewrite/
#grant_classifications.csv carries the old ies_project_type column for
#all 528 grants. coalesce it in as a fallback so the keyword classifier
#in stage 04 has data to work with on all 528 and the grant-type cross-
#tab in the workbook isn't dominated by an "unclassified" row
snapshot_path <- here("outputs_snapshot_pre_rewrite", "grant_classifications.csv")
if (file.exists(snapshot_path)) {
  old_proj_type <- read_csv(snapshot_path, show_col_types = FALSE) %>%
    transmute(grant_id = clean_grant(grant_id),
              snapshot_project_type = ies_project_type)
  master <- master %>%
    left_join(old_proj_type, by = "grant_id") %>%
    mutate(ies_project_type = coalesce(ies_project_type, snapshot_project_type)) %>%
    select(-snapshot_project_type)
}

#------------------------------
## 3. AUDIT FIX (MAY 2026) ##
#------------------------------

#validation audit discovered 10 grants where the IES page parser had
#mapped grant_id to the wrong URL - PI/year/title were all wrong as a
#result. the fix re-parsed the full cached HTML store, found each grant's
#correct page by ies_award_number, and built a correction file. apply it
#here if present

corrections_path <- here("outputs", "06b2_recover_correct_grant_page_mappings",
                         "table_03_corrections_summary.csv")
if (file.exists(corrections_path)) {
  fixes <- read_csv(corrections_path, show_col_types = FALSE) %>%
    transmute(grant_id = clean_grant(grant_id),
              fix_pi = correct_pi,
              fix_year = as.integer(correct_year),
              fix_title = correct_title,
              fix_awardee = correct_awardee) %>%
    filter(!is.na(grant_id), !is.na(fix_pi), !is.na(fix_year))

  master <- master %>%
    left_join(fixes, by = "grant_id") %>%
    mutate(
      pi = if_else(!is.na(fix_pi) & nzchar(fix_pi), fix_pi, coalesce(ies_pi, pi)),
      award_year = if_else(!is.na(fix_year), fix_year, coalesce(ies_year, award_year)),
      title = if_else(!is.na(fix_title) & nzchar(fix_title), fix_title,
                      coalesce(ies_title, title)),
      institution = if_else(!is.na(fix_awardee) & nzchar(fix_awardee), fix_awardee,
                            coalesce(ies_awardee, institution))
    ) %>%
    select(-starts_with("fix_"))
}

#---------------------------------------
## 3b. AWARD-YEAR FALLBACK FROM GRANT ID ##
#---------------------------------------

#after the audit fix, ~half of the rows still have award_year=NA because
#neither baseline nor ies_year filled. but the grant_id encodes the year
#(R305A100654 -> year position 6-7 = 10 -> 2010). derive it as a last
#resort so downstream cohort/timing analyses don't drop half the sample
year_from_grant_id <- function(gid) {
  yr2 <- suppressWarnings(as.integer(
    sub(".*[A-Z](\\d{2})\\d{4}$", "\\1", gid, perl = TRUE)))
  ifelse(is.na(yr2), NA_integer_, 2000L + yr2)
}
master <- master %>%
  mutate(award_year = suppressWarnings(as.integer(award_year)),
         award_year = coalesce(award_year, year_from_grant_id(grant_id)))

#------------------------------------
## 4. INSTITUTION TYPE (3 SOURCES) ##
#------------------------------------

#institution type comes from three sources, in order of preference:
# 1. IES award page text - they often state "Awardee" plainly enough to
#    classify (University of X, Inc., etc.)
# 2. OpenAlex affiliation lookup for grants we couldn't classify from IES
# 3. manual override file for the handful we got wrong both ways

#start with IES-derived type
master <- master %>%
  mutate(institution_type = ies_institution_type_from_name,
         institution_type_source = if_else(!is.na(institution_type),
                                            "06b live IES award page", NA_character_))

#fill missing from OpenAlex if available
oa_inst_path <- here("outputs", "06c_recover_institution_type_from_openalex",
                     "table_01_institution_type_from_openalex.csv")
if (file.exists(oa_inst_path)) {
  oa_inst <- read_csv(oa_inst_path, show_col_types = FALSE) %>%
    transmute(grant_id = clean_grant(grant_id), oa_type = institution_type)
  master <- master %>%
    left_join(oa_inst, by = "grant_id") %>%
    mutate(
      institution_type_source = if_else(is.na(institution_type) & !is.na(oa_type),
                                         "06c OpenAlex", institution_type_source),
      institution_type = coalesce(institution_type, oa_type)
    ) %>% select(-oa_type)
}

#apply manual overrides last - source of truth for the cases we know
overrides_path <- here("data", "05_manual_overrides", "institution_type_overrides.csv")
if (file.exists(overrides_path)) {
  overrides <- read_csv(overrides_path, show_col_types = FALSE) %>%
    transmute(grant_id = clean_grant(grant_id), override_type = institution_type)
  master <- master %>%
    left_join(overrides, by = "grant_id") %>%
    mutate(
      institution_type_source = if_else(!is.na(override_type),
                                         "06e manual override", institution_type_source),
      institution_type = coalesce(override_type, institution_type)
    ) %>% select(-override_type)
}

#--------------------------------------------
## 5. DISAGGREGATE THE 'OTHER' BUCKET ##
#--------------------------------------------

#the legacy 3-way taxonomy (University / Other / Firm) had 127 grants
#in "Other" - a mix of nonprofit research orgs (WestEd, AIR, MDRC, RAND,
#NBER), state DOEs, hospitals, and one museum. we split using a name
#lookup table for the big research orgs + keyword rules for the rest
#
#we also split Research Organization into Major (>=5 IES grants) and
#Minor (<5) to expose the dominance of WestEd / AIR / MDRC / RAND etc

#Major nonprofit research organizations (name lookup)
research_org_names <- c(
  "WestEd", "American Institutes for Research (AIR)",
  "American Institutes for Research", "MDRC", "RAND Corporation",
  "National Bureau of Economic Research (NBER)",
  "National Bureau of Economic Research", "Oregon Research Institute",
  "SRI International", "Education Development Center, Inc.",
  "Education Development Center", "Washington Research Institute",
  "Oregon Social Learning Center", "CNA Corporation",
  "Mid-Continent Research for Education and Learning (McREL)",
  "Southwest Educational Development Corporation (SEDL)",
  "Biological Sciences Curriculum Study (BSCS)",
  "Center for Applied Special Technology (CAST)",
  "Center for Civic Education", "Education Northwest",
  "Educational Testing Service (ETS)", "Haskins Laboratories",
  "Orelena Hawks Puckett Institute", "Public Policy Institute of California",
  "RTI International", "Research for Action",
  "Strategic Education Research Partnership (SERP) Institute",
  "Technical Education Research Centers, Inc. (TERC)",
  "Urban Institute", "Mathematica", "Mathematica Policy Research"
)

#classify each grant into a finer category
classify_inst <- function(name, legacy) {
  name <- coalesce(name, "")
  legacy <- coalesce(legacy, "")
  if (name %in% research_org_names) return("Research Organization")
  if (legacy == "Uni") return("University")
  if (legacy == "Firm") return("Private Firm")
  nl <- str_to_lower(name)
  if (str_detect(nl, "department of (education|public instruction|elementary)|state board of education|school district|state education agency"))
    return("State or Local Agency")
  if (str_detect(nl, "hospital|medical center|medical school|children's"))
    return("Hospital / Medical Center")
  if (str_detect(nl, "foundation\\b")) return("Foundation / Nonprofit")
  if (str_detect(nl, "museum|civic education")) return("Museum / Other Nonprofit")
  if (str_detect(nl, "\\binstitute\\b|research center|research, inc|research organization"))
    return("Research Organization")
  if (str_detect(nl, "\\bllc\\b|\\bcorporation\\b|\\binc\\.|\\bassociates\\b") &&
      !str_detect(nl, "hospital|medical|university|college"))
    return("Private Firm")
  "Other"
}

master <- master %>%
  rowwise() %>%
  mutate(institution_type_detail = classify_inst(institution, institution_type)) %>%
  ungroup()

#split Research Organization into Major (>=5 IES grants) and Minor (<5)
#the threshold is arbitrary but anchored to the natural break in the data:
#the big six (WestEd, AIR, MDRC, RAND, NBER, Oregon Research, SRI, EDC)
#all have 5+ grants; the long tail of one-off research orgs sits below
research_org_size_threshold <- 5L

research_org_grants <- master %>%
  filter(institution_type_detail == "Research Organization") %>%
  count(institution, name = "n_grants") %>%
  mutate(size_tier = if_else(n_grants >= research_org_size_threshold,
                              "Research Organization (Major)",
                              "Research Organization (Minor)"))

master <- master %>%
  left_join(research_org_grants %>% select(institution, size_tier),
            by = "institution") %>%
  mutate(institution_type_detail = if_else(
    institution_type_detail == "Research Organization" & !is.na(size_tier),
    size_tier, institution_type_detail
  )) %>%
  select(-size_tier)

#-----------------------------------
## 5b. ERIC PRODUCTIVITY COUNTS ##
#-----------------------------------

# the ERIC download script (00a) writes a per-grant productivity file
# that counts:
#   - products with a DOI (linkable into the citation graph)
#   - products without a DOI (working papers, ERIC-only documents etc.)
#   - total products of either kind
# we join it here so the master carries an honest count of grant outputs
# regardless of whether each output is linkable downstream. useful for
# the "did this grant produce anything" question separate from the
# "did anything reach policy" question

prod_path <- here("data", "_eric_download", "eric_per_grant_productivity.csv")
if (file.exists(prod_path)) {
  productivity <- read_csv(prod_path, show_col_types = FALSE) %>%
    mutate(grant_id = clean_grant(grant_id))
  master <- master %>%
    left_join(productivity, by = "grant_id") %>%
    mutate(
      n_products_total = replace_na(n_products_total, 0L),
      n_products_with_doi = replace_na(n_products_with_doi, 0L),
      n_products_without_doi = replace_na(n_products_without_doi, 0L),
      # a useful derived: did the grant produce ANY output IES could track?
      had_any_eric_output = n_products_total > 0
    )
} else {
  master <- master %>%
    mutate(n_products_total = NA_integer_,
           n_products_with_doi = NA_integer_,
           n_products_without_doi = NA_integer_,
           had_any_eric_output = NA)
}

#-------------------------
## 6. WRITE OUTPUTS ##
#-------------------------

out_dir <- here("outputs", "06_build_grant_policy_master")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(master, file.path(out_dir, "table_01_grant_policy_master.csv"))

#also write to the legacy 06e path because the dashboard reads from there
#yes this is silly. it's also what makes downstream work without changes
out_06e <- here("outputs", "06e_apply_manual_overrides")
dir.create(out_06e, showWarnings = FALSE, recursive = TRUE)
write_csv(master, file.path(out_06e, "table_02_grant_policy_master_final.csv"))

#and the institution-type detail file (used by 13c zero-reach analysis)
out_06f <- here("outputs", "06f_disaggregate_institution_types")
dir.create(out_06f, showWarnings = FALSE, recursive = TRUE)
write_csv(master %>% select(grant_id, institution, institution_type,
                             institution_type_detail),
          file.path(out_06f, "table_01_grant_master_with_inst_detail.csv"))

cat("Master built. Rows:", nrow(master), "\n")
cat("Reached policy:    ", sum(master$has_any_current_policy_reach, na.rm = TRUE), "\n")
cat("Inst type tiers:\n")
print(sort(table(master$institution_type_detail), decreasing = TRUE))

# ============================================================================ #
# PART B - recover_grant_covariates
# ============================================================================ #

#tiny helpers - same shapes used elsewhere in the pipeline
clean_grant <- function(x) {
  x %>% as.character() %>% str_trim() %>% str_to_upper() %>%
    na_if("") %>% na_if("NA")
}

#treat blanks and the universal sentinel strings as missing. note we do
#NOT fold "Multiple Goals" or "(unclassified)" in here - those are real
#(if coarse) IES/classifier values, handled at their own read sites - so
#the missingness report stays honest about what's truly absent
is_blank <- function(x) {
  is.na(x) | str_trim(as.character(x)) == "" |
    str_to_lower(as.character(x)) %in% c("na", "unknown")
}

#coalesce that respects is_blank (base coalesce keeps "" and "NA" strings)
fill_blank <- function(x, ...) {
  fillers <- list(...)
  out <- x
  for (f in fillers) out <- if_else(is_blank(out), f, out)
  out
}

#------------------------------------
## 1. LOAD MASTER AND LOCAL SOURCES ##
#------------------------------------

master <- read_csv(
  here("outputs", "06_build_grant_policy_master",
       "table_01_grant_policy_master.csv"),
  show_col_types = FALSE
) %>% mutate(grant_id = clean_grant(grant_id))

#snapshot pre-fill missingness so the recovery report can show before/after
covariate_cols <- c("institution", "institution_type", "program_name",
                    "pi", "goal_text")
miss_before <- map_int(covariate_cols, ~ sum(is_blank(master[[.x]]))) %>%
  set_names(covariate_cols)

#the legacy enrichment file is the workhorse here. one row per grant with
#the IES award-page fields parsed long before the rewrite scrape regressed
#column names are the raw IES export names (CenterName, ProgramName, ...)
enriched <- read_csv(
  here("data", "02_legacy_existing_outputs",
       "grant_compare_enriched_with_grant_info.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    grant_id = clean_grant(grant_id),
    #affiliation_ies is the IES-stated awardee; affiliation is the OpenAlex
    #normalized name. prefer the IES one, fall back to OpenAlex. both arrive
    #in all-caps so we title-case for display
    enr_institution = str_to_title(coalesce(affiliation_ies, affiliation)),
    enr_center = na_if(str_to_upper(str_trim(CenterName)), ""),
    enr_program = na_if(str_trim(ProgramName), ""),
    enr_pi = na_if(str_trim(personnel), ""),
    enr_goal = na_if(str_trim(GoalText), ""),
    enr_inst_type = na_if(str_trim(institution_type), "")
  ) %>%
  filter(!is.na(grant_id)) %>%
  distinct(grant_id, .keep_all = TRUE)

master <- master %>% left_join(enriched, by = "grant_id")

#------------------------------------
## 2. BACKFILL THE MISSING FIELDS ##
#------------------------------------

#only fill gaps - never overwrite a value the scrape already resolved
#institution name, program, PI, and goal text all come straight from the
#enriched file where the master is blank
master <- master %>%
  mutate(
    institution  = fill_blank(institution, enr_institution),
    program_name = fill_blank(program_name, ies_program_name, enr_program),
    pi           = fill_blank(pi, enr_pi),
    goal_text    = fill_blank(goal_text, enr_goal)
  )

#---------------------------------------
## 3. CENTER: NCER vs NCSER FROM AWARD ##
#---------------------------------------

#IES award numbers encode the funding center: R305* is NCER (National
#Center for Education Research), R324* is NCSER (Special Education
#Research). this is the cleanest covariate in the whole table - 100%
#coverage, no lookup needed. the enriched file's CenterName agrees with
#it where both exist; we use the award number as the source of truth and
#keep CenterName only as a cross-check fallback
master <- master %>%
  mutate(
    center = case_when(
      str_detect(grant_id, "^R305") ~ "NCER",
      str_detect(grant_id, "^R324") ~ "NCSER",
      TRUE ~ enr_center
    )
  )

#---------------------------------------------
## 4. INSTITUTION TYPE: FILL AND RE-DERIVE ##
#---------------------------------------------

#stage 03 sets institution_type from the IES name (Uni/Other/Firm) and
#leaves it NA for the un-scraped grants. now that those grants have an
#institution name, classify the blanks from the name. the rule mirrors
#stage 03's intent: clearly a university/college -> Uni, clearly a company
#-> Firm, everything else (research orgs, agencies, hospitals) -> Other
type_from_name <- function(name) {
  nl <- str_to_lower(coalesce(name, ""))
  case_when(
    nl == "" ~ NA_character_,
    str_detect(nl, "\\buniversit|\\bcollege\\b|polytechnic") ~ "Uni",
    str_detect(nl, "hospital|medical center|children's") ~ "Other",
    str_detect(nl, "\\bllc\\b|\\bl\\.l\\.c|corporation|\\binc\\b|\\binc\\.|incorporated|\\bassociates\\b|company") ~ "Firm",
    TRUE ~ "Other"
  )
}

master <- master %>%
  mutate(
    institution_type = fill_blank(institution_type, enr_inst_type,
                                  type_from_name(institution)),
    #note the provenance for the rows we just filled
    institution_type_source = if_else(
      is_blank(institution_type_source) & !is_blank(institution_type),
      "03b recovered from name/legacy", institution_type_source)
  )

#re-derive the fine-grained institution_type_detail. stage 03 had already
#disaggregated the 258 scraped grants correctly; we re-run the same logic
#over the whole table so the newly-named grants get a real tier instead of
#the "Other" default. the research-org name match is case-insensitive here
#because the recovered names are title-cased from all-caps originals
research_org_names <- str_to_lower(c(
  "WestEd", "American Institutes for Research (AIR)",
  "American Institutes for Research", "MDRC", "RAND Corporation",
  "National Bureau of Economic Research (NBER)",
  "National Bureau of Economic Research", "Oregon Research Institute",
  "SRI International", "Education Development Center, Inc.",
  "Education Development Center", "Washington Research Institute",
  "Oregon Social Learning Center", "CNA Corporation",
  "Mid-Continent Research for Education and Learning (McREL)",
  "Southwest Educational Development Corporation (SEDL)",
  "Biological Sciences Curriculum Study (BSCS)",
  "Center for Applied Special Technology (CAST)",
  "Center for Civic Education", "Education Northwest",
  "Educational Testing Service (ETS)", "Haskins Laboratories",
  "Orelena Hawks Puckett Institute", "Public Policy Institute of California",
  "RTI International", "Research for Action",
  "Strategic Education Research Partnership (SERP) Institute",
  "Technical Education Research Centers, Inc. (TERC)",
  "Urban Institute", "Mathematica", "Mathematica Policy Research"
))

classify_inst <- function(name, legacy) {
  name <- coalesce(name, ""); legacy <- coalesce(legacy, "")
  nl <- str_to_lower(name)
  #genuinely no signal - leave it unknown rather than defaulting to "Other"
  #this is the silent-default bug the recovery exists to fix: a grant with
  #no institution name shouldn't be confidently labeled anything
  if (nl == "" && legacy == "") return(NA_character_)
  if (nl %in% research_org_names) return("Research Organization")
  if (legacy == "Uni") return("University")
  if (legacy == "Firm") return("Private Firm")
  if (str_detect(nl, "department of (education|public instruction|elementary)|state board of education|school district|state education agency"))
    return("State or Local Agency")
  if (str_detect(nl, "hospital|medical center|medical school|children's"))
    return("Hospital / Medical Center")
  if (str_detect(nl, "foundation\\b")) return("Foundation / Nonprofit")
  if (str_detect(nl, "museum|civic education")) return("Museum / Other Nonprofit")
  if (str_detect(nl, "\\binstitute\\b|research center|research, inc|research organization"))
    return("Research Organization")
  if (str_detect(nl, "\\bllc\\b|\\bcorporation\\b|\\binc\\.|\\bassociates\\b") &&
      !str_detect(nl, "hospital|medical|university|college"))
    return("Private Firm")
  if (legacy == "Uni") return("University")
  "Other"
}

master <- master %>%
  rowwise() %>%
  mutate(institution_type_detail = classify_inst(institution, institution_type)) %>%
  ungroup()

#split Research Organization into Major (>=5 IES grants) / Minor (<5)
#count on a normalized institution key so the same org recorded once as
#"WestEd" (scraped) and once as "Wested" (recovered) isn't double-counted
master <- master %>% mutate(institution_norm = str_squish(str_to_upper(institution)))
research_org_size <- master %>%
  filter(institution_type_detail == "Research Organization") %>%
  count(institution_norm, name = "n_grants") %>%
  mutate(size_tier = if_else(n_grants >= 5L,
                             "Research Organization (Major)",
                             "Research Organization (Minor)"))
master <- master %>%
  left_join(research_org_size %>% select(institution_norm, size_tier),
            by = "institution_norm") %>%
  mutate(institution_type_detail = if_else(
    institution_type_detail == "Research Organization" & !is.na(size_tier),
    size_tier, institution_type_detail)) %>%
  select(-size_tier)

#reconcile the coarse Uni/Other/Firm type FROM the detail so the two can
#never disagree (e.g. "Education Development Center, Inc." reads as a firm
#by name but is really a research org -> base becomes Other, not Firm)
#the curated research-org list in classify_inst is the more trustworthy
#signal, so the detail drives the coarse label here
master <- master %>%
  mutate(institution_type = case_when(
    institution_type_detail == "University"   ~ "Uni",
    institution_type_detail == "Private Firm" ~ "Firm",
    is.na(institution_type_detail)            ~ NA_character_,
    TRUE                                      ~ "Other"
  ))

#-------------------------------------------
## 5. TOPIC AREA FROM THE FUNDING PROGRAM ##
#-------------------------------------------

#map IES's own program-topic classification into the broad topic buckets
#the paper's "differences by topic" table uses. the best signal is the IES
#award page's "Program topic(s)" field (ies_program_topic) - that's IES's
#authoritative topic label for the grant ("Literacy", "STEM Education",
#"Postsecondary and Adult Education"). we fall back to the funding program
#name only where the topic field is blank. the broad omnibus competitions
#("Education Research Grants") name no topic; those drop to the DOI-content
#fallback in 5b rather than getting a guessed label here
#
#matching is on a lowercased substring so wording variants ("STEM
#Education" vs "Science, Technology, ...") collapse. row order matters -
#earlier rows win, so the narrower buckets (literacy, ECE, postsecondary)
#are checked before the broad Soc/Beh and Policy patterns that would
#otherwise swallow them (e.g. "Transition to Postsecondary ... disabilities"
#is postsecondary, not special-ed)
topic_crosswalk <- tribble(
  ~pattern,                                                                          ~topic_area,
  "read|writing|literacy|\\blanguage\\b|english l",                                  "Reading/Writing/Literacy",
  "early learning|early intervention|preschool|early childhood",                     "ECE",
  "postsecondary|adult education|career and technical|community college|transition to postsecondary", "Postsecondary",
  "science, technology|\\bstem\\b|mathematic",                                       "STEM",
  "social, emotional|social and behavioral|behavioral comp|character|autism|multi-tiered|families of children|disabilit", "Soc/Beh",
  "education system|state and local|\\bpolicy|policies|accountabilit|standards|low.achieving|reform|leadership|finance|partnerships|policymaking", "Policy",
  "education technology",                                                            "Other",
  "teaching, teachers|teacher quality|education workforce|educators and school|civics|social studies|rural|special topic|effective instruction", "Other"
)

#assign the first crosswalk row whose pattern hits the topic string
assign_topic <- function(topic) {
  if (is_blank(topic)) return(NA_character_)
  tl <- str_to_lower(topic)
  for (i in seq_len(nrow(topic_crosswalk))) {
    if (str_detect(tl, topic_crosswalk$pattern[i])) return(topic_crosswalk$topic_area[i])
  }
  NA_character_
}

#prefer IES's program-topic field; fall back to the funding program name
master <- master %>%
  mutate(
    topic_signal = if_else(is_blank(ies_program_topic), program_name, ies_program_topic),
    topic_from_ies_topic = !is_blank(ies_program_topic),
    topic_area = map_chr(topic_signal, assign_topic),
    topic_source = case_when(
      !is.na(topic_area) & topic_from_ies_topic ~ "IES program topic",
      !is.na(topic_area)                        ~ "IES funding program",
      is_blank(topic_signal)                    ~ "no program/topic on record",
      TRUE                                      ~ "program is funding-mechanism only (topic unresolved)"
    ),
    topic_area = replace_na(topic_area, "Unclassified")
  ) %>%
  select(-topic_signal, -topic_from_ies_topic)

#----------------------------------------------------
## 5b. TOPIC FALLBACK FROM DOI CONTENT (OpenAlex) ##
#----------------------------------------------------

#about half the grants sit under omnibus mechanisms ("Education Research
#Grants") that name no topic, so the program crosswalk leaves them
#Unclassified. their papers do carry a topic though: OpenAlex assigns each
#DOI a granular primary_topic ("Reading and Literacy Development", "Autism
#Spectrum Disorder Research", ...). we map those to the same buckets and
#give each still-Unclassified grant the modal substantive topic across its
#own DOIs. this is content-derived rather than program-derived, so it gets
#its own topic_source value and can be audited or overridden separately

#map a granular OpenAlex topic string to a paper bucket. order matters -
#reading/ECE/postsecondary are checked before the broader STEM/Soc-Beh
#patterns so e.g. "Language Development and Disorders" lands in literacy,
#not swept up by a bare "science"/"behavior" match
bucket_from_topic <- function(t) {
  tl <- str_to_lower(coalesce(t, ""))
  case_when(
    tl == "" ~ NA_character_,
    str_detect(tl, "read|literacy|writing|handwriting|\\blanguage\\b|vocabulary|phon|spelling|bilingual") ~ "Reading/Writing/Literacy",
    str_detect(tl, "early childhood|preschool|early learning|kindergarten|early intervention") ~ "ECE",
    str_detect(tl, "higher education|postsecondary|\\bcollege\\b|career|workforce|adult education|vocational") ~ "Postsecondary",
    str_detect(tl, "mathemat|\\bstem\\b|science education|computer science|engineering|computational|intelligent tutoring|robotic|physics|chemistry|geoscience") ~ "STEM",
    str_detect(tl, "behavior|psychosocial|emotional|bullying|autism|\\badhd\\b|attention deficit|disab|aggression|psychological|character|mental health|trauma|social-emotional") ~ "Soc/Beh",
    str_detect(tl, "policy|school choice|accountabilit|leadership|inequalit|\\breform\\b|governance|achievement|assessment|discipline|finance|teacher education") ~ "Policy",
    TRUE ~ "Other"
  )
}

pairs_path <- here("outputs", "02_build_current_grant_doi_universe",
                   "table_01_current_grant_doi_pair_union.csv")
oa_path <- here("outputs", "openalex_enrichment", "doi_metadata.csv")
if (file.exists(pairs_path) && file.exists(oa_path)) {
  norm_doi <- function(x) str_trim(str_to_lower(
    str_remove(str_remove(as.character(x), "^https?://doi\\.org/"), "^doi:")))

  pairs <- read_csv(pairs_path, show_col_types = FALSE) %>%
    transmute(grant_id = clean_grant(grant_id), doi = norm_doi(doi))
  oa_topic <- read_csv(oa_path, show_col_types = FALSE) %>%
    transmute(doi = norm_doi(doi), primary_topic) %>%
    distinct(doi, .keep_all = TRUE)

  #each grant-DOI tagged with its content bucket (NA where OpenAlex has no
  #topic for the DOI)
  grant_buckets <- pairs %>%
    left_join(oa_topic, by = "doi") %>%
    mutate(bucket = bucket_from_topic(primary_topic)) %>%
    filter(!is.na(bucket))

  #modal SUBSTANTIVE bucket per grant (Other excluded from the vote so a
  #grant with mostly methods papers but a clear substantive thread still
  #gets that thread). grants whose papers are all "Other"/untopiced fall
  #back to "Other" rather than staying Unclassified
  substantive_modal <- grant_buckets %>%
    filter(bucket != "Other") %>%
    count(grant_id, bucket) %>%
    group_by(grant_id) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(grant_id, fb_topic = bucket)

  topic_fallback <- grant_buckets %>%
    distinct(grant_id) %>%
    left_join(substantive_modal, by = "grant_id") %>%
    mutate(fb_topic = replace_na(fb_topic, "Other"))

  master <- master %>%
    left_join(topic_fallback, by = "grant_id") %>%
    mutate(
      topic_source = if_else(topic_area == "Unclassified" & !is.na(fb_topic),
                             "DOI primary topic (fallback)", topic_source),
      topic_area = if_else(topic_area == "Unclassified" & !is.na(fb_topic),
                           fb_topic, topic_area)
    ) %>%
    select(-fb_topic)
}

#---------------------------------------------------
## 6. OPTIONAL: DESIGN + SUBJECT FROM CLASSIFIER ##
#---------------------------------------------------

#stage 04's keyword classifier writes grant_classifications.csv (one row
#per grant: research-design class + a coarse subject_area). it usually
#runs after this stage, so we read it only if a prior run left it on disk
#and join the two fields in for convenience. topic_area above is the
#analysis covariate; subject_area here is the classifier's separate take
classif_path <- here("outputs", "grant_classifications.csv")
if (file.exists(classif_path)) {
  classif <- read_csv(classif_path, show_col_types = FALSE) %>%
    transmute(grant_id = clean_grant(grant_id),
              grant_design_class = classification)
  master <- master %>% left_join(classif, by = "grant_id")
}

#drop the scratch columns we only needed during the join
master <- master %>% select(-starts_with("enr_"), -institution_norm)

#-----------------------------------------
## 6b. MANUAL HAND-FILLS (IES award page) ##
#-----------------------------------------

#last-resort hand-fills for the handful of grants no automated source can
#reach. these are read straight off the live IES award page and take final
#precedence. add a row per grant; only the columns you list get overwritten
#
#R324A220161 is a transfer case: it is the PREVIOUS award number for the
#CIRCLES transition study (now R324A230240 at the University of Kansas)
#under R324A220161 the awardee was UNC Charlotte, so that's the institution
#we attribute to this grant_id. topic "Transition to Postsecondary..." maps
#to Postsecondary per the crosswalk above
manual_fills <- tribble(
  ~grant_id,     ~institution,                            ~institution_type, ~institution_type_detail, ~pi,                ~title,                                                                                                       ~program_name,                      ~topic_area,      ~topic_source,
  "R324A220161", "University of North Carolina, Charlotte", "Uni",            "University",             "Valerie Mazzotti", "Effects of CIRCLES on the Provision of Transition Services and Resulting Transition Outcomes for Students with Disabilities", "Special Education Research Grants", "Postsecondary", "manual fill (IES award page)"
) %>% mutate(grant_id = clean_grant(grant_id))

master <- rows_update(master, manual_fills, by = "grant_id", unmatched = "ignore")

#--------------------------------------------------------
## 6c. MANUAL SUBJECT REASSIGNMENT (DOI-content review) ##
#--------------------------------------------------------

#why this step exists: 5b classifies a grant by the modal topic of its papers,
#but OpenAlex tags a paper by the psychological MECHANISM it studies (memory,
#attention, cognition, perception) - the "how" - not the school subject the
#intervention targets - the "what", which is what our seven buckets are about
#so a grant really about teaching MATHEMATICS gets its papers tagged "Memory
#Processes" / "Psychology" and 5b reads it as Other or Soc/Beh instead of STEM
#this is systematic for the IES "Cognition and Student Learning" program (basic
#learning-science applied across content areas). for the handful 5b demonstrably
#mislands, we check the grant's own title / stated purpose and reassign it here;
#we set topic_area (grant_subject_area below inherits it). reviewed 2026-06-30:
#  R305A160263 Interleaved Mathematics Practice                   -> STEM
#  R305A170640 Enhancing Middle School Mathematics Achievement    -> STEM
#  R305A120288 Perceptual Learning Tech in Mathematics Education  -> STEM    (was Policy)
#  R305A130206 My Science Tutor: science learning                 -> STEM    (was Other)
#  R305F050284 Conjoint Behavioral Consultation for behavior      -> Soc/Beh
#  R324C120001 Special Education R&D Center (deaf/hard-of-hearing) -> Soc/Beh
#  R305A100058 Tools of the Mind: self-regulation                 -> Soc/Beh  (was ECE)
#  R305A110398 Training Attention in At-risk Preschoolers         -> Soc/Beh  (was ECE)
#(R305H060080 Test-Enhanced Learning is pure retrieval-practice / memory with no
#content area or behavioral component - left in Other where 5b placed it, per review.)
subject_overrides <- tribble(
  ~grant_id,     ~topic_area,
  "R305A160263", "STEM",
  "R305A170640", "STEM",
  "R305F050284", "Soc/Beh",
  "R324C120001", "Soc/Beh",
  "R305A120288", "STEM",
  "R305A130206", "STEM",
  "R305A100058", "Soc/Beh",
  "R305A110398", "Soc/Beh"
) %>% mutate(grant_id = clean_grant(grant_id))

master <- master %>%
  left_join(subject_overrides, by = "grant_id", suffix = c("", "_ovr")) %>%
  mutate(
    topic_source = if_else(!is.na(topic_area_ovr), "manual (DOI-content review)", topic_source),
    topic_area   = if_else(!is.na(topic_area_ovr), topic_area_ovr, topic_area)
  ) %>%
  select(-topic_area_ovr)

#grant_subject_area: the public-facing subject covariate for the "differences by
#subject" analysis. the same 7-bucket consolidation as the now-final topic_area
#(Reading/Writing/Literacy, STEM, Soc/Beh, Policy, ECE, Postsecondary, Other),
#but the 5 grants with no resolvable topic (all no-DOI) are folded into "Other"
#so every grant carries a subject - no "Unclassified" bucket. computed here,
#after the DOI fallback and manual hand-fills, so it tracks the final topic_area
#(the old, mostly-NA grant_subject_area from stage 04's classifier was dropped.)
master <- master %>%
  mutate(grant_subject_area = if_else(topic_area == "Unclassified", "Other", topic_area))

#----------------------------
## 7. WRITE CANONICAL PATHS ##
#----------------------------

#same three paths stage 03 writes, so downstream reads the enriched master
out_dir <- here("outputs", "06_build_grant_policy_master")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(master, file.path(out_dir, "table_01_grant_policy_master.csv"))

out_06e <- here("outputs", "06e_apply_manual_overrides")
dir.create(out_06e, showWarnings = FALSE, recursive = TRUE)
write_csv(master, file.path(out_06e, "table_02_grant_policy_master_final.csv"))

out_06f <- here("outputs", "06f_disaggregate_institution_types")
dir.create(out_06f, showWarnings = FALSE, recursive = TRUE)
write_csv(master %>% select(grant_id, institution, institution_type,
                            institution_type_detail),
          file.path(out_06f, "table_01_grant_master_with_inst_detail.csv"))

#---------------------------------
## 8. DATA-QUALITY REPORT ##
#---------------------------------

#before/after missingness for the covariates we touched, plus the two new
#columns. this is the audit trail for "how clean is the dataset now."
miss_after <- map_int(covariate_cols, ~ sum(is_blank(master[[.x]]))) %>%
  set_names(covariate_cols)

recovery_report <- tibble(
  variable = c(covariate_cols, "center", "topic_area"),
  n_grants = nrow(master),
  missing_before = c(miss_before[covariate_cols],
                     sum(is_blank(master$center)),       #center didn't exist before
                     NA_integer_),
  missing_after = c(miss_after[covariate_cols],
                    sum(is_blank(master$center)),
                    sum(master$topic_area == "Unclassified"))
) %>%
  mutate(pct_missing_after = round(100 * missing_after / n_grants, 1))

dq_dir <- here("outputs", "data_quality")
dir.create(dq_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(recovery_report, file.path(dq_dir, "grant_covariate_recovery_report.csv"))

#per-grant residual: list the grants that still have a blank in any core
#covariate, with exactly which fields, so the remaining gaps can be hand-
#filled from the IES site if needed
residual <- master %>%
  transmute(
    grant_id, center, institution, institution_type, program_name, topic_area,
    still_missing = pmap_chr(
      list(institution, institution_type, program_name, pi, goal_text),
      function(inst, itype, prog, pi, goal) {
        flags <- c(
          if (is_blank(inst))  "institution"      else NULL,
          if (is_blank(itype)) "institution_type" else NULL,
          if (is_blank(prog))  "program_name"     else NULL,
          if (is_blank(pi))    "pi"               else NULL,
          if (is_blank(goal))  "goal_text"        else NULL
        )
        if (length(flags) == 0) NA_character_ else paste(flags, collapse = ", ")
      })
  ) %>%
  filter(!is.na(still_missing))
write_csv(residual, file.path(dq_dir, "grant_covariate_residual_missing.csv"))

#---------------------
## 9. CONSOLE REPORT ##
#---------------------

cat("Covariate recovery complete.\n\n")
cat("Missingness (of", nrow(master), "grants):\n")
print(recovery_report)
cat("\nCenter (NCER vs NCSER):\n"); print(table(master$center, useNA = "ifany"))
cat("\nInstitution type:\n"); print(table(master$institution_type, useNA = "ifany"))
cat("\nTopic area:\n"); print(sort(table(master$topic_area), decreasing = TRUE))
cat("\nGrants with any residual missing covariate:", nrow(residual), "\n")
