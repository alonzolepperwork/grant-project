# classifies the grants, enriches their DOIs with OpenAlex bibliometrics, and
# screens out off-topic (mis-attributed) DOIs. four steps that all annotate or
# filter the universe, run in this order:
#
#   PART A: classify every grant on research design, intervention
#     type, and subject area by keyword-matching the IES award-page text. local,
#     no API
#   PART B: fetch bibliographic + impact metadata from OpenAlex for
#     every universe DOI (FWCI, fields, funders, abstracts). THIS IS THE ONE PART
#     THAT TOUCHES THE NETWORK - polite-pool, no key, fully cached
#   PART C: flag DOIs whose OpenAlex primary_field is outside the
#     education-adjacent clusters - the likeliest mis-attributions
#   PART D: apply a permissive education-keyword whitelist to those
#     candidates and write the exclusion list stage 01 reads on its next run
#
# PARTS C-D depend on PART B's output; PART B needs the universe from stage 01
# the exclusion list (PART D) feeds back into stage 01, so a full refresh is a
# two-pass loop (run 01 -> here -> 01 again) - the same bootstrap as the original
# pipeline

#------------------
## 0. INITIALIZE ##
#------------------

library(tidyverse)
library(here)
library(curl)
library(jsonlite)

# ============================================================================ #
# PART A - classify_grants
# ============================================================================ #
#load the parsed IES award pages - this is the source of title and purpose
#text for every grant. produced by the IES live-scrape stage
d <- read_csv(
  here("outputs", "06b_enrich_master_from_ies_live_scrape",
       "table_05_ies_award_page_parsed.csv"),
  show_col_types = FALSE
) %>%
  # coerce to character first - if any column is all NA, read_csv types it
  # as logical, which then chokes replace_na("") with a logical-vs-character
  # mismatch. ies_purpose in particular comes through empty when the new
  # IES page layout's "purpose" block isn't matched by the 00c parser
  mutate(across(c(ies_award_number, ies_title, ies_purpose, ies_project_type,
                  ies_program_name, ies_program_topic, ies_awardee, ies_pi),
                as.character)) %>%
  transmute(
    grant_id = ies_award_number %>% replace_na("") %>% str_to_upper(),
    title = ies_title %>% replace_na(""),
    purpose = ies_purpose %>% replace_na(""),
    project_type = ies_project_type %>% replace_na(""),
    program_name = ies_program_name %>% replace_na(""),
    program_topic = ies_program_topic %>% replace_na(""),
    awardee = ies_awardee %>% replace_na(""),
    pi = ies_pi %>% replace_na("")
  ) %>%
  filter(grant_id != "")

#the IES-page parse misses ~270 of the 528 grants (the 2012+ page layout
#change broke the parser for that cohort). fold in the baseline title +
#goal text from the universe table so the keyword classifier has something
#to work with for those grants. without this we'd lose them entirely from
#design / intervention / subject classification
universe <- read_csv(
  here("outputs", "02_build_current_grant_doi_universe",
       "table_02_current_grant_universe.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    grant_id = str_to_upper(grant_id),
    baseline_title = replace_na(as.character(title), ""),
    baseline_program_name = replace_na(as.character(program_name), ""),
    baseline_goal_text = replace_na(as.character(goal_text), "")
  )

#pre-rewrite snapshot - the older IES parse was complete (project_type
#filled for all 528 grants). coalesce its project_type values in so the
#case_when at the bottom of classify_one() can find an "efficacy" /
#"replicat" / "scale" / "effectiveness" hit for the 270 grants the
#current parse missed. without this fallback the design distribution
#collapses to a giant "Other" bucket; with it we recover the 465/44/15/
#2/2 breakdown the SREE submission reported
snapshot_path <- here("outputs_snapshot_pre_rewrite", "grant_classifications.csv")
snapshot_pt <- if (file.exists(snapshot_path)) {
  read_csv(snapshot_path, show_col_types = FALSE) %>%
    transmute(grant_id = str_to_upper(grant_id),
              snapshot_project_type = ies_project_type)
} else {
  tibble(grant_id = character(), snapshot_project_type = character())
}

#every grant in the universe gets a row, even if the IES parse failed
#left_join then prefer ies_* text where present, fall back to baseline_*
#and the snapshot project_type for the 270-grant cohort
d <- universe %>%
  left_join(d, by = "grant_id") %>%
  left_join(snapshot_pt, by = "grant_id") %>%
  mutate(
    title        = if_else(nzchar(replace_na(title, "")), title, baseline_title),
    purpose      = replace_na(purpose, ""),
    # snapshot wins over current parse - the current parser strips compound
    # types ("Development and Innovation, Efficacy" -> "Development and
    # Innovation"), which would mis-bucket grants the SREE submission
    # counted as Efficacy
    project_type = if_else(nzchar(replace_na(snapshot_project_type, "")),
                            snapshot_project_type,
                            replace_na(project_type, "")),
    program_name = if_else(nzchar(replace_na(program_name, "")),
                            program_name, baseline_program_name),
    program_topic = replace_na(program_topic, ""),
    awardee = replace_na(awardee, ""),
    pi = replace_na(pi, "")
  )

#text we'll match against - title + IES purpose + baseline goal text (when
#available). lowercased once here so the helpers don't have to do it 528
#times each
d$text <- paste(d$title, d$purpose, d$baseline_goal_text) %>% str_to_lower()

#--------------------
## 1. KEYWORD BANKS ##
#--------------------

#DESIGN TYPE - what kind of research design is being tested?
design_kw <- list(
  Efficacy = c(
    "randomized controlled trial", "\\brct\\b", "causal impact",
    "efficacy study", "efficacy trial", "treatment and control",
    "randomly assigned", "random assignment", "random[ly]+ allocat",
    "experimental design", "control group", "treatment group"
  ),
  Effectiveness = c(
    "effectiveness study", "effectiveness trial", "\\beffectiveness\\b",
    "real.world condition", "as.implemented", "routine practice",
    "implemented as intended", "implementation fidelity",
    "natural setting", "authentic setting"
  ),
  Replication = c(
    "\\breplicat", "generalizab", "new population", "different setting",
    "different context", "identified for replication", "confirm findings",
    "confirm the findings", "replicate the", "replication study",
    "test whether.*findings", "broader.*population"
  ),
  `Scale-up` = c(
    "\\bscale.up\\b", "\\bscaling\\b", "at scale\\b", "broad implementation",
    "larger sample", "large.scale", "nationwide", "district.wide",
    "statewide implementation", "widespread", "disseminat"
  )
)

other_design_signals <- c(
  "research center", "\\bcenter\\b", "descriptive study", "descriptive research",
  "survey study", "literature review", "systematic review", "meta.analysis",
  "measurement study", "instrument development", "policy analysis",
  "longitudinal study", "correlational study", "exploratory study"
)

#INTERVENTION TYPE - what kind of intervention is the grant testing?
#the production banks are extensive (~500 patterns total) and live in
#legacy script 21_classify_intervention_type.R. for the consolidated
#rewrite we use the same bank definitions; only an abbreviated set is
#shown here for readability. when running this, set FULL_BANKS = TRUE
#and source the bank definitions from the legacy file
intervention_kw <- list(
  `Curriculum/Instructional Program` = c(
    "\\bcurriculum\\b", "\\bcurricula\\b",
    "instructional program", "instructional intervention",
    "instructional approach", "instructional method", "instructional model",
    "reading program", "literacy program", "math.*program", "science.*program",
    "writing program", "language.*program", "\\btextbook\\b",
    "\\bphonics\\b", "explicit.*instruct", "structured.*instruct",
    "inquiry.*approach", "writing.*instruction.*science"
  ),
  `Professional Development` = c(
    "teacher.*professional development", "professional development.*teacher",
    "teacher.*coaching", "instructional coach", "teacher.*training",
    "teacher.*preparation", "preservice teacher", "inservice teacher",
    "teacher effectiveness", "teacher.*practice",
    "teacher.*knowledge", "content knowledge.*teacher",
    "mentor.*teacher", "teacher.*mentor", "teacher.*feedback",
    "teacher.*certification", "teacher.*retention"
  ),
  `Technology/Software` = c(
    "technology.*learning", "technology.*instruction", "digital.*learning",
    "online.*learning", "online.*instruction", "computer.*instruction",
    "educational software", "educational technolog", "adaptive.*learning",
    "blended learning", "e.learning", "\\bMOOC\\b",
    "intelligent tutor", "computer.assisted", "tablet.*learning",
    "online.*credit.*recov", "online.*course.*school", "online.*algebra"
  ),
  `Behavioral/SEL Program` = c(
    "social.*emotional.*learning", "\\bSEL\\b",
    "behavior.*intervention", "behavioral.*intervention",
    "positive behavior", "\\bPBIS\\b", "school.*climate",
    "social skills", "stereotype threat", "\\bHOPS\\b",
    "ADHD.*school", "Positive Action", "character development.*school",
    "self.regulation", "mindfulness.*school", "trauma.informed"
  ),
  `Tutoring/Mentoring` = c(
    "\\btutoring\\b", "\\btutor\\b", "tutorial program",
    "\\bmentoring\\b", "\\bmentor\\b", "mentorship",
    "peer.*tutor", "high.dosage tutoring", "one.on.one tutoring",
    "small.group tutoring", "individualized tutoring"
  ),
  `Systems/Policy Change` = c(
    "school reform", "district reform", "education policy",
    "school improvement", "school turnaround", "school accountability",
    "school choice", "charter school", "voucher",
    "school finance", "education funding", "school governance",
    "after.school.*program", "expanded learning time"
  )
)

#manual overrides for grants where keyword inference is wrong. these
#were caught by spot-checking the low-confidence outputs
manual_subject <- c(
  R305A060034 = "Teaching & Workforce",
  R305C150017 = "Systems/Policy",
  R305C200012 = "Cognition & Learning",
  R305F050069 = "Systems/Policy",
  R305S210005 = "Systems/Policy",
  R305S220003 = "Systems/Policy"
)

#--------------------
## 2. HELPERS ##
#--------------------

#count how many regex patterns match a string. used for every classifier
#dimension. perl = TRUE because some patterns use \\b for word boundary
count_hits <- function(text, patterns) {
  sum(sapply(patterns, function(p) {
    grepl(p, text, ignore.case = TRUE, perl = TRUE)
  }))
}

#-----------------------------------
## 3. DIMENSION 1 - DESIGN TYPE ##
#-----------------------------------

#first read off the IES project_type field where available. when IES
#tagged the grant as "Effectiveness" or "Scale-up" or "Replication"
#explicitly, that's a stronger signal than keyword inference. only fall
#back to keywords when the project_type is missing or ambiguous

classify_design <- function(text, project_type) {

  pt <- str_to_lower(project_type)

  ies_label <- case_when(
    str_detect(pt, "effectiveness") ~ "Effectiveness",
    str_detect(pt, "scale") ~ "Scale-up",
    str_detect(pt, "replicat") ~ "Replication",
    str_detect(pt, "efficacy") ~ "Efficacy",
    str_detect(pt, "development|exploration|measurement|follow.up|other") ~ "Other",
    TRUE ~ NA_character_
  )

  hits <- sapply(design_kw, function(kw) count_hits(text, kw))
  other_hits <- count_hits(text, other_design_signals)
  best_type <- names(which.max(hits))
  best_n <- max(hits)

  #IES label always wins when present - 90 confidence
  if (!is.na(ies_label)) {
    flag <- if (best_n > 0 && best_type != ies_label) {
      sprintf("keyword analysis suggested %s but IES project_type = %s",
              best_type, ies_label)
    } else ""
    return(list(label = ies_label, confidence = 90L,
                evidence = sprintf("IES project_type = %s", project_type),
                flag = flag))
  }

  #no IES label - fall back to keyword evidence
  if (best_n == 0 || other_hits >= 2) {
    return(list(label = "Other",
                confidence = if (other_hits >= 2) 80L else 60L,
                evidence = sprintf("no design signals; other-signals=%d",
                                    other_hits),
                flag = ""))
  }

  conf <- as.integer(min(90L, 60L + best_n * 10L))
  list(label = best_type, confidence = conf,
       evidence = sprintf("matched %d %s keyword(s)", best_n, best_type),
       flag = "")
}

design <- d %>%
  rowwise() %>%
  mutate(r = list(classify_design(text, project_type))) %>%
  ungroup() %>%
  mutate(
    classification = map_chr(r, "label"),
    confidence = map_int(r, "confidence"),
    reasoning = map_chr(r, "evidence"),
    flags = map_chr(r, "flag")
  ) %>%
  select(grant_id, title, project_type, pi, awardee,
         classification, confidence, reasoning, flags)
#cat("Design distribution:\n"); print(table(design$classification))

#--------------------------------------
## 4. DIMENSION 2 - INTERVENTION ##
#--------------------------------------

#same pattern as design - count hits per bank, pick max. plus three
#disambiguation rules that came out of spot-checking:
# (a) PD wins when text mentions "teacher" 3+ times
# (b) Technology wins when text mentions a named software platform
#     (ASSISTments, Cognitive Tutor, Khan Academy, etc.)
# (c) Behavioral/SEL wins when IES program_topic == "Social and
#     Behavioral Context"

named_tech <- c("ASSISTments", "Cognitive Tutor", "Khan Academy",
                "IXL", "DreamBox", "i-Ready", "Reading Plus",
                "Achieve3000", "MyLab", "ALEKS")

classify_intervention <- function(text, program_topic) {
  hits <- sapply(intervention_kw, function(kw) count_hits(text, kw))

  #disambiguation - PD wins for teacher-heavy texts
  if (str_count(text, "\\bteacher") >= 3 && hits["Professional Development"] >= 2) {
    return(list(label = "Professional Development",
                confidence = 85L,
                evidence = "teacher mentioned 3+ times + PD keywords"))
  }

  #disambiguation - Tech wins when a named platform appears
  if (any(str_detect(text, regex(paste(named_tech, collapse = "|"),
                                  ignore_case = TRUE)))) {
    return(list(label = "Technology/Software",
                confidence = 90L,
                evidence = "named tech platform in text"))
  }

  #disambiguation - SEL wins for SEL program topic
  if (str_detect(str_to_lower(program_topic), "social.*behavioral|behavior")) {
    return(list(label = "Behavioral/SEL Program",
                confidence = 85L,
                evidence = "IES topic = Social and Behavioral"))
  }

  best_type <- names(which.max(hits))
  best_n <- max(hits)
  if (best_n == 0) {
    return(list(label = "Other", confidence = 50L,
                evidence = "no intervention keywords matched"))
  }
  conf <- as.integer(min(95L, 55L + best_n * 8L))
  list(label = best_type, confidence = conf,
       evidence = sprintf("matched %d %s keyword(s)", best_n, best_type))
}

intervention <- d %>%
  rowwise() %>%
  mutate(r = list(classify_intervention(text, program_topic))) %>%
  ungroup() %>%
  mutate(
    intervention_type = map_chr(r, "label"),
    int_confidence = map_int(r, "confidence"),
    int_reasoning = map_chr(r, "evidence")
  ) %>%
  select(grant_id, title, intervention_type, int_confidence, int_reasoning)
#cat("Intervention distribution:\n"); print(table(intervention$intervention_type))

#-------------------------------
## 5. DIMENSION 3 - SUBJECT ##
#-------------------------------

#subject_area starts from IES program_topic when present. there are 38
#raw IES topics; we map them to 14 standard categories
ies_topic_to_subject <- c(
  "Cognition and Student Learning" = "Cognition & Learning",
  "Cognition" = "Cognition & Learning",
  "Early Learning Programs and Policies" = "Early Childhood",
  "Education Leadership" = "Systems/Policy",
  "Education Technology" = "Ed Technology",
  "Effective Teachers and Effective Teaching" = "Teaching & Workforce",
  "English Learners" = "English Learners",
  "Improving Education Systems" = "Systems/Policy",
  "Mathematics and Science Education" = "STEM",
  "Postsecondary and Adult Education" = "Postsecondary",
  "Reading and Writing" = "Reading/ELA",
  "Reading, Writing, and Language Development" = "Reading/ELA",
  "Research Networks Focused on Critical Problems of Education" = "Systems/Policy",
  "School Improvement" = "School Improvement",
  "Social and Behavioral Context for Academic Learning" = "Social/Behavioral",
  "Special Education" = "Special Education",
  "Transition to Postsecondary Education, Career, and/or Independent Living" =
    "Transition/Postsecondary"
)

subject <- d %>%
  mutate(
    subject_area = ies_topic_to_subject[program_topic],
    subject_inferred = is.na(subject_area)
  ) %>%
  select(grant_id, subject_area, subject_inferred)

#apply manual overrides last
subject <- subject %>%
  mutate(subject_area = if_else(grant_id %in% names(manual_subject),
                                 manual_subject[grant_id], subject_area))

#fill any remaining NAs with "(unclassified)"
subject$subject_area <- replace_na(subject$subject_area, "(unclassified)")

#-------------------
## 6. JOIN + WRITE ##
#-------------------

#one row per grant with all three dimensions
combined <- design %>%
  left_join(intervention %>% select(grant_id, intervention_type,
                                     int_confidence, int_reasoning),
            by = "grant_id") %>%
  left_join(subject, by = "grant_id")

out_dir <- here("outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

#write the historical filenames so downstream consumers don't break
#legacy scripts (notably 16_export_combined_dashboard.R) refer to this
#column as ies_project_type - rename on write so we don't have to patch
#them. internal references in this script keep using project_type
write_csv(combined %>% select(grant_id, title,
                                 ies_project_type = project_type,
                                 pi, awardee, classification, confidence,
                                 reasoning, flags, subject_area, subject_inferred),
          file.path(out_dir, "grant_classifications.csv"))

write_csv(combined %>% select(grant_id, intervention_type,
                                 int_confidence, int_reasoning),
          file.path(out_dir, "grant_intervention_types.csv"))

cat("Design distribution:\n"); print(table(combined$classification))
cat("\nIntervention distribution:\n"); print(table(combined$intervention_type))
cat("\nSubject distribution:\n"); print(table(combined$subject_area))
cat("\nMedian design confidence:    ", median(combined$confidence), "\n")
cat("Median intervention confidence:", median(combined$int_confidence), "\n")

# ============================================================================ #
# PART B - enrich_via_openalex (NETWORK)
# ============================================================================ #

# progress-aware map_dfr drop-in. prints elapsed time, rate, and ETA
# every iteration. useful for any long loop that hits an API - shows whether
# it has stalled
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

#load the universe DOIs - one fetch per distinct DOI
pairs <- read_csv(
  here("outputs", "02_build_current_grant_doi_universe",
       "table_01_current_grant_doi_pair_union.csv"),
  show_col_types = FALSE
)
doi_list <- unique(pairs$doi)
doi_list <- doi_list[!is.na(doi_list) & nchar(doi_list) > 5]
#length(doi_list) #~2900 to fetch on first run

#polite-pool mailto. always set this; OpenAlex routes mailto requests
#to a faster lane and won't rate-limit politely-identified callers
mailto <- "your_email_here@email.com"
batch_size <- 50    #max DOIs per OpenAlex /works query
concurrency <- 10   #parallel curl connections

#fields we actually use downstream. keeping this list explicit so we
#don't accidentally bloat responses
fields <- paste(c(
  "id", "doi", "title", "publication_year", "publication_date",
  "language", "type", "abstract_inverted_index",
  "primary_location", "open_access", "authorships", "biblio",
  "cited_by_count", "fwci", "is_retracted",
  "primary_topic", "topics", "funders",
  "best_oa_location"
), collapse = ",")

#-----------------------------
## 1. BUILD AND FIRE QUERIES ##
#-----------------------------

#OpenAlex /works supports filter=doi:A|B|C (up to 50 per call). batch
#and fire concurrently via curl::multi. responses cached under
#outputs/_cache/openalex/<batch_hash>.json so re-runs are cheap

cache_dir <- here("outputs", "_cache", "openalex")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

#per-DOI cache: each returned work is stored under its own DOI, so a re-run only
#queries DOIs not already cached. a partial OpenAlex response can never overwrite
#what already came back (misses are simply re-queried), and a DOI OpenAlex
#genuinely lacks just stays uncached. this is the guard against the silent
#corruption where one bad API day wiped doi_metadata.csv
doi_cache_path <- function(doi)
  file.path(cache_dir, paste0(gsub("[^a-z0-9]+", "_", tolower(doi)), ".json"))
doi_from_url <- function(u) tolower(sub("^https?://doi\\.org/", "", u %||% ""))

cached_works <- list()
for (d in doi_list) {
  cp <- doi_cache_path(d)
  if (file.exists(cp)) {
    w <- tryCatch(fromJSON(cp, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(w)) cached_works[[d]] <- w
  }
}
need <- setdiff(doi_list, names(cached_works))
cat(sprintf("  %d/%d DOIs from cache; fetching %d\n",
            length(cached_works), length(doi_list), length(need)))

#fetch only the DOIs still needed, in batches through the connection pool. each
#returned work is written to its own cache file in the done callback
fetch_batches <- if (length(need) > 0)
  split(need, ceiling(seq_along(need) / batch_size)) else list()
fetched <- vector("list", length(fetch_batches))
if (length(fetch_batches) > 0) {
  pool <- new_pool(host_con = concurrency, total_con = concurrency * 2)
  for (i in seq_along(fetch_batches)) {
    local({
      idx <- i
      url <- paste0("https://api.openalex.org/works",
                    "?filter=doi:", paste(fetch_batches[[idx]], collapse = "|"),
                    "&select=", fields, "&per-page=", batch_size,
                    "&mailto=", mailto)
      h <- new_handle()
      handle_setheaders(h, `User-Agent` = paste0("mailto:", mailto))
      curl_fetch_multi(url, pool = pool, handle = h,
        done = function(resp) {
          if (resp$status_code == 200) {
            res <- tryCatch(
              fromJSON(rawToChar(resp$content), simplifyVector = FALSE)$results,
              error = function(e) list())
            fetched[[idx]] <<- res
            for (w in res) {
              d <- doi_from_url(w$doi)
              if (nzchar(d))
                tryCatch(write_json(w, doi_cache_path(d), auto_unbox = TRUE),
                         error = function(e) NULL)
            }
          }
          if (idx %% 20 == 0) cat(sprintf("  %d/%d fetch-batches done\n",
                                          idx, length(fetch_batches)))
        },
        fail = function(msg) message(sprintf("Batch %d failed: %s", idx, msg))
      )
    })
  }
  multi_run(pool = pool)
}
all_works <- c(unname(cached_works), unlist(fetched, recursive = FALSE))
cat(sprintf("Works returned: %d / %d DOIs queried\n",
            length(all_works), length(doi_list)))

#guard: a healthy run covers ~95%+ of DOIs. far below that means a partial API
#response, not real coverage - stop before the parse/write can overwrite the good
#doi_metadata.csv. the per-DOI cache persists, so just re-run and it resumes
coverage <- if (length(doi_list) > 0) length(all_works) / length(doi_list) else 1
if (coverage < 0.80) {
  stop(sprintf(paste0("OpenAlex enrichment covered only %d of %d DOIs (%.0f%%) - ",
    "that's a partial API response, refusing to overwrite doi_metadata.csv. ",
    "re-run to fetch the misses (successes are cached)."),
    length(all_works), length(doi_list), 100 * coverage))
}

#-----------------------------
## 2. PARSE: FLAT METADATA ##
#-----------------------------

#OpenAlex returns abstracts as an "inverted index" (token -> positions)
#reconstruct the readable abstract here so downstream stages have plain
#text to work with
reconstruct_abstract <- function(aii) {
  if (is.null(aii) || length(aii) == 0) return(NA_character_)
  tryCatch({
    pos_word <- unlist(mapply(
      function(word, positions) setNames(rep(word, length(positions)),
                                          as.character(positions)),
      names(aii), aii, SIMPLIFY = FALSE
    ))
    paste(pos_word[order(as.integer(names(pos_word)))], collapse = " ")
  }, error = function(e) NA_character_)
}

clean_doi <- function(raw) {
  tolower(str_remove(replace_na(raw, ""), "^https?://doi\\.org/"))
}

#flat metadata - one row per DOI. NULL-guard every field via %||% so
#that OpenAlex returning a partial work doesn't crash map_int/map_chr
#with "Result must be length 1, not 0"
metadata <- tibble(
  doi = map_chr(all_works, ~clean_doi(.x$doi %||% NA_character_)),
  openalex_id = map_chr(all_works, ~as.character(.x$id %||% NA_character_)),
  title = map_chr(all_works, ~as.character(.x$title %||% NA_character_)),
  publication_year = map_int(all_works,
                              ~as.integer(.x$publication_year %||% NA_integer_)),
  publication_date = map_chr(all_works,
                              ~as.character(.x$publication_date %||% NA_character_)),
  language = map_chr(all_works, ~as.character(.x$language %||% NA_character_)),
  type = map_chr(all_works, ~as.character(.x$type %||% NA_character_)),
  cited_by_count = map_int(all_works,
                            ~as.integer(.x$cited_by_count %||% 0L)),
  fwci = map_dbl(all_works,
                  ~as.numeric(.x$fwci %||% NA_real_)),
  is_retracted = map_lgl(all_works,
                          ~as.logical(.x$is_retracted %||% FALSE)),
  abstract = map_chr(all_works, ~reconstruct_abstract(.x$abstract_inverted_index)),
  is_oa = map_lgl(all_works,
                   ~as.logical(.x$open_access$is_oa %||% FALSE)),
  oa_status = map_chr(all_works,
                       ~as.character(.x$open_access$oa_status %||% NA_character_)),
  # primary_topic and primary_field surface the top OpenAlex topic
  # assignment at the metadata level - useful for one-tag-per-DOI
  # rollups in the dashboard (the long-format topics file has all
  # tags but the headline tables want a single "primary" label)
  primary_topic = map_chr(all_works,
                           ~as.character(.x$primary_topic$display_name %||% NA_character_)),
  primary_field = map_chr(all_works,
                           ~as.character(.x$primary_topic$field$display_name %||% NA_character_))
)

#---------------------------------
## 3. PARSE: NESTED FIELDS ##
#---------------------------------

#OpenAlex returns lists for authorships, topics, funders. each becomes
#its own long-format CSV so we can pivot/aggregate downstream without
#re-parsing JSON

#funder records - one row per DOI per funder
funders <- map(all_works, function(w) {
  if (is.null(w$funders) || length(w$funders) == 0) return(NULL)
  map(w$funders, function(f) {
    tibble(doi = clean_doi(w$doi),
           funder_name = replace_na(f$display_name, ""),
           funder_id = replace_na(f$id, ""),
           award_id = paste(unlist(f$award_id %||% list()), collapse = ";"))
  }) %>% bind_rows()
}) %>% bind_rows()

#topics (OpenAlex's auto-classification)
topics <- map(all_works, function(w) {
  if (is.null(w$topics) || length(w$topics) == 0) return(NULL)
  map(w$topics, function(t) {
    tibble(doi = clean_doi(w$doi),
           topic = replace_na(t$display_name, ""),
           topic_score = as.numeric(replace_na(t$score, NA)),
           subfield = replace_na(t$subfield$display_name, ""),
           field = replace_na(t$field$display_name, ""))
  }) %>% bind_rows()
}) %>% bind_rows()

#authorships (used for PI lookup in validation audit)
authorships <- map(all_works, function(w) {
  if (is.null(w$authorships) || length(w$authorships) == 0) return(NULL)
  map(w$authorships, function(a) {
    inst_name <- ""
    if (!is.null(a$institutions) && length(a$institutions) > 0) {
      inst_name <- replace_na(a$institutions[[1]]$display_name, "")
    }
    tibble(doi = clean_doi(w$doi),
           author_id = replace_na(a$author$id, ""),
           author_name = replace_na(a$author$display_name, ""),
           is_corresponding = as.logical(a$is_corresponding %||% FALSE),
           institution = inst_name)
  }) %>% bind_rows()
}) %>% bind_rows()

#------------------
## 4. WRITE OUT ##
#------------------

out_dir <- here("outputs", "openalex_enrichment")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(metadata, file.path(out_dir, "doi_metadata.csv"))
write_csv(funders, file.path(out_dir, "doi_grants.csv"))
write_csv(topics, file.path(out_dir, "doi_topics.csv"))
write_csv(authorships, file.path(out_dir, "doi_authorships.csv"))

cat("OpenAlex enrichment done.\n")
cat("  Universe DOIs:        ", length(doi_list), "\n")
cat("  In OpenAlex metadata: ", nrow(metadata), "\n")
cat("  With funder records:  ", n_distinct(funders$doi), "\n")
cat("  Missing from OpenAlex:", length(doi_list) - nrow(metadata), "\n")

# ============================================================================ #
# PART C - audit_offtopic_dois
# ============================================================================ #

pairs <- read_csv(
  here("outputs", "02_build_current_grant_doi_universe",
       "table_01_current_grant_doi_pair_union.csv"),
  show_col_types = FALSE
)

oa <- read_csv(
  here("outputs", "openalex_enrichment", "doi_metadata.csv"),
  show_col_types = FALSE
) %>%
  distinct(doi, .keep_all = TRUE) %>%
  select(doi, openalex_title = title, publication_year, type,
         primary_topic, primary_field, fwci, cited_by_count)

out_dir <- here("outputs", "_paper_audit")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


#----------------------------------
## 1. CLASSIFY EACH DOI BY FIELD ##
#----------------------------------

# fields we treat as expected for an education-research universe
expected_fields <- c(
  "Social Sciences", "Psychology", "Mathematics",
  "Arts and Humanities", "Decision Sciences",
  "Business, Management and Accounting",
  "Economics, Econometrics and Finance"
)

universe_with_meta <- pairs %>%
  distinct(grant_id, doi, src_original, src_eric_r3, src_manual,
            src_canonical, src_crossref) %>%
  left_join(oa, by = "doi") %>%
  mutate(field_bucket = case_when(
    is.na(primary_field)                   ~ "(no OpenAlex metadata)",
    primary_field %in% expected_fields     ~ "Expected (education-adjacent)",
    TRUE                                    ~ "Off-topic candidate"
  ))

# headline count
field_summary <- universe_with_meta %>%
  distinct(doi, field_bucket, primary_field) %>%
  count(field_bucket, primary_field, sort = TRUE) %>%
  group_by(field_bucket) %>%
  mutate(field_bucket_total = sum(n)) %>%
  ungroup()

cat("Universe field summary:\n\n")
field_summary %>%
  group_by(field_bucket) %>%
  summarize(distinct_dois = sum(n), .groups = "drop") %>%
  mutate(pct = sprintf("%.1f%%", 100 * distinct_dois / sum(distinct_dois))) %>%
  print()

cat("\nOff-topic candidates by primary_field:\n\n")
field_summary %>%
  filter(field_bucket == "Off-topic candidate") %>%
  select(primary_field, n) %>%
  arrange(desc(n)) %>%
  print()


#---------------------------------------
## 2. EMIT THE FULL OFF-TOPIC LIST ##
#---------------------------------------

# one row per (grant, off-topic DOI), with all the metadata a reviewer
# would want for spot-checking: title, year, journal field, journal,
# citation count, and which source contributed the pair
audit <- universe_with_meta %>%
  filter(field_bucket == "Off-topic candidate") %>%
  transmute(
    grant_id, doi,
    primary_field,
    primary_topic,
    publication_year,
    openalex_type = type,
    title = openalex_title,
    fwci,
    cited_by_count,
    # provenance flags - which source(s) contributed this pair
    src_original  = replace_na(src_original,  FALSE),
    src_eric_r3   = replace_na(src_eric_r3,   FALSE),
    src_manual    = replace_na(src_manual,    FALSE),
    src_canonical = replace_na(src_canonical, FALSE),
    src_crossref  = replace_na(src_crossref,  FALSE)
  ) %>%
  arrange(primary_field, grant_id)

audit_path <- file.path(out_dir, "offtopic_doi_candidates.csv")
write_csv(audit, audit_path)

cat("\nWrote off-topic candidate list:\n  ", audit_path, "\n")
cat("Rows:                ", nrow(audit), "\n")
cat("Distinct DOIs:       ", n_distinct(audit$doi), "\n")
cat("Distinct grants:     ", n_distinct(audit$grant_id), "\n")

# a per-grant rollup: how many off-topic DOIs does each grant carry?
# grants with many off-topic DOIs are the most concentrated mis-
# attribution risk and the highest-value spot-check targets
per_grant_summary <- audit %>%
  group_by(grant_id) %>%
  summarize(n_offtopic_dois = n_distinct(doi),
            fields = paste(sort(unique(primary_field)), collapse = "; "),
            sample_titles = paste(head(unique(title), 3), collapse = " | "),
            .groups = "drop") %>%
  arrange(desc(n_offtopic_dois))

per_grant_path <- file.path(out_dir, "offtopic_by_grant.csv")
write_csv(per_grant_summary, per_grant_path)

cat("\nPer-grant rollup:\n  ", per_grant_path, "\n")
cat("Top 10 grants with most off-topic DOIs:\n\n")
per_grant_summary %>% head(10) %>%
  select(grant_id, n_offtopic_dois, fields) %>%
  print(width = Inf)

# ============================================================================ #
# PART D - build_offtopic_exclusion_list
# ============================================================================ #

audit <- read_csv(
  here("outputs", "_paper_audit", "offtopic_doi_candidates.csv"),
  show_col_types = FALSE
)

review_dir <- here("data", "_manual_review")
dir.create(review_dir, showWarnings = FALSE, recursive = TRUE)


#-------------------------
## 1. WHITELIST PATTERNS ##
#-------------------------

# regex patterns. matched (case-insensitive) against primary_topic and
# title. the test passes if ANY pattern hits. these are deliberately
# broad - the goal is to catch every plausibly-IES-relevant DOI
education_kw <- c(
  # explicit education / pedagogy / classroom
  "educat", "pedagog", "teach", "learn",
  "tutor", "curricul", "classroom", "school",
  "student", "instruction", "schooling",

  # core IES research areas
  "literacy", "reading", "writing", "math",
  "stem", "numerac", "vocabulary", "phonic",
  "comprehension", "spelling", "fluency",

  # cognitive / developmental science of education
  "cognit", "memory", "attention", "mind wander",
  "metacognit", "executive function", "self.regulat",
  "academic", "achievement", "test\\b", "assess",

  # special education / NCSER
  "autis", "asd\\b", "adhd", "disabilit", "special.ed",
  "intervention", "language disorder", "speech",
  "cerebral palsy", "dyslex", "downsynd", "down syndrome",

  # early childhood
  "early childhood", "preschool", "kindergarten",
  "infant develop", "preterm", "head start",

  # ed tech
  "tutoring system", "adaptive learn", "online learn",
  "intelligent tutor", "e.learn", "mooc", "edu.tech",
  "computer.assisted", "ai in.*educat",

  # specific IES program areas
  "english learner", "ell\\b", "bilingual",
  "professional development", "teacher",
  "postsecondary", "college", "university",
  "transition.*postsecondary", "transition.*adult",

  # measurement / methodology relevant to ed research
  "value.added", "item response", "irt\\b",
  "psychometric", "scale", "factor analysis",
  "topic modeling", "text readability",
  "statistical model",

  # policy / implementation
  "implementation science", "policy", "research-to-practice",

  # behavioral health if school-relevant
  "behavior", "sel\\b", "social.emotional", "social emotional",

  # library / information science of education
  "library", "information literacy"
)
whitelist_re <- paste0("(?i)", paste(education_kw, collapse = "|"))


#-------------------------------
## 2. APPLY WHITELIST ##
#-------------------------------

# rescue patterns - specific titles we know are legit IES research but
# whose OpenAlex primary_topic is mis-tagged (e.g., PATHS, a famous SEL
# curriculum, gets tagged as "Diet and metabolism studies"). these are
# title-only patterns; matched case-insensitively. add new ones here as
# audit cases come up
rescue_titles <- c(
  "Promoting Alternative THinking Strategies",  # PATHS - SEL curriculum
  "PATHS",                                       # same, abbreviated
  "English language skills",                     # ELL research
  "Partial Least Squares Structural Equation",   # PLS-SEM stats methods
  "Prevention Science as a Platform",            # implementation science
  "Explainable Artificial Intelligence",         # ed-tech / AI
  "Practice to Research and Back",               # implementation science
  "Social Service Agency"                        # implementation science
)
rescue_re <- paste0("(?i)", paste(rescue_titles, collapse = "|"))

# force-EXCLUDE overrides - the mirror image of the rescue list. specific
# (grant_id, doi) pairs we KNOW are off-topic mis-links even though the keyword
# whitelist clears them: the DOI's title or topic happens to carry an education
# keyword (e.g. "trial", "autis") but the paper is a clinical-medicine or
# epidemiology product wrongly attributed to an education grant. surfaced by the
# policy-before-publication audit (audit_policy_before_pub.R) as hard off-topic
# mis-links. listing them here drops them from the UNIVERSE (stage 01), not just
# from the timing events the timing grace filter handles. add new
# confirmed mis-links as audit cases come up
force_exclude <- tribble(
  ~grant_id,     ~doi,
  "R324A110353", "10.15585/mmwr.ss6904a1",         # CDC MMWR autism-prevalence surveillance (not a product of this autism-ed grant)
  "R305A110483", "10.1016/s0140-6736(14)60845-x",  # REVEL lung-cancer phase-3 trial (Lancet), mis-linked to an ed grant
  "R305A080063", "10.1145/3485128"                 # "Tackling Climate Change with ML" false-matched to a chemistry-tutor grant that produced no indexed publications (IES page lists none)
)
force_exclude_key <- paste(str_to_upper(force_exclude$grant_id),
                           str_to_lower(str_trim(force_exclude$doi)))

# test each candidate against the regex. either field gets a vote -
# if primary_topic OR title contains an ed keyword, keep the DOI
audit <- audit %>%
  mutate(
    topic_match = str_detect(replace_na(primary_topic, ""), whitelist_re),
    title_match = str_detect(replace_na(title, ""),         whitelist_re),
    rescued     = str_detect(replace_na(title, ""),         rescue_re),
    force_excluded = paste(str_to_upper(grant_id), str_to_lower(str_trim(doi))) %in%
                     force_exclude_key,
    # a force-excluded pair jumps the whitelist: even a rescued / keyword-matched
    # row is dropped when it's a known off-topic mis-link
    whitelist_kept = (topic_match | title_match | rescued) & !force_excluded,
    matched_via = case_when(
      force_excluded                        ~ "force-excluded (off-topic mis-link override)",
      rescued & !topic_match & !title_match ~ "rescue list",
      topic_match & title_match  ~ "topic + title",
      topic_match                ~ "topic",
      title_match                ~ "title",
      TRUE                       ~ "(no match)"
    )
  )

cat("Whitelist results on the 403 audit rows:\n")
audit %>% count(whitelist_kept) %>%
  mutate(pct = sprintf("%.1f%%", 100 * n / sum(n))) %>%
  print()

cat("\nMatch source for the kept rows:\n")
audit %>% filter(whitelist_kept) %>% count(matched_via) %>% print()

cat("\nDistinct DOIs kept vs flagged for exclusion:\n")
audit %>% distinct(doi, whitelist_kept) %>%
  count(whitelist_kept) %>% print()

cat("\nForce-excluded off-topic mis-links (whitelist overridden):\n")
audit %>% filter(force_excluded) %>%
  transmute(grant_id, doi, primary_field, title) %>% print(width = Inf)


#-----------------------------------------
## 3. WRITE THE EXCLUSION CANDIDATE FILE ##
#-----------------------------------------

# rows the whitelist did NOT clear. these go to a manual-review CSV
# stage 01 will read this file and drop matching DOIs from the universe
# edit the file by hand (remove rows to put back into universe, add
# rows by DOI to forcibly exclude additional matches)
exclusion <- audit %>%
  filter(!whitelist_kept) %>%
  transmute(
    grant_id, doi,
    primary_field,
    primary_topic,
    publication_year,
    openalex_type,
    title,
    fwci,
    cited_by_count,
    # provenance - which source contributed this pair?
    src_eric_r3, src_manual, src_canonical, src_original, src_crossref,
    # placeholder column for overriding decisions on specific DOIs -
    # "keep" rescues a row from the exclusion
    decision = "exclude"
  ) %>%
  arrange(primary_field, primary_topic, grant_id)

excl_path <- file.path(review_dir, "offtopic_exclusion_candidates.csv")
write_csv(exclusion, excl_path)

cat("\nWrote exclusion candidate list (", nrow(exclusion), " rows):\n  ",
    excl_path, "\n")

# also write the kept set for audit purposes
kept <- audit %>%
  filter(whitelist_kept) %>%
  transmute(grant_id, doi, primary_field, primary_topic, matched_via, title)

kept_path <- here("outputs", "_paper_audit", "offtopic_whitelist_kept.csv")
write_csv(kept, kept_path)

cat("\nWrote kept-set audit (", nrow(kept), " rows):\n  ", kept_path, "\n")


#-------------------------
## 4. SUMMARY ##
#-------------------------

cat("\nSummary:\n")
cat("  Audit candidates total:                  ", nrow(audit), "rows (",
    n_distinct(audit$doi), "DOIs)\n")
cat("  Whitelist kept:                          ", sum(audit$whitelist_kept),
    "rows (", n_distinct(audit$doi[audit$whitelist_kept]), "DOIs)\n")
cat("  Flagged for exclusion (manual review):   ", sum(!audit$whitelist_kept),
    "rows (", n_distinct(audit$doi[!audit$whitelist_kept]), "DOIs)\n")
cat("\nNext step: review", excl_path, "\n")
cat("Edit the 'decision' column - set to 'keep' for any DOI you want\n")
cat("to put back into the universe. then re-run scripts_rewrite/01.\n")
