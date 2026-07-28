#analyzes the policy documents themselves rather than the grants
#every count in this script is at the distinct policy_document_id level,
#NOT pairs. a policy doc can be cited by many grants and many DOIs so the
#grant-doc route table from stage 02 has duplicates per doc - we de-dup
#before aggregating, every time
#
#it covers, in order:
#  - doc-level overview (counts, source type, year, language, publishers)
#  - geography (country canonicalization, US federal-vs-state split)
#  - per-region utilization (lag stats, top grants, multi-continent)
#  - workbook export
#  - normalized reach rates vs. an Overton education baseline

#------------------
## 0. INITIALIZE ##
#------------------

#load libraries
library(tidyverse)
library(here)

#load the routes table from stage 02 - this is the long table with grant_id,
#policy_document_id and all the policy-doc metadata attached
routes <- read_csv(
  here("outputs", "05_rebuild_current_policy_routes",
       "table_07_current_grant_policy_doc_routes.csv"),
  show_col_types = FALSE
)

#one-row-per-policy-document master. the routes table only carries the
#columns we needed for routing - title and rich metadata come from the
#policy doc reference table joined in below
docs <- routes %>%
  distinct(policy_document_id, policy_source_type,
           policy_source_country, policy_published_year, policy_document_url) %>%
  rename(country_raw = policy_source_country)
#nrow(docs) #~8,400 distinct first-order policy docs

#load the policy doc reference table for the rich metadata fields
docs_ref <- read_csv(
  here("outputs", "03_build_policy_doi_linkage_reference",
       "table_02_policy_document_reference.csv"),
  show_col_types = FALSE
)
#drop only the columns already in docs (came in from routes), so we
#still pick up policy_title, policy_source_title, policy_source_id and
#the rest from the reference table. dropping policy_title here used to
#leave docs without a title column, breaking the legacy dashboard
docs <- docs %>%
  left_join(docs_ref %>% select(-any_of(c("policy_source_type",
                                          "policy_source_country",
                                          "policy_published_year",
                                          "policy_document_url"))),
            by = "policy_document_id")

#-------------------------
## 1. OVERALL OVERVIEW ##
#-------------------------

#headline counts. keep these simple - they're what the executive-summary
#slide shows
overall_summary <- tibble(
  metric = c("Distinct policy documents",
             "Distinct (grant, policy doc) pairs",
             "Distinct grants reached by >=1 policy doc",
             "Avg policy docs per grant (over reached grants)",
             "Documents with publication year",
             "Documents with country tag",
             "Documents with source type tag"),
  value = c(
    nrow(docs),
    nrow(routes %>% distinct(grant_id, policy_document_id)),
    n_distinct(routes$grant_id),
    routes %>% count(grant_id) %>% summarize(round(mean(n), 1)) %>% pull(),
    sum(!is.na(docs$policy_published_year)),
    sum(!is.na(docs$country_raw)),
    sum(!is.na(docs$policy_source_type))
  )
)

#source type distribution
by_source_type <- docs %>%
  count(`Source type` = replace_na(policy_source_type, "(unspecified)"),
        name = "Documents") %>%
  mutate(`Share of total` = sprintf("%.1f%%", 100 * Documents / sum(Documents))) %>%
  arrange(desc(Documents))

#year distribution (pre-2004 docs are dropped as orphans - they're legacy
#Overton records not connected to any grant via the current route tables)
by_year <- docs %>%
  filter(!is.na(policy_published_year), policy_published_year >= 2004) %>%
  count(Year = policy_published_year, name = "Documents") %>%
  arrange(Year) %>%
  mutate(`Cumulative` = cumsum(Documents))

#top publishers
top_publishers <- docs %>%
  filter(!is.na(policy_source_title)) %>%
  count(Publisher = policy_source_title, name = "Documents") %>%
  arrange(desc(Documents)) %>% head(50)

#language distribution
by_language <- docs %>%
  count(Language = replace_na(language, "(unspecified)"), name = "Documents") %>%
  arrange(desc(Documents))

#-------------------------
## 2. GEOGRAPHY CLEANUP ##
#-------------------------

#the raw policy_source_country field mixes real countries (USA, UK, Germany)
#with supranational labels (IGO, EU). a naive "by country" count puts the
#World Bank in the same column as Sweden. we use an inline lookup table
#to split them: IGO and EU get continent = "(Supranational)" so they
#don't roll up with real continents

country_lookup <- tibble::tribble(
  ~raw, ~canonical, ~continent,
  "USA", "United States", "North America",
  "UK", "United Kingdom", "Europe",
  "Germany", "Germany", "Europe",
  "Australia", "Australia", "Oceania",
  "Sweden", "Sweden", "Europe",
  "Canada", "Canada", "North America",
  "Netherlands", "Netherlands", "Europe",
  "Ireland", "Ireland", "Europe",
  "Denmark", "Denmark", "Europe",
  "Norway", "Norway", "Europe",
  "Turkey", "Turkey", "Asia",
  "Chile", "Chile", "South America",
  "Finland", "Finland", "Europe",
  "Peru", "Peru", "South America",
  "Spain", "Spain", "Europe",
  "France", "France", "Europe",
  "Belgium", "Belgium", "Europe",
  "Kenya", "Kenya", "Africa",
  "Colombia", "Colombia", "South America",
  "New Zealand", "New Zealand", "Oceania",
  "Brazil", "Brazil", "South America",
  "Indonesia", "Indonesia", "Asia",
  "Slovakia", "Slovakia", "Europe",
  "Estonia", "Estonia", "Europe",
  "Switzerland", "Switzerland", "Europe",
  "Italy", "Italy", "Europe",
  "Latvia", "Latvia", "Europe",
  "Czech Republic", "Czech Republic", "Europe",
  "Iceland", "Iceland", "Europe",
  "Lithuania", "Lithuania", "Europe",
  "Malta", "Malta", "Europe",
  "Japan", "Japan", "Asia",
  "Mexico", "Mexico", "North America",
  "Portugal", "Portugal", "Europe",
  "Uganda", "Uganda", "Africa",
  "Uruguay", "Uruguay", "South America",
  "Venezuela", "Venezuela", "South America",
  "Argentina", "Argentina", "South America",
  "China", "China", "Asia",
  "Costa Rica", "Costa Rica", "North America",
  "Ecuador", "Ecuador", "South America",
  "Israel", "Israel", "Asia",
  "Malaysia", "Malaysia", "Asia",
  "Oman", "Oman", "Asia",
  "Papua New Guinea", "Papua New Guinea", "Oceania",
  "Saudi Arabia", "Saudi Arabia", "Asia",
  "Singapore", "Singapore", "Asia",
  "Slovenia", "Slovenia", "Europe",
  "South Africa", "South Africa", "Africa",
  "South Korea", "South Korea", "Asia",
  "Tanzania", "Tanzania", "Africa",
  "United Arab Emirates", "United Arab Emirates", "Asia",
  "Vietnam", "Vietnam", "Asia",
  "Hong Kong", "Hong Kong", "Asia",
  #countries that were falling through to "(unmapped)" - real countries Overton
  #records but that were missing from this lookup (added 2026-06-08)
  "Austria", "Austria", "Europe",
  "Romania", "Romania", "Europe",
  "Cyprus", "Cyprus", "Europe",
  "Hungary", "Hungary", "Europe",
  "Luxembourg", "Luxembourg", "Europe",
  "Moldova", "Moldova", "Europe",
  "Serbia", "Serbia", "Europe",
  "India", "India", "Asia",
  "Sri Lanka", "Sri Lanka", "Asia",
  "Iran", "Iran", "Asia",
  "Nepal", "Nepal", "Asia",
  "Philippines", "Philippines", "Asia",
  "Thailand", "Thailand", "Asia",
  "Iraq", "Iraq", "Asia",
  "Qatar", "Qatar", "Asia",
  "Syria", "Syria", "Asia",
  "Egypt", "Egypt", "Africa",
  "Ethiopia", "Ethiopia", "Africa",
  "Guinea", "Guinea", "Africa",
  "Mauritania", "Mauritania", "Africa",
  "Morocco", "Morocco", "Africa",
  "Nigeria", "Nigeria", "Africa",
  "Tunisia", "Tunisia", "Africa",
  "El Salvador", "El Salvador", "North America",
  "Guatemala", "Guatemala", "North America",
  "Vanuatu", "Vanuatu", "Oceania",
  "IGO", "Supranational - IGO", "(Supranational)",
  "EU", "Supranational - EU", "(Supranational)"
)

#publisher-name -> country fallback. Overton often leaves the country field
#blank even when the publisher clearly identifies it ("Government of Turkey",
#"State of Texas", "OECD", named national ministries). fill country_raw from the
#publisher (policy_source_title) before the country lookup runs, so these docs
#land in their real region instead of "(unmapped)". curated from the
#blank-country publishers actually present in the data; unmatched names stay
#NA -> "(unmapped)"
publisher_lookup <- read_csv(here("data", "_rewrite_outputs", "publisher_lookup.csv"),
                             show_col_types = FALSE)
pub_ctry <- setNames(publisher_lookup$ctry, publisher_lookup$publisher)
docs <- docs %>%
  mutate(country_raw = if_else(
    (is.na(country_raw) | country_raw == "") & !is.na(policy_source_title),
    unname(pub_ctry[policy_source_title]), country_raw))

docs <- docs %>%
  left_join(country_lookup, by = c("country_raw" = "raw")) %>%
  mutate(
    canonical_country = if_else(is.na(canonical) & !is.na(country_raw),
                                paste0("(unmapped) ", country_raw), canonical),
    continent = replace_na(continent, "(unmapped)")
  )

#by country
by_country <- docs %>%
  filter(!is.na(country_raw)) %>%
  count(Country = canonical_country, Continent = continent, name = "Documents") %>%
  arrange(desc(Documents))

#by continent (supranational gets its own row)
by_continent <- docs %>%
  filter(!is.na(country_raw)) %>%
  count(Continent = continent, name = "Documents") %>%
  arrange(desc(Documents))

#by REGION - same as continent but the United States is split out of North
#America (the Section C geography table). includes Supranational (IGO) and a
#single "(unmapped / no country)" row (mostly docs Overton records no country
#for). also counts the distinct grants whose research reaches each region
docs_region <- docs %>%
  mutate(region = case_when(
           canonical_country == "United States" ~ "United States",
           continent == "North America"         ~ "North America (excl. US)",
           continent == "Europe"                ~ "Europe",
           continent == "Asia"                  ~ "Asia",
           continent == "Africa"                ~ "Africa",
           continent == "South America"         ~ "South America",
           continent == "Oceania"               ~ "Australia / Oceania",
           str_detect(continent, "Supranational") ~ "Supranational (IGO)",
           TRUE                                 ~ "(unmapped / no country)"),
         is_gov_doc = policy_source_type == "government" & !is.na(policy_source_type))
region_levels <- c("United States", "North America (excl. US)", "Europe", "Asia",
                   "Africa", "South America", "Australia / Oceania",
                   "Supranational (IGO)", "(unmapped / no country)")
region_grants <- routes %>%
  mutate(policy_document_id = as.character(policy_document_id)) %>%
  left_join(docs_region %>% transmute(policy_document_id = as.character(policy_document_id), region),
            by = "policy_document_id") %>%
  group_by(region) %>% summarize(`Grants reaching` = n_distinct(grant_id), .groups = "drop")
by_region <- docs_region %>%
  group_by(region) %>%
  summarize(Documents = n(), `Gov docs` = sum(is_gov_doc), .groups = "drop") %>%
  left_join(region_grants, by = "region") %>%
  mutate(`% of docs` = sprintf("%.1f%%", 100 * Documents / sum(Documents)),
         region = factor(region, levels = region_levels)) %>%
  arrange(region) %>%
  select(Region = region, Documents, `% of docs`, `Gov docs`, `Grants reaching`)

#-----------------------------
## 3. US FEDERAL VS STATE ##
#-----------------------------

#within the USA bucket, some are state/local-government docs and some are
#federal-agency docs and some are think-tank publications with US in the
#country field. we want a 4-way split that distinguishes:
#  Federal government (US federal agencies, congress, federal reserve)
#  Think tank / Research org (US-based but not government)
#  State/Local government (states, commonwealths, cities, counties, DC)
#  Other US (everything left over)
#
#the hard part is federal-vs-state for the government docs. the old version
#only matched "^State of X$" on the title, but most state docs carry the
#state only in the source_id slug ("stateofwashington", "texasgov",
#"california_state_agencies") or as "X Government" -- so they were misfiled
#as Federal. classify_us() fixes this: it reads title OR slug, guards the
#Federal Reserve banks first (they embed city/state names but are federal),
#then detects state/local via explicit "State/Commonwealth/City/County of",
#state-name + government marker, or known city/abbrev gov slugs

us_state_names <- c(
  Alabama="alabama", Alaska="alaska", Arizona="arizona", Arkansas="arkansas",
  California="california", Colorado="colorado", Connecticut="connecticut",
  Delaware="delaware", Florida="florida", Georgia="georgia", Hawaii="hawaii",
  Idaho="idaho", Illinois="illinois", Indiana="indiana", Iowa="iowa",
  Kansas="kansas", Kentucky="kentucky", Louisiana="louisiana", Maine="maine",
  Maryland="maryland", Massachusetts="massachusetts", Michigan="michigan",
  Minnesota="minnesota", Mississippi="mississippi", Missouri="missouri",
  Montana="montana", Nebraska="nebraska", Nevada="nevada",
  `New Hampshire`="newhampshire", `New Jersey`="newjersey",
  `New Mexico`="newmexico", `New York`="newyork",
  `North Carolina`="northcarolina", `North Dakota`="northdakota", Ohio="ohio",
  Oklahoma="oklahoma", Oregon="oregon", Pennsylvania="pennsylvania",
  `Rhode Island`="rhodeisland", `South Carolina`="southcarolina",
  `South Dakota`="southdakota", Tennessee="tennessee", Texas="texas",
  Utah="utah", Vermont="vermont", Virginia="virginia", Washington="washington",
  `West Virginia`="westvirginia", Wisconsin="wisconsin", Wyoming="wyoming"
)
#known abbreviated state/city government slugs that carry no full state name
abbrev_local_slugs <- c(ingov="Indiana", pagov="Pennsylvania",
  massgov="Massachusetts", nygov="New York", nycgov="New York",
  njgov="New Jersey", ctgov="Connecticut", seattlegov="Washington",
  bostongov="Massachusetts")

classify_us <- function(pub) {
  x  <- tolower(trimws(coalesce(pub, "")))
  si <- gsub("[^a-z]", "", x)                    #letters-only slug form
  level <- rep(NA_character_, length(x))
  state <- rep(NA_character_, length(x))
  for (i in seq_along(x)) {
    if (!nzchar(si[i])) next
    #1) federal reserve system - embeds city/state names but is federal
    if (grepl("federalreserve|reservebank", si[i]) || grepl("fed$", si[i])) {
      level[i] <- "Federal government"; next
    }
    #2) DC
    if (grepl("districtofcolumbia", si[i]) || si[i] %in% c("dcgov", "dc")) {
      level[i] <- "State/Local government"; state[i] <- "District of Columbia"; next
    }
    #3) explicit "State of X" / "Commonwealth of X" (captures the state name)
    mt <- regmatches(x[i], regexec("^(?:state|commonwealth) of ([a-z ]+)$", x[i]))[[1]]
    if (length(mt) >= 2) {
      level[i] <- "State/Local government"; state[i] <- tools::toTitleCase(trimws(mt[2])); next
    }
    #4) city / county (title or slug form) - local government
    if (grepl("^(?:city|county) of ", x[i]) || grepl("^(?:cityof|countyof)", si[i])) {
      level[i] <- "State/Local government"; next
    }
    #5) interstate compact (education commission of the states)
    if (grepl("commissionofthestates", si[i]) || grepl("^educationcommission", si[i])) {
      level[i] <- "State/Local government"; next
    }
    #6) state name in the slug + a government marker (texasgov, marylandgov,
    #   californiastateagencies, austintexasgov, stateof* slugs)
    matched <- NA_character_
    for (nm in names(us_state_names)) {
      if (grepl(us_state_names[[nm]], si[i], fixed = TRUE)) { matched <- nm; break }
    }
    if (!is.na(matched) &&
        (grepl("gov|government|stateagencies|legislature|assembly", si[i]) ||
         grepl("^stateof", si[i]))) {
      level[i] <- "State/Local government"; state[i] <- matched; next
    }
    #7) known abbreviated state/city gov slugs
    if (si[i] %in% names(abbrev_local_slugs)) {
      level[i] <- "State/Local government"; state[i] <- abbrev_local_slugs[[si[i]]]; next
    }
    #else: remaining US government publisher -> federal
    level[i] <- "Federal government"
  }
  list(level = level, state = state)
}

#classify every doc, then keep the level only for US docs (gov -> fed/state,
#think tank/other US get their own label); us_state only for US gov docs
.uc <- classify_us(coalesce(na_if(docs$policy_source_title, ""), docs$policy_source_id))
.src <- replace_na(docs$policy_source_type, "other")
docs <- docs %>% mutate(
  us_level = case_when(
    canonical_country != "United States" ~ NA_character_,
    .src == "government"  ~ .uc$level,
    .src == "think tank"  ~ "Think tank / Research org",
    TRUE                  ~ "Other US"
  ),
  us_state = if_else(canonical_country == "United States" & .src == "government",
                     .uc$state, NA_character_)
)

us_docs <- docs %>% filter(canonical_country == "United States")

us_split <- us_docs %>%
  count(`US level` = us_level, name = "Documents") %>%
  mutate(`Share of US total` = sprintf("%.1f%%",
                                        100 * Documents / sum(Documents)))

by_us_state <- us_docs %>%
  filter(!is.na(us_state)) %>%
  count(`US state` = us_state, name = "Documents") %>%
  arrange(desc(Documents))

#publisher normalization (Overton sometimes stores only a slug in
#policy_source_id when the title is blank - so "OPRE" and "opregovus"
#are the same agency. this lookup collapses known slugs.)
source_id_normalize <- c(
  "opregovus" = "OPRE",
  "randcorporation" = "RAND Corporation",
  "mathematicaus" = "Mathematica",
  "urbaninstitute" = "Urban Institute",
  "brookings" = "Brookings Institution",
  "nber" = "NBER",
  "gpogov" = "Government Publishing Office (GPO)",
  "dcgov" = "DC Government",
  "wested" = "WestEd",
  "ingov" = "Indiana Government",
  "stateofwashington" = "State of Washington",
  "stateofidaho" = "State of Idaho",
  "texasgov" = "Texas Government",
  "marylandgov" = "Maryland Government"
)

us_docs$publisher_norm <- coalesce(
  source_id_normalize[us_docs$policy_source_title],
  source_id_normalize[us_docs$policy_source_id],
  us_docs$policy_source_title,
  us_docs$policy_source_id
)

us_publishers_by_level <- us_docs %>%
  count(`US level` = us_level, Publisher = publisher_norm, name = "Documents") %>%
  group_by(`US level`) %>% slice_max(Documents, n = 25) %>% ungroup()

#----------------------------------
## 4. PER-REGION UTILIZATION ##
#----------------------------------

#how does IES research flow into policy in each region?
#top grants reached globally (by # distinct policy docs)
top_grants_global <- routes %>%
  group_by(grant_id) %>%
  summarize(
    n_docs = n_distinct(policy_document_id),
    n_countries = n_distinct(policy_source_country),
    .groups = "drop"
  ) %>%
  arrange(desc(n_docs)) %>% head(30)

#grants reaching 3+ continents (the highest-impact subset)
grant_continents <- routes %>%
  left_join(country_lookup, by = c("policy_source_country" = "raw")) %>%
  distinct(grant_id, continent) %>%
  filter(!is.na(continent), continent != "(unmapped)") %>%
  count(grant_id, name = "n_continents")
multi_continent_grants <- grant_continents %>%
  filter(n_continents >= 3) %>%
  arrange(desc(n_continents))

#per-country lag stats: median years from award to first policy citation
#in that country. need award_year per grant - join from master
master <- read_csv(
  here("outputs", "06_build_grant_policy_master",
       "table_01_grant_policy_master.csv"),
  show_col_types = FALSE
)

lag_by_country <- routes %>%
  filter(!is.na(policy_published_year)) %>%
  left_join(master %>% select(grant_id, award_year), by = "grant_id") %>%
  mutate(lag = as.integer(policy_published_year) - as.integer(award_year)) %>%
  filter(!is.na(lag), lag >= 0) %>%
  group_by(Country = policy_source_country) %>%
  summarize(
    n_grants = n_distinct(grant_id),
    median_lag = median(lag),
    mean_lag = round(mean(lag), 1),
    .groups = "drop"
  ) %>%
  filter(n_grants >= 3) %>%   #drop countries with too few grants for stable medians
  arrange(desc(n_grants))

#-----------------------------------
## 4b. SECOND-ORDER AMPLIFICATION ##
#-----------------------------------

#which first-order docs are the strongest amplifiers? load the long
#second-order link table (intermediate -> second_order) and rank each
#first-order doc by how many downstream docs cite it and how many
#distinct grants benefit. these are the high-leverage policy docs for
#IES research - the table to look at when the question is "which single
#document is doing the most amplification work?"

so_links_path <- here("outputs", "05_rebuild_current_policy_routes",
                      "table_08_current_grant_to_second_order_links.csv")

if (file.exists(so_links_path)) {
  so_links <- read_csv(so_links_path, show_col_types = FALSE)

  # per intermediate doc: out-degree (distinct downstream docs that cite it)
  # and how many distinct grants it serves as an amplifier for
  amplifiers <- so_links %>%
    group_by(intermediate_policy_document_id) %>%
    summarize(
      n_downstream_docs = n_distinct(second_order_policy_document_id),
      n_grants_amplified = n_distinct(grant_id),
      .groups = "drop"
    ) %>%
    arrange(desc(n_downstream_docs)) %>%
    left_join(docs %>% rename(intermediate_policy_document_id = policy_document_id),
              by = "intermediate_policy_document_id")

  # second-order doc characteristics - mirrors the first-order breakdowns
  # above (by_source_type, by_country) so we can compare amplification
  # across types/countries
  so_docs <- so_links %>%
    distinct(second_order_policy_document_id, policy_source_type,
             policy_source_country, policy_published_year)

  so_by_source_type <- so_docs %>%
    count(`Source type` = replace_na(policy_source_type, "(unspecified)"),
          name = "Documents") %>%
    mutate(`% of docs` = paste0(round(100 * Documents / sum(Documents), 1), "%")) %>%
    arrange(desc(Documents))

  so_by_country <- so_docs %>%
    count(`Country (raw)` = replace_na(policy_source_country, "(unspecified)"),
          name = "Documents") %>%
    arrange(desc(Documents)) %>% head(25)

} else {
  amplifiers <- tibble()
  so_by_source_type <- tibble()
  so_by_country <- tibble()
}

#----------------------------------------
## 4c. TOPIC CLASSIFICATION (OVERTON) ##
#----------------------------------------

#three Overton classification streams answer the "what is this doc
#about?" question at different granularities:
#  - classifications  = IPTC top-level (education, health, etc.) - 17ish
#  - sdgcategories    = UN SDGs
#  - topics           = granular keyword tags
#all are long-format (one row per (doc, tag)) and pre-filtered to the
#IES-relevant doc set by 00h. small enough to load with read_csv

classif_path <- here("data", "_rewrite_outputs",
                     "overton_full_classifications.csv")
sdg_path     <- here("data", "_rewrite_outputs", "overton_full_sdg.csv")
topics_path  <- here("data", "_rewrite_outputs", "overton_full_topics.csv")

if (file.exists(classif_path)) {

  classif <- read_csv(classif_path, show_col_types = FALSE)
  sdg     <- if (file.exists(sdg_path))
    read_csv(sdg_path, show_col_types = FALSE) else tibble()
  topics  <- if (file.exists(topics_path))
    read_csv(topics_path, show_col_types = FALSE) else tibble()

  # first-order doc IDs are the ones in `routes`. second-order IDs come
  # from the P2P link table built in stage 02
  first_order_doc_ids <- unique(routes$policy_document_id)

  so_links_for_classif <- if (file.exists(so_links_path))
    read_csv(so_links_path, show_col_types = FALSE) else tibble()
  second_order_doc_ids <- if (nrow(so_links_for_classif) > 0)
    unique(so_links_for_classif$second_order_policy_document_id) else character()

  # helper: tag distribution restricted to a doc set, with % of docs
  # tagged (denominator = distinct docs in the set, not row count)
  tag_distribution <- function(tag_df, tag_col, doc_ids, top_n = 30) {
    if (length(doc_ids) == 0 || nrow(tag_df) == 0) return(tibble())
    n_docs <- length(doc_ids)
    tag_df %>%
      filter(policy_document_id %in% doc_ids) %>%
      distinct(policy_document_id, !!sym(tag_col)) %>%
      count(!!sym(tag_col), name = "Documents") %>%
      mutate(`% of docs tagged` =
               paste0(round(100 * Documents / n_docs, 1), "%")) %>%
      arrange(desc(Documents)) %>%
      head(top_n)
  }

  # IPTC top-level: ~17 categories, no need to truncate
  classif_first  <- tag_distribution(classif, "classification",
                                     first_order_doc_ids,  top_n = 30)
  classif_second <- tag_distribution(classif, "classification",
                                     second_order_doc_ids, top_n = 30)

  # SDG: keep parent codes only ("SDG 4: Quality Education", not the
  # "SDG Target 4.7" sub-rows) so the table isn't flooded with targets
  if (nrow(sdg) > 0) {
    sdg_top_level <- sdg %>%
      filter(str_detect(sdgcategory, "^SDG [0-9]+:")) %>%
      distinct()
  } else {
    sdg_top_level <- tibble()
  }
  sdg_first  <- tag_distribution(sdg_top_level, "sdgcategory",
                                 first_order_doc_ids,  top_n = 17)
  sdg_second <- tag_distribution(sdg_top_level, "sdgcategory",
                                 second_order_doc_ids, top_n = 17)

  # granular topics: top 40 by first-order doc count. these are the
  # words/phrases Overton's classifier attaches, useful for sanity-
  # checking whether IES research is landing where we expect it to
  topics_first  <- tag_distribution(topics, "topic",
                                    first_order_doc_ids,  top_n = 40)
  topics_second <- tag_distribution(topics, "topic",
                                    second_order_doc_ids, top_n = 40)

  # side-by-side first vs second order classification mix. tells the
  # "are second-order docs in different topic spaces?" story directly
  classif_compare <- full_join(
    classif_first %>% select(classification, FO_docs = Documents,
                              FO_pct = `% of docs tagged`),
    classif_second %>% select(classification, SO_docs = Documents,
                               SO_pct = `% of docs tagged`),
    by = "classification"
  ) %>%
    mutate(FO_docs = replace_na(FO_docs, 0L),
           SO_docs = replace_na(SO_docs, 0L)) %>%
    arrange(desc(FO_docs))

  #-----------------------------------
  ## per-grant classification profile
  #-----------------------------------

  # Overton's classifications are hierarchical ("education>school>...")
  # collapse to the IPTC top-level (~17 categories) so per-grant indicators
  # are independent rather than a forest of overlapping sub-tags
  classif_top <- classif %>%
    mutate(classification_top = str_split(classification, ">", simplify = TRUE)[, 1] %>%
             str_trim() %>% na_if("")) %>%
    filter(!is.na(classification_top)) %>%
    distinct(policy_document_id, classification_top)

  # per grant, count first-order docs tagged with each top category +
  # convert to a share of the grant's reached docs
  grant_doc_pairs <- routes %>% distinct(grant_id, policy_document_id)
  grant_total_docs <- grant_doc_pairs %>%
    count(grant_id, name = "n_first_order_docs")

  grant_classif_long <- grant_doc_pairs %>%
    inner_join(classif_top, by = "policy_document_id",
               relationship = "many-to-many") %>%
    group_by(grant_id, classification_top) %>%
    summarize(n_docs_tagged = n_distinct(policy_document_id),
              .groups = "drop") %>%
    left_join(grant_total_docs, by = "grant_id") %>%
    mutate(pct_docs_tagged = round(100 * n_docs_tagged / n_first_order_docs, 1)) %>%
    arrange(grant_id, desc(n_docs_tagged))

  # wide binary/count table - one column per top-level category. used
  # by stage 03 to attach as master columns and by stage 08 for the
  # institution-type cross-tab
  top_cats <- c("education", "health", "economy, business and finance",
                "labour", "politics", "society", "science and technology",
                "environment", "human interest", "lifestyle and leisure")
  cat_to_col <- function(x) paste0("n_docs_tagged_",
                                    str_replace_all(x, "[^a-z]+", "_"))

  grant_classif_wide <- grant_classif_long %>%
    filter(classification_top %in% top_cats) %>%
    mutate(col = cat_to_col(classification_top)) %>%
    select(grant_id, col, n_docs_tagged) %>%
    pivot_wider(names_from = col, values_from = n_docs_tagged, values_fill = 0L) %>%
    left_join(grant_total_docs, by = "grant_id")

  # dominant classification per grant - "what's this grant mostly cited in?"
  # break ties by alphabetical order so the output is deterministic
  grant_dominant_classif <- grant_classif_long %>%
    group_by(grant_id) %>%
    arrange(desc(n_docs_tagged), classification_top) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(grant_id,
              dominant_classification = classification_top,
              dominant_n_docs = n_docs_tagged,
              dominant_pct_docs = pct_docs_tagged)

} else {
  classif_first <- tibble(); classif_second <- tibble()
  sdg_first <- tibble(); sdg_second <- tibble()
  topics_first <- tibble(); topics_second <- tibble()
  classif_compare <- tibble()
  grant_classif_long <- tibble(); grant_classif_wide <- tibble()
  grant_dominant_classif <- tibble()
}

#----------------------
## 5. WRITE OUTPUTS ##
#----------------------

#preserve original output paths
out12 <- here("outputs", "12_build_policy_doc_overview")
out12b <- here("outputs", "12b_build_policy_doc_geography")
out12c <- here("outputs", "12c_build_policy_doc_research_use_by_region")
walk(c(out12, out12b, out12c), ~dir.create(.x, showWarnings = FALSE, recursive = TRUE))

write_csv(docs, file.path(out12, "policy_doc_master.csv"))
write_csv(overall_summary, file.path(out12, "table_01_overall_summary.csv"))
write_csv(by_source_type, file.path(out12, "table_02_by_source_type.csv"))
write_csv(by_year, file.path(out12, "table_04_by_year.csv"))
write_csv(top_publishers, file.path(out12, "table_05_top_publishers.csv"))
write_csv(by_language, file.path(out12, "table_06_by_language.csv"))

#legacy dashboard scripts (13b, 14, 16) read this file and expect the
#full doc metadata schema (title, source, url, language, etc) plus the
#derived geo columns. write everything, renaming country_raw back to
#policy_source_country so downstream code keeps working
write_csv(docs %>% rename(policy_source_country = country_raw),
          file.path(out12b, "policy_doc_master_with_geo.csv"))
write_csv(by_country, file.path(out12b, "table_02_by_country.csv"))
write_csv(by_continent, file.path(out12b, "table_03_by_continent.csv"))
write_csv(by_region, file.path(out12b, "table_03b_by_region_us_split.csv"))
write_csv(us_split, file.path(out12b, "table_05_us_federal_vs_state.csv"))
write_csv(by_us_state, file.path(out12b, "table_06_by_us_state.csv"))
write_csv(us_publishers_by_level,
          file.path(out12b, "table_09_us_publishers_by_level.csv"))

write_csv(top_grants_global,
          file.path(out12c, "table_01_top_grants_globally.csv"))
write_csv(multi_continent_grants,
          file.path(out12c, "table_07_grants_reaching_3plus_continents.csv"))
write_csv(lag_by_country,
          file.path(out12c, "table_04_grant_policy_lag_by_country.csv"))

if (nrow(amplifiers) > 0) {
  write_csv(amplifiers,
            file.path(out12, "table_07_top_amplifying_intermediaries.csv"))
  write_csv(so_by_source_type,
            file.path(out12, "table_08_second_order_by_source_type.csv"))
  write_csv(so_by_country,
            file.path(out12b, "table_10_second_order_by_country.csv"))
}

# classification tables - written to a new subfolder so they don't
# collide with the existing 12_* output schema
out12e <- here("outputs", "12e_classify_policy_docs")
dir.create(out12e, showWarnings = FALSE, recursive = TRUE)

if (nrow(classif_first) > 0) {
  write_csv(classif_first,
            file.path(out12e, "table_01_classifications_first_order.csv"))
  write_csv(classif_second,
            file.path(out12e, "table_02_classifications_second_order.csv"))
  write_csv(classif_compare,
            file.path(out12e, "table_03_classifications_first_vs_second.csv"))
  write_csv(sdg_first,
            file.path(out12e, "table_04_sdg_first_order.csv"))
  write_csv(sdg_second,
            file.path(out12e, "table_05_sdg_second_order.csv"))
  write_csv(topics_first,
            file.path(out12e, "table_06_granular_topics_first_order.csv"))
  write_csv(topics_second,
            file.path(out12e, "table_07_granular_topics_second_order.csv"))
  if (nrow(grant_classif_long) > 0) {
    write_csv(grant_classif_long,
              file.path(out12e, "table_08_per_grant_classification_long.csv"))
    write_csv(grant_classif_wide,
              file.path(out12e, "table_09_per_grant_classification_wide.csv"))
    write_csv(grant_dominant_classif,
              file.path(out12e, "table_10_per_grant_dominant_classification.csv"))
  }
}

cat("Policy doc analysis done.\n")
cat("  Distinct docs:", nrow(docs), "\n")
cat("  Top country:", by_country$Country[1], "(", by_country$Documents[1], ")\n")
cat("  Multi-continent grants:", nrow(multi_continent_grants), "\n")
if (nrow(amplifiers) > 0) {
  top_src <- if ("policy_source_id" %in% names(amplifiers))
    amplifiers$policy_source_id[1] else NA
  if (is.na(top_src)) top_src <- "(no source_id)"
  cat("  Top amplifier:        ", top_src,
      "-", amplifiers$n_downstream_docs[1], "downstream docs\n")
  cat("  Distinct amplifying intermediaries:",
      nrow(amplifiers), "\n")
}
if (nrow(classif_first) > 0) {
  cat("\nTop 5 first-order classifications:\n")
  classif_first %>% head(5) %>%
    transmute(line = sprintf("  %-40s %5d (%s)",
                              classification, Documents, `% of docs tagged`)) %>%
    pull(line) %>% cat(sep = "\n")
  cat("\n")
}
