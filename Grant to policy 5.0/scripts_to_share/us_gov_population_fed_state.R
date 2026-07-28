# ============================================================================
# US-GOVERNMENT POPULATION BASELINE: FEDERAL vs STATE vs LOCAL
# ============================================================================
# the federal/state split reported for our IES-cited subset (see 05/06) answers
# "of the US-government docs that cite IES research, how many are federal?". this
# script answers the baseline question: across the ENTIRE Overton US-government
# population, what is the federal / state / local mix? it is the denominator that
# shows whether the IES subset over- or under-represents any tier
#
# one pass over df_policy_doc_info.csv (~2.9 GB, no network): dedup docs, keep
# source_type=government & country=USA, reduce each publisher to a normalized
# slug, then classify the slug as Federal / State / Local from the committed
# lookup and tally. 95.6% of US-gov docs sit on .gov hosts and all 130 distinct
# slugs in the 20260305 dump are enumerated in the lookup, so the classification
# is deterministic with no residual
#
# the lookup (data/_rewrite_outputs/us_gov_slug_level_lookup.csv) is a hand-curated
# map of all 130 US-gov slugs to a tier. city governments (chicagogov, lacitygov,
# houstontxgov, cityof*, nycgov, seattlegov, ...) are Local; Federal Reserve
# district banks (clevelandfed, kansascityfed, ...) stay Federal; state agencies
# and state portals are State. this corrects the old default-to-Federal bug
#
# outputs (outputs/_overton_baseline/):
#   us_gov_slug_counts.csv          per-slug doc counts (recon)
#   us_gov_slug_level_map.csv       slug + docs + level (audit)
#   us_gov_population_fed_state.csv  level tally (Federal / State / Local)

#------------------
## 0. INITIALIZE ##
#------------------

library(data.table)
library(here)

overton_dir <- here("Overton_20260305")
out_dir     <- here("outputs", "_overton_baseline")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

#-------------------------------------------------
## 1. LOAD DOC INFO, DEDUP, KEEP US GOVERNMENT ##
#-------------------------------------------------

di <- fread(file.path(overton_dir, "df_policy_doc_info.csv"),
            select = c("policy_document_id", "policy_source_id",
                       "policy_source_type", "policy_source_country"),
            encoding = "UTF-8")

cat("doc_info rows:", nrow(di), "| distinct ids:", uniqueN(di$policy_document_id), "\n")
di <- di[!duplicated(policy_document_id)]

di[, src  := tolower(trimws(as.character(policy_source_type)))]
di[, ctry := tolower(trimws(as.character(policy_source_country)))]

# fed-vs-state only makes sense for US-government docs; igo is supranational and
# other source_types are not government
gov <- di[src == "government" & ctry == "usa"]
cat("US-government docs (deduped):", nrow(gov), "\n")
rm(di); invisible(gc())

#-------------------------------------------------
## 2. NORMALIZE SLUG + PER-SLUG COUNTS ##
#-------------------------------------------------

# reduce the publisher id to bare lowercase letters so "Texas.gov" / "texasgov"
# / "texas_gov" all collapse to one slug
gov[, slug := tolower(gsub("[^a-z]", "", as.character(policy_source_id)))]

slug_counts <- gov[slug != "", .(docs = .N), by = slug][order(-docs)]
fwrite(slug_counts, file.path(out_dir, "us_gov_slug_counts.csv"))

#-------------------------------------------------
## 3. CLASSIFY SLUG -> LEVEL + TALLY ##
#-------------------------------------------------

lookup <- fread(here("data", "_rewrite_outputs", "us_gov_slug_level_lookup.csv"))
m <- merge(slug_counts, lookup, by = "slug", all.x = TRUE)

unmatched <- m[is.na(level)]
if (nrow(unmatched) > 0L) {
  cat("WARNING:", nrow(unmatched), "slug(s) not in the level lookup (",
      sum(unmatched$docs), "docs) -> tagged 'Unclassified'. add them to",
      "us_gov_slug_level_lookup.csv:\n")
  print(unmatched[order(-docs)][seq_len(min(20L, nrow(unmatched)))])
  m[is.na(level), level := "Unclassified"]
}

slug_map <- m[order(-docs), .(slug, docs, level)]
fwrite(slug_map, file.path(out_dir, "us_gov_slug_level_map.csv"))

pop   <- m[, .(slugs = .N, docs = sum(docs)), by = level][order(-docs)]
grand <- sum(pop$docs)
pop[, pct := sprintf("%.1f%%", 100 * docs / grand)]

fed     <- sum(pop[level == "Federal", docs])
two_way <- data.table(
  level = c("Federal", "State-Local"),
  docs  = c(fed, grand - fed),
  pct   = sprintf("%.1f%%", 100 * c(fed, grand - fed) / grand))

fwrite(pop, file.path(out_dir, "us_gov_population_fed_state.csv"))

#---------------------
## 4. CONSOLE REPORT ##
#---------------------

cat(sprintf("\nFull Overton US-government population: %s docs across %d slugs\n",
            format(grand, big.mark = ","), nrow(slug_map)))
cat("\n-- three-way (Federal / State / Local) --\n"); print(pop)
cat("\n-- two-way (paper framing) --\n");             print(two_way)
cat("\nwrote us_gov_slug_counts.csv + us_gov_slug_level_map.csv +",
    "us_gov_population_fed_state.csv to", out_dir, "\n")
