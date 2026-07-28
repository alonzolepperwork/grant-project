# ---------------------------------------------------------------------------
# Sankey: grant -> DOI -> meta -> non-government policy -> gov policy
# ---------------------------------------------------------------------------
# a grant-level flow figure. all 528 grants enter on the left and move through
# five stages; a grant drops out of the picture at whatever stage its reach
# ends, so the ribbon leaving a column is smaller than the one that entered it
# reading left to right:
#
#   Grants           every funded grant
#   DOIs             grants with at least one indexed publication; the rest end
#                    at "No DOI"
#   Meta             of those, the grants whose work was cited by a meta-analysis
#                    pass through "Linked to meta"; the others ("No meta") carry
#                    straight on - the meta column is a waypoint, not a wall
#   Non-gov policy   grants whose research reaches a non-government policy doc;
#                    the 31 that reach only non-government docs peel off into the
#                    short "Non-gov only" stub and end there
#   Gov policy       grants whose research reaches a government policy doc
#                    (government includes IGOs throughout this project)
#
# a grant reaches policy if any of its publications land in a policy document by
# either the direct or the meta route. counts line up with the pathway lattice:
# 48 have no DOI, 179 reach no policy, 31 are non-government only, 270 reach
# government, 301 reach policy in all
#
# networkD3 with iterations = 0, so the within-column vertical order follows the
# node order below (keeps Gov policy on top and the non-gov-only stub beneath it)
#
# input : the denormalised grant-DOI-meta-policy megafile (one row per
#         grant / DOI / meta / policy-doc combination), in the project root
# output: outputs/_sankey/sankey_grant_doi_meta_policy.{html,png}

library(tidyverse)
library(data.table)
library(networkD3)
library(htmlwidgets)
library(here)

## ---------------------------------------------------------------------------
## 1. read the megafile and reduce it to one row per grant
## ---------------------------------------------------------------------------

# the megafile sits in the project root, one level up from this pipeline folder
megafile  <- here("..", "megafile_grant_doi_meta_policy_full(in).csv")
gov_types <- c("government", "igo")            # government includes IGOs

raw <- fread(megafile,
             select = c("grant_id", "grant_has_doi", "route",
                        "policy_document_id", "policy_source_type"))

raw[, is_gov_doc  := tolower(trimws(policy_source_type)) %in% gov_types]
raw[, hits_policy := route %in% c("direct", "meta") &
                     nzchar(trimws(policy_document_id))]

# one row per grant carrying the four milestones the figure cares about
grants <- raw[, .(
  has_doi      = any(toupper(trimws(as.character(grant_has_doi))) == "TRUE"),
  has_meta     = any(route %in% c("meta", "meta_no_policy")),   # cited by a meta
  reach_nongov = any(hits_policy & !is_gov_doc),
  reach_gov    = any(hits_policy &  is_gov_doc)
), by = grant_id] %>% as_tibble()

message(sprintf(
  "grants: %d | with DOI: %d | cited by a meta: %d | reach gov: %d | reach any policy: %d",
  nrow(grants), sum(grants$has_doi), sum(grants$has_meta),
  sum(grants$reach_gov), sum(grants$reach_nongov | grants$reach_gov)))

## ---------------------------------------------------------------------------
## 2. nodes, and the drop-off links between them
## ---------------------------------------------------------------------------

# node ORDER sets the top-to-bottom stack within each column. group tags drive
# the colour: a coloured progression spine plus amber for the non-gov section
# (its node and its drop-off stub), green for government, grey for dead-ends
nodes <- tibble(
  name  = c("Grants", "DOIs", "No DOI", "Linked to meta", "No meta",
            "Non-gov policy", "No policy", "Gov policy", "Non-gov only"),
  group = c("grant", "doi", "nodoi", "meta", "passthru",
            "nongov", "stop", "gov", "nongov"))

# grants meeting a condition. each column partitions the grants that entered it
n <- function(cond) sum(cond)
g <- grants
links <- tribble(
  ~source,          ~target,          ~value,
  "Grants",         "DOIs",           n(g$has_doi),
  "Grants",         "No DOI",         n(!g$has_doi),
  "DOIs",           "Linked to meta", n(g$has_doi &  g$has_meta),
  "DOIs",           "No meta",        n(g$has_doi & !g$has_meta),
  "Linked to meta", "Non-gov policy", n(g$has_meta &  g$reach_nongov),
  "Linked to meta", "Gov policy",     n(g$has_meta &  g$reach_gov & !g$reach_nongov),
  "Linked to meta", "No policy",      n(g$has_meta & !g$reach_nongov & !g$reach_gov),
  "No meta",        "Non-gov policy", n(g$has_doi & !g$has_meta &  g$reach_nongov),
  "No meta",        "Gov policy",     n(g$has_doi & !g$has_meta &  g$reach_gov & !g$reach_nongov),
  "No meta",        "No policy",      n(g$has_doi & !g$has_meta & !g$reach_nongov & !g$reach_gov),
  "Non-gov policy", "Gov policy",     n(g$reach_nongov &  g$reach_gov),
  "Non-gov policy", "Non-gov only",   n(g$reach_nongov & !g$reach_gov)
) %>%
  filter(value > 0)

# label each node with its size - inflow, or outflow for the single source
sizes <- bind_rows(
  links %>% group_by(name = target) %>% summarise(v = sum(value), .groups = "drop"),
  links %>% filter(source == "Grants") %>% summarise(name = "Grants", v = sum(value)))
nodes <- nodes %>%
  left_join(sizes, by = "name") %>%
  mutate(label = sprintf("%s (%d)", name, v))

# networkD3 keys links by zero-based node position
node_id <- function(name) match(name, nodes$name) - 1L
links <- links %>%
  mutate(source_id = node_id(source), target_id = node_id(target))

print(links %>% select(source, target, value))

## ---------------------------------------------------------------------------
## 3. draw and save
## ---------------------------------------------------------------------------

# harmonised Tableau-10 palette: blue Grants, orange DOIs, purple Meta, gold
# Non-gov, green Gov; the "No DOI" dead-end is flagged red, other drop-offs /
# pass-through stay grey
palette <- 'd3.scaleOrdinal()
  .domain(["grant","doi","meta","nongov","gov","nodoi","passthru","stop"])
  .range(["#4e79a7","#f28e2b","#b07aa1","#edc948","#59a14f","#e15759","#bdbdbd","#d9d9d9"])'

fig <- sankeyNetwork(
  Links = as.data.frame(links), Nodes = as.data.frame(nodes),
  Source = "source_id", Target = "target_id", Value = "value",
  NodeID = "label", NodeGroup = "group", colourScale = palette,
  fontSize = 13, nodeWidth = 22, nodePadding = 16,
  iterations = 0, sinksRight = FALSE)

out_dir <- here("outputs", "_sankey")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

html <- file.path(out_dir, "sankey_grant_doi_meta_policy.html")
saveWidget(fig, html, selfcontained = FALSE)
webshot2::webshot(html, file.path(out_dir, "sankey_grant_doi_meta_policy.png"),
                  vwidth = 1100, vheight = 600, zoom = 2)

message("wrote sankey_grant_doi_meta_policy.{html,png} to ", out_dir)
