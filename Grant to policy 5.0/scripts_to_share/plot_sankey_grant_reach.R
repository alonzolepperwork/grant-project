# ============================================================================
# Grant-level reach Sankey with the four pathways to government policy marked.
# Recolored (blue spine Grants -> DOIs -> meta, green = government reach, purple =
# non-government, grey = drop-offs), sans-serif, high-resolution. The P1/P2/P3
# tags sit on the three flows that reach Gov policy and a key defines them:
#   P1 direct   P2 via non-gov   P3 via meta   P4 via meta then non-gov (within P2)
# networkD3 can't label individual links, so the tags + key are added as HTML
# overlays that Chrome renders during the webshot, then trimmed with magick.
#
# input : the grant-DOI-meta-policy megafile at the project root
# output: outputs/_sankey/sankey_grant_reach.{html,png}
# ============================================================================
suppressPackageStartupMessages({
  library(tidyverse); library(data.table); library(networkD3)
  library(htmlwidgets); library(webshot2); library(here)
})

megafile  <- here("..", "megafile_grant_doi_meta_policy_full(in).csv")
out_dir   <- here("outputs", "_sankey")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
gov_types <- c("government", "igo")           # government includes IGOs

raw <- fread(megafile,
             select = c("grant_id", "grant_has_doi", "route",
                        "policy_document_id", "policy_source_type"))
raw[, is_gov_doc  := tolower(trimws(policy_source_type)) %in% gov_types]
raw[, hits_policy := route %in% c("direct", "meta") & nzchar(trimws(policy_document_id))]

grants <- raw[, .(
  has_doi      = any(toupper(trimws(as.character(grant_has_doi))) == "TRUE"),
  has_meta     = any(route %in% c("meta", "meta_no_policy")),
  reach_nongov = any(hits_policy & !is_gov_doc),
  reach_gov    = any(hits_policy &  is_gov_doc)
), by = grant_id] %>% as_tibble()

## nodes (order = top-to-bottom stack), group tags drive colour ---------------
nodes <- tibble(
  name  = c("Grants", "DOIs", "No DOI", "Linked to meta", "No meta",
            "Non-gov policy", "No policy", "Gov policy", "Non-gov only"),
  group = c("grant", "doi", "nodoi", "meta", "passthru",
            "nongov", "stop", "gov", "nongov"))

n <- function(cond) sum(cond); g <- grants
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
) %>% filter(value > 0)

sizes <- bind_rows(
  links %>% group_by(name = target) %>% summarise(v = sum(value), .groups = "drop"),
  links %>% filter(source == "Grants") %>% summarise(name = "Grants", v = sum(value)))
nodes <- nodes %>% left_join(sizes, by = "name") %>%
  mutate(label = sprintf("%s (%d)", name, v))

node_id <- function(name) match(name, nodes$name) - 1L
# colour each flow by where it is HEADING (target node's group): green toward Gov
# policy, purple toward non-gov, blue along the spine, grey into the drop-offs
links <- links %>%
  mutate(source_id = node_id(source), target_id = node_id(target),
         lgroup = nodes$group[match(target, nodes$name)])
cat("Sankey flows:\n"); print(links %>% select(source, target, value))

## palette: blue spine, green gov, purple non-gov, greys for drop-offs --------
palette <- 'd3.scaleOrdinal()
  .domain(["grant","doi","meta","nongov","gov","nodoi","passthru","stop"])
  .range(["#9ecae1","#4292c6","#08519c","#8c6bb1","#238b45","#bdbdbd","#dcdcdc","#d0d0d0"])'

fig <- sankeyNetwork(
  Links = as.data.frame(links), Nodes = as.data.frame(nodes),
  Source = "source_id", Target = "target_id", Value = "value",
  NodeID = "label", NodeGroup = "group", LinkGroup = "lgroup",
  colourScale = palette,
  fontFamily = "Helvetica, Arial, sans-serif",
  fontSize = 14, nodeWidth = 18, nodePadding = 22,
  height = 600, width = 1100,
  margin = list(top = 20, right = 130, bottom = 20, left = 20),
  iterations = 0, sinksRight = FALSE)

html <- file.path(out_dir, "sankey_grant_reach.html")
png  <- file.path(out_dir, "sankey_grant_reach.png")
saveWidget(fig, html, selfcontained = FALSE)

# P1/P2/P3 tags on the three flows into Gov policy (HTML overlays; positions are
# in the 1100x600 widget space, approximate).
tag_div <- function(lbl, left, top)
  sprintf('<div style="position:absolute;left:%dpx;top:%dpx;font:700 15px Helvetica,Arial,sans-serif;background:rgba(255,255,255,.85);padding:0 3px;z-index:9">%s</div>',
          left, top, lbl)
overlay <- paste0(
  '<style>body{margin:0}</style>',
  tag_div("P2", 840,  70),    # big Non-gov -> Gov flow (via non-gov)
  tag_div("P3", 612, 150),    # Linked-to-meta -> Gov flow (via meta)
  tag_div("P1", 730, 322))    # No-meta -> Gov flow (direct)
txt <- paste(readLines(html, warn = FALSE), collapse = "\n")
txt <- sub("</body>", paste0(overlay, "</body>"), txt, fixed = TRUE)
writeLines(txt, html)

webshot2::webshot(html, png, vwidth = 1100, vheight = 615, zoom = 4)
sank <- magick::image_trim(magick::image_read(png))

# pathway key rendered as its own strip (reliable placement) and stacked below
keyhtml <- file.path(out_dir, "sankey_key.html"); keypng <- file.path(out_dir, "sankey_key.png")
writeLines(paste0('<html><body style="margin:0">',
  '<div style="width:1080px;font:14px Helvetica,Arial,sans-serif;color:#444;text-align:center;padding:6px">',
  'Pathways to Gov policy &nbsp;&nbsp;&nbsp; P1 direct &nbsp;&nbsp;&nbsp; P2 via non-gov ',
  '&nbsp;&nbsp;&nbsp; P3 via meta &nbsp;&nbsp;&nbsp; P4 via meta then non-gov</div></body></html>'), keyhtml)
webshot2::webshot(keyhtml, keypng, vwidth = 1100, vheight = 44, zoom = 4)
key <- magick::image_trim(magick::image_read(keypng))
W <- magick::image_info(sank)$width
key <- magick::image_extent(key, geometry = sprintf("%dx%d", W, magick::image_info(key)$height + 60),
                            gravity = "center", color = "white")
out <- magick::image_border(magick::image_append(c(sank, key), stack = TRUE), "white", "40x24")
magick::image_write(out, png)
cat("wrote", png, "\n")
