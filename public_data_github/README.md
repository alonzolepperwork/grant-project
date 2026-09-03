# Public data

This folder has the public data from our project on how IES-funded education research
gets cited in policy documents.

The data comes from public sources: the IES website (grants and award pages), ERIC
(products from each grant), and OpenAlex and Crossref (paper and meta-analysis details).
We kept the DOIs and IDs so the records can be matched back to those sources.

## Overton

The links from research to policy documents come from Overton, a licensed database we
are not permitted to redistribute. So you will not find any Overton content in this
repository.

## Folders

- **grants/** — the 528 NCER and NCSER grants (2002 to 2022) and grant-level info:
  center-grant flags, RCT flags, institution type, and efficacy/trial-type breakdowns.
- **publications/** — the DOIs each grant produced, with paper details from OpenAlex and
  Crossref, hand-added DOIs, and papers dropped for being dated before the grant started.
- **eric/** — ERIC records for these grants, a per-grant count of products, and products
  with no DOI.
- **ies_award_pages/** — data parsed from the IES award pages (PI, institution, project
  type, products).
- **lookups/** — a publisher-name lookup and a US government slug-to-level (federal,
  state, local) lookup.
- **analysis_dataset/** — `grants_publications_meta_public.csv`, the main file. One row
  per grant, DOI, and meta-analysis, with all grant and paper info. The policy and
  Overton columns have been removed.

## Notes

Grant records include PI names as listed by IES. The scripts use a placeholder email
(your_email_here@email.com) in their API requests; put your own if you rerun them.
