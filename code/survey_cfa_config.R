# survey_cfa_config.R
# The factor analysis arm: which items, which settings. Sources
# survey_data_read.R for the design and demographics, which both arms share.
#
# Sourced after survey_lca_source.R by survey_cfa_report.qmd.

source("survey_data_read.R")

# ---- 1. Settings ------------------------------------------------------------
# Card B: every item on the same one to seven ladder, all asked of every
# respondent. Four items measure diffuse support for the political system and
# nine measure trust in specific institutions. A battery of one format asking
# one kind of question about different objects, which is the case a factor model
# is built for. Comment out an item to drop it, with the reason.

item_codes <- c(respect_institutions = "b2",
                rights_protected     = "b3",
                system_pride         = "b4",
                system_support       = "b6",
                armed_forces         = "b12",
                legislature          = "b13",
                police               = "b18",
                political_parties    = "b21",
                president            = "b21a",
                supreme_court        = "b31",
                municipality         = "b32",
                media                = "b37",
                elections            = "b47a")

# WLSMV scores only complete cases, so the complete-case rule is not optional
# here the way it is in the class arm.
complete_cases <- TRUE
min_items <- 7L

# ---- 2. Items ---------------------------------------------------------------
# select() with a named vector renames on the way through. Items are recoded to
# consecutive integers here, on every row, so the estimation frame and the
# prediction frame are always on the same coding. Levels come from the rows that
# will actually be fitted; a value seen only outside that set becomes NA and
# drops out of that respondent's product.

items <- names(item_codes)

item_dat <- raw_survey_dat |>
  select(all_of(item_codes)) |>
  mutate(across(everything(), function(x) {
    v = as.numeric(unclass(x))
    if_else(v %in% codes_to_drop, NA_real_, v)
  }))

n_answered <- rowSums(!is.na(item_dat))
in_analysis <- if (complete_cases) n_answered == length(items) else n_answered >= min_items

item_levels <- map(item_dat[in_analysis, ], function(x) sort(unique(x[!is.na(x)])))
cats <- map_int(item_levels, length)

item_dat <- item_dat |>
  mutate(across(everything(), function(x) match(x, item_levels[[cur_column()]])))

survey_dat_full <- bind_cols(design_dat, item_dat, demo_dat) |>
  mutate(in_analysis = in_analysis)

# ---- 3. Dictionary ----------------------------------------------------------
# Question wording and response labels in item_levels order, so the response text
# lines up with the fitted category indices. This is what the labelling prompt
# reads.

dictionary <- tibble(item = items, variable = unname(item_codes)) |>
  mutate(
    question = map_chr(variable, function(v) {
      lab = attr(raw_survey_dat[[v]], "label", exact = TRUE)
      if (is.character(lab) && length(lab) == 1 && nzchar(lab)) lab else v
    }),
    responses = map2(variable, item, function(v, it) {
      vl = attr(raw_survey_dat[[v]], "labels", exact = TRUE)
      key = if (length(vl)) set_names(names(vl), as.character(unname(vl))) else character(0)
      vals = as.character(item_levels[[it]])
      unname(if_else(vals %in% names(key), key[vals], vals))
    }))

# ---- 4. Configuration -------------------------------------------------------
# n_factors is the analyst decision: leave NULL, render, read the eigenvalue and
# stability evidence, set a count and the item assignment, re-render.
#
# cfa_factors names which items load on which factor once the search has settled
# it. NULL fits one factor over every item. Names become the factor names in the
# output. cfa_free takes residual covariances the modification indices flag, or a
# constraint such as "g ~~ 0*support" for a bifactor.
#
cfa_factors <- list(
   support = c("respect_institutions", "rights_protected",
               "system_pride", "system_support"),
   trust   = c("armed_forces", "legislature", "police", "political_parties",
               "president", "supreme_court", "municipality", "media",
               "elections"))

#cfa_factors <- NULL

cfg <- list(
  items = items,
  aux = demos,
  strata = "strata", psu = "psu", weight = "wt", id = "id",
  cats = cats,
  min_items = min_items,

  k_range = 1:4,
  n_factors = 2,
  cfa_factors = cfa_factors,
  cfa_free = NULL, # ex.  "armed_forces ~~ police"

  seed = 2026,
  parallel = TRUE,
  workers = NULL,

  cfa_dir = here::here("output", "cfa"),
  lca_dir = here::here("output", "lca"),   # read only, for the comparison section

  compass_base_url = NULL,
  llm_model = "google/gemma-4-31b-it",

  survey_context = paste(
    "These items come from the 2023 AmericasBarometer survey of Ecuador,",
    "conducted by the LAPOP Lab at Vanderbilt University. Fieldwork was carried",
    "out face to face in Spanish by IPSOS between February and April 2023 with",
    "1,604 respondents, drawn by a multi-stage probability design stratified by",
    "the three major regions of the country: Costa, Sierra, and Oriente.",
    "\n\nAll items analysed here are answered on the same seven-point ladder",
    "anchored at 1 (not at all) and 7 (a lot), shown to the respondent on a",
    "single card. Four ask how far the respondent supports the political system",
    "in general; the remaining nine ask how far the respondent trusts a specific",
    "national institution.")
)

cfg$data <- filter(survey_dat_full, in_analysis)

dir.create(cfg$cfa_dir, showWarnings = FALSE, recursive = TRUE)
