# survey_lca_config.R
# The latent class arm: which items, which settings. 
# Sources survey_data_read.R for the design and demographics

# Sourced after survey_lca_source.R by survey_lca_report.qmd.

source("survey_data_read.R")

# 1. Settings
 # select items and rename
#____________________________________________________________________
item_codes <- c(refrigerator  = "r3",
                computer      = "r15",
                home_internet = "r18",
                ran_out_food  = "fs2",
                water_worry   = "ws1",
                own_finances  = "idio2",
                feel_unsafe   = "aoj11",
                crime_victim  = "vic1ext",
                govt_aid      = "wf1",
                cash_transfer = "cct1b",
                emigrate      = "q14")

# TRUE fits on item-complete cases. 
# FALSE fits on everyone with at least min_items answered, using the EM's 
#   own handling of missing items.
complete_cases <- TRUE
min_items <- 6L


# 2. Items
# select() with a named vector renames on the way through. 
# Items are recoded to consecutive integers here, on every row, so the 
# estimation frame and the prediction frame are always on the same coding. 
# Levels come from the rows that will actually be fitted; a value seen only 
# outside that set becomes NA and drops out of that respondent's product.
#____________________________________________________________________

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

# 3. Dictionary
# Question wording and response labels in item_levels order, so the response 
# text lines up with the fitted category indices.
#____________________________________________________________________

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

# 4. Configuration
# K_force is the analyst decision: leave NULL, render, read the enumeration
#   evidence and the diagnostics, set a candidate, re-render.
#____________________________________________________________________

cfg <- list(
  items = items,
  aux = demos,
  strata = "strata", psu = "psu", weight = "wt", id = "id",
  cats = cats,
  min_items = min_items,
  
  K_range = 2:10,
  K_force = 4,
  n_starts = 200,
  
  seed = 2026,
  parallel = TRUE,
  workers = NULL,
  
  lca_dir = here::here( "output", "lca"),
  
  # LLM attributes
  compass_base_url = NULL,
  llm_model = "google/gemma-4-31b-it",
  
  survey_context = paste(
    "These items come from the 2023 AmericasBarometer survey of Ecuador,",
    "conducted by the LAPOP Lab at Vanderbilt University. Fieldwork was carried",
    "out face to face in Spanish by IPSOS between February and April 2023 with",
    "1,604 respondents, drawn by a multi-stage probability design stratified by",
    "the three major regions of the country: Costa, Sierra, and Oriente.",
    "\n\nThe items analysed here record the material circumstances of the",
    "household and the respondent's own situation: whether the household owns",
    "particular durable goods, whether it has run short of food or water,",
    "whether finances and income have improved or worsened, whether the",
    "respondent feels unsafe or has been a victim of crime, whether the",
    "household receives government assistance or a conditional cash transfer,",
    "and",
    "whether the respondent intends to emigrate. The segments summarise",
    "patterns of economic vulnerability across these items.")
)

cfg$data <- filter(survey_dat_full, in_analysis)

dir.create(cfg$lca_dir, showWarnings = FALSE, recursive = TRUE)