# survey_cfa_config.R
# The factor analysis arm: which items, which settings. Sources
# survey_data_read.R for the design and demographics, which both arms share.
#
# Sourced after source_code.R by survey_cfa_report.qmd, which is where the
#   shared helpers this file calls at the bottom come from.

if (!exists("check_config_columns"))
  stop("source('source_code.R') before survey_cfa_config.R.", call. = FALSE)

source("survey_data_read.R")

# ---- 1. Settings ------------------------------------------------------------
# Card B: every item on the same one to seven ladder, all asked of every
# respondent. Four items measure diffuse support for the political system and
# the rest measure trust in specific institutions. A battery of one format
# asking one kind of question about different objects, which is the case a
# factor model is built for. Comment out an item to drop it, with the reason.
#
# Two are commented out below with no reason recorded, which leaves eleven of
# the thirteen. Restore them or write the reason in: cfg$survey_context and the
# report both describe this battery to a reader, and the description below has
# been brought in line with eleven.

item_codes <- c(respect_institutions = "b2",
                rights_protected     = "b3",
                system_pride         = "b4",
                system_support       = "b6",
                armed_forces         = "b12",
                legislature          = "b13",
                police               = "b18",
                political_parties    = "b21",
                #president            = "b21a",
                supreme_court        = "b31",
                #municipality         = "b32",
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
# cfa_factors <- list(
#    support = c("respect_institutions", "rights_protected",
#                "system_pride", "system_support"),
#    trust   = c("armed_forces", "legislature", "police", "political_parties",
#                "president", "supreme_court", "municipality", "media",
#                "elections"))

# Identifiers, not interpretations. The construct names come from the frozen
# factor_labels.csv, so nothing in the config anchors the naming step.
cfa_factors = list(
  f1 = c("respect_institutions", "rights_protected",
         "system_pride", "system_support"),
  f2 = c("armed_forces", "legislature", "police",
         "political_parties", "supreme_court", "media", "elections")
)

#cfa_factors <- NULL

cfg <- list(
  items = items,
  aux = demos,
  strata = "strata", psu = "psu", weight = "wt", id = "id",
  cats = cats,
  min_items = min_items,

  k_range = 1:4,
  n_pa = 100,
  n_factors = 2,
  cfa_factors = cfa_factors,
  cfa_free = NULL, # ex.  "armed_forces ~~ police"

  seed = 2026,
  parallel = TRUE,
  workers = NULL,

  cfa_dir = here::here("output", "cfa"),

  # LLM endpoint. One word switches it: "openrouter" outside work, "work" for
  #   the OpenAI-compatible gateway inside it. The key is never named here --
  #   openrouter reads OPENROUTER_API_KEY and work reads OPENAI_API_KEY, both
  #   from ~/.Renviron, written unquoted as KEY=<value>. llm_check(cfg) in the
  #   report's setup chunk fails the render immediately if the one in use is
  #   missing, and prints which endpoint drafted the names below it.
  llm_provider = "openrouter",
  #   Two roles. The worker drafts one label per segment or factor, sees nothing
  #   else, and is called once per segment: small and cheap is the right choice.
  #   The editor is handed every label at once to tell near neighbours apart, and
  #   writes the domain readings; both need the whole set in view, and both are a
  #   handful of calls per render, so a larger model earns its cost there. Leave
  #   an editor entry empty to run everything on the worker; llm_check() says so
  #   in the setup chunk when it falls back.
  #
  #   Model ids are the providers' own strings and are not interchangeable. Check
  #   the openrouter ones against openrouter.ai/models when a render comes back
  #   404: ids there are retired on the provider's schedule.
  llm_model_worker = c(openrouter = "google/gemma-4-31b-it",
                       work       = "REPLACE-WITH-THE-SMALL-MODEL-YOUR-GATEWAY-SERVES"),
  #   The openrouter editor is empty, so that arm runs on one model until you put
  #   a larger id here; the setup chunk prints the fallback on every render.
  llm_model_editor = c(openrouter = "meta-llama/llama-4-maverick",
                       work       = "REPLACE-WITH-THE-LARGER-MODEL-YOUR-GATEWAY-SERVES"),
  llm_base_url = c(work = "https://REPLACE-WITH-YOUR-GATEWAY/v1"),

  #   Where the key lives on this machine. The key is never written here and
  #   never passed as an argument: ellmer reads it from the environment itself,
  #   and this only says under which name. The defaults are the names ellmer
  #   reads, OPENROUTER_API_KEY and OPENAI_API_KEY, so this line is needed only
  #   when one variable serves both endpoints -- uncomment it to run openrouter
  #   off OPENAI_API_KEY as well. Written in ~/.Renviron unquoted: KEY=<value>.
  # llm_key_var = c(openrouter = "OPENAI_API_KEY"),

  #   Anything the work gateway wants on every request. Both are passed straight
  #   to chat_openai_compatible() and are sent only if the installed ellmer has
  #   the argument, so an older library cannot fail on them. Leave them out
  #   unless the endpoint asks for them.
  # llm_endpoint_name = "work gateway",
  # llm_api_headers = c(`x-tenant-id` = "..."),
  # llm_api_args = list(),

  #   When the editor runs. "on_collision" uses the mechanical word-overlap gate
  #   in labels_collide(); "always" sends every drafted set once and does not
  #   rely on that gate catching a synonym it was not built to catch. Labels are
  #   frozen to a CSV after the first render, so "always" costs one extra editor
  #   call per output directory, not per render. The cutoff is this project's
  #   convention and is not drawn from anywhere.
  label_harmonise = "on_collision",
  label_collision_cutoff = 0.5,

  survey_context = paste(
    "These items come from the 2023 AmericasBarometer survey of Ecuador,",
    "conducted by the LAPOP Lab at Vanderbilt University. Fieldwork was carried",
    "out face to face in Spanish by IPSOS between February and April 2023 with",
    "1,604 respondents, drawn by a multi-stage probability design stratified by",
    "the three major regions of the country: Costa, Sierra, and Oriente.",
    "\n\nAll items analysed here are answered on the same seven-point ladder",
    "anchored at 1 (not at all) and 7 (a lot), shown to the respondent on a",
    "single card. Four ask how far the respondent supports the political system",
    "in general; the remaining seven ask how far the respondent trusts a",
    "specific national institution.")
)

# Empty demographic levels are dropped once, here, so every frame the report
#   builds has the same levels; the check is by name against the data rather
#   than against a memory of what the file contains.
cfg$data <- survey_dat_full |>
  filter(in_analysis) |>
  drop_empty_levels(cfg$aux, "estimation frame")

check_config_columns(cfg$data, cfg)

dir.create(cfg$cfa_dir, showWarnings = FALSE, recursive = TRUE)
