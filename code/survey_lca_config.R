# survey_lca_config.R
# The latent class arm: which items, which settings. 
# Sources survey_data_read.R for the design and demographics

# Sourced after source_code.R by survey_lca_report.qmd, which is where the
#   shared helpers this file calls at the bottom come from.

if (!exists("check_config_columns"))
  stop("source('source_code.R') before survey_lca_config.R.", call. = FALSE)

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
  
  K_range = 2:12,
  K_force = 4,
  n_starts = 200,
  
  seed = 2026,
  parallel = TRUE,
  workers = NULL,
  
  lca_dir = here::here( "output", "lca"),
  
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

# Empty demographic levels are dropped once, here, so every frame the report
#   builds has the same levels; the check is by name against the data rather
#   than against a memory of what the file contains.
cfg$data <- survey_dat_full |>
  filter(in_analysis) |>
  drop_empty_levels(cfg$aux, "estimation frame")

check_config_columns(cfg$data, cfg)

dir.create(cfg$lca_dir, showWarnings = FALSE, recursive = TRUE)