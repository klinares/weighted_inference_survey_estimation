# survey_data_read.R script for WISE project

# Reads the spss data file, cleans everything that does not depend on 
#   which model is being fitted, Produces: 
#     - raw_survey_dat, design_dat, demo_dat, demos, demo_levels,
#        recode_audit, na_codes, & codes_to_drop

# BEFORE THE 1st RENDER on a new country file, integrate the observed labels 
#   and write the recode arms in section 3 from them. 
# Label text differs between country questionnaires, 
#   so arms carried over will fail; unmatched = "error" names variable that failS

# 1. Settings 
#include recoding variable names, 
#  indicators are set in the config script since they differ by method
#_______________________________________________________
demo_codes <- c(age_cat = "q2",
                sex = "q1tc_r",
                education = "edre",
                urban = "ur",
                employment = "ocup4a")

demos <- names(demo_codes)

na_codes <- c(888888, 988888, 999999)

# TRUE keeps "don't know" as a substantive category on the items. The item
#   recode in each method config sorts values, so the large DK code lands in
# category C+1 with no extra arithmetic

dk_as_category <- FALSE
codes_to_drop <- if (dk_as_category) setdiff(na_codes, 888888) else na_codes

# 2. Read spss file
# user_na = TRUE keeps the nonresponse codes as values instead of letting haven
#   convert them to NA on read, 
#_______________________________________________________

raw_survey_dat <- haven::read_sav(
  here::here("data", 
             "ECU_2023_LAPOP_AmericasBarometer_v1.0_w.sav"),
  user_na = TRUE) |>
  janitor::clean_names()

design_dat <- raw_survey_dat |>
  transmute(id = as.numeric(unclass(idnum)),
            strata = as.numeric(unclass(strata)),
            psu = as.numeric(unclass(upm)),
            wt = as.numeric(unclass(wt)))

# 3. Demographics 
# One recode_values per variable, every observed label named
# zap_missing() turns the nonresponse codes into NA first
#_______________________________________________________

demo_dat <- raw_survey_dat |>
  select(all_of(demo_codes)) |>
  haven::zap_missing() |>
  transmute(
    # recode demographics here as needed
    
    age_cat = cut(as.numeric(age_cat), breaks = c(15, 29, 44, 59, Inf),
                  labels = c("16-29", "30-44", "45-59", "60+")) |>
      as.character(),

    sex = as.character(haven::as_factor(sex)) |>
      recode_values("Hombre/masculino" ~ "Male",
                    "Mujer/femenino" ~ "Female",
                    "No se identifica como hombre ni como mujer" ~ NA_character_,
                    NA ~ NA_character_,
                    unmatched = "error"),

    education = as.character(haven::as_factor(education)) |>
      recode_values(
        "Ninguna" ~ NA_character_,
        c("Primaria/b\u00e1sica incompleta",
          "Primaria/b\u00e1sica completa") ~ "Primary",
        c("Secundaria/bachillerato incompleto",
          "Secundaria/bachillerato completo") ~ "Secondary",
        c("Terciaria o universitaria o superior incompleta",
          "Terciaria o universitaria o superior completa") ~ "Tertiary",
        NA ~ NA_character_,
        unmatched = "error"),

    urban = as.character(haven::as_factor(urban)) |>
      recode_values("Urbano" ~ "Urban",
                    "Rural" ~ "Rural",
                    NA ~ NA_character_,
                    unmatched = "error"),

    employment = as.character(haven::as_factor(employment)) |>
      recode_values(
        c("Trabajando?",
          "No est\u00e1 trabajando en este momento pero tiene trabajo?") ~ "Employed",
        "Est\u00e1 buscando trabajo activamente?" ~ "Unemployed",
        "No trabaja y no est\u00e1 buscando trabajo?" ~ "Not in labor force",
        "Es estudiante?" ~ "Student",
        "Se dedica a los quehaceres de su hogar?" ~ "Homemaker",
        "Est\u00e1 jubilado, pensionado o incapacitado permanentemente para trabajar?" ~ "Retired",
        NA ~ NA_character_,
        unmatched = "error"))

# First level of each is the contrast reference in the reports.
demo_levels <- list(
  age_cat = c("16-29", "30-44", "45-59", "60+"),
  sex = c("Male", "Female"),
  education = c("Secondary", "Primary", "Tertiary"),
  urban = c("Urban", "Rural"),
  employment = c("Employed", "Unemployed", "Not in labor force", "Student",
                 "Homemaker", "Retired"))

demo_dat <- demo_dat |>
  mutate(across(all_of(demos),
                function(x) factor(x, levels = demo_levels[[cur_column()]])))


# 4. Recode audit
# What each source label became. Read once per dataset 
#_______________________________________________________

recode_audit <- imap(demo_codes, function(src, tgt) {
  tibble(variable = tgt,
         source_label = as.character(haven::as_factor(raw_survey_dat[[src]])),
         recoded = as.character(demo_dat[[tgt]]))
}) |>
  list_rbind() |>
  count(variable, source_label, recoded, name = "n") |>
  arrange(variable, desc(n))
