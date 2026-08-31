# WISE: Weighted Inference for Survey Estimation

![](images/clipboard-2943011998.png)

Latent variable measurement for complex survey data, with design-based variance carried through every stage. One stratified jackknife (JKn) replicate design generates every standard error in the analysis: the measurement model parameters, the domain estimates within demographic groups, and the corrections applied to them. Nothing is computed under an independence assumption at any point.

Two arms share a design, an engine, and a replicate variance routine:

- **Latent class analysis**, for a battery where respondents differ in *which* answers they favour. Output is a segment per respondent and a share per group.
- **Confirmatory factor analysis**, for a battery where respondents differ in *how much*. Output is a position on a continuum and a group mean.

Which one a battery calls for is a property of the data, and the workflow reports the evidence rather than choosing for you. A latent class is called a **segment** throughout; the statistical object is unchanged.

## Why this exists

Fitting a latent variable model to survey data is not the hard part. Reporting honest uncertainty is. Design-weighted point estimation is available in several packages. Standard errors that respect stratification and clustering, carried without interruption from the measurement model through to domain estimates and the corrections applied to them, are not available jointly in R.

The gap is not academic, and it is not assumed either: both reports print the ratio of the design-based standard error to the one the default machinery gives, parameter by parameter and cell by cell. Where that ratio is well above one, an analyst using the defaults would be calling differences significant that the data cannot support. Where it is near one, as it is for the factor arm's domain means on the demonstration file, the replicate design is what established that rather than something anyone was entitled to assume.

Two design choices follow from that.

**Replication rather than linearization.** A pseudo-likelihood does admit a sandwich variance, so this is a choice and not a necessity. Replication propagates to new statistics without rederivation, which is why adding the factor arm required no new variance code at all; it avoids inverting a several-hundred-dimensional information matrix with parameters at the boundary; and it makes the design specification auditable.

**The analyst decides the dimension.** Neither arm selects the number of segments or factors. Each renders the search evidence, stops, and waits for a number in the config. That decision is recorded in a file rather than buried in a default.

## The workflow at a glance

![](images/clipboard-2841528287.png)

Four stages are shared and identical code: reading and configuring, the replicate design, the variance routine, and the domain estimation that follows scoring. Four branch: the search over dimensions, the measurement model, the fit diagnostics, and scoring. The branches rejoin twice, which is why adding the factor arm to a pipeline built for the class arm needed no new variance code.

Naming happens after the diagnostics and before scoring, because a model that fails its diagnostics should be refit rather than named, and the domain tables are unreadable without names.

## Repository layout

Six files. Three are edited per dataset, one is never edited.

```         
survey_data_read.R      the file path, design columns, demographic recodes   per dataset
source_code.R           the engine: EM, WLSMV, replicate variance, prompts   never edited
survey_lca_config.R     items and settings for the class arm                 per dataset
survey_lca_report.qmd   the class analysis
survey_cfa_config.R     items and settings for the factor arm                per dataset
survey_cfa_report.qmd   the factor analysis
```

`survey_data_read.R` is shared deliberately. The demographic recodes are the one place where a mistake is silent, and two copies would give it two places to hide. The item lists live in the method configs and are allowed to differ: a battery suited to one arm is usually not suited to the other, and forcing them to match would cripple one model to flatter the other.

Each arm writes to its own output folder, so the two never overwrite each other.

## The workflow

Render with the dimension unset. Both reports run their search and stop with a note where the chosen model would be. Read the evidence, set `K_force` or `n_factors` in the config, re-render.

|   | Class arm | Factor arm |
|------------------------|------------------------|------------------------|
| Search evidence | BIC, entropy across K | eigenvalues with replicate intervals against a random-data threshold, EFA fit and admissibility |
| Diagnostics | item discrimination, bivariate residuals | loadings, modification indices |
| Scoring | posterior over answered items | Bartlett factor scores |
| Domain estimate | share of a group in each segment | mean position of each group |

Both arms report the same three estimators for every domain quantity: naive, design-based, and corrected for the attenuation the assignment step introduces. The first gap is what ignoring the design costs. The second is what the assignment costs.

The class arm scores more respondents than it estimated on: local independence lets a missing answer drop out of the product, so a partial responder still receives a posterior. The factor arm cannot. `WLSMV` estimates on item-complete cases and `lavPredict` returns a score for those cases only, so its scored frame equals its estimation frame. Item nonresponse is not random, so where it is substantial this is a real advantage of the class arm rather than a detail.

## Choosing between the arms

The question is whether people differ in *how much* or in *which*, and each arm prints the evidence for its own battery. The class report prints a level-to-pattern ratio: how far the segments differ in overall level, against how far they differ in which items they favour, with every item rescaled so that a binary and a four-category item contribute the same possible range. Well under one is a continuum a factor model would describe in far fewer parameters; near or above one is structure no single factor can hold. The factor report prints design-weighted eigenvalues with replicate intervals against a random-data threshold at the effective sample size.

The two batteries in the demonstration are chosen to make the contrast: an institutional trust battery of one format asking one kind of question about different objects, and an economic vulnerability battery of mixed formats spanning unrelated domains, where a household can own a computer and still have run short of food. Neither report chooses the arm, and neither ratio is quoted here, because both are properties of the file in front of you and both are printed on every render.

## The language model endpoint

One line in each config decides where the drafting calls go.

``` r
llm_provider     = "openrouter",                       # or "work"
llm_model_worker = c(openrouter = "...", work = "..."),
llm_model_editor = c(openrouter = "",    work = "..."),
llm_base_url     = c(work = "https://your-gateway/v1"),
label_harmonise  = "on_collision",                     # or "always"
```

`openrouter` calls `ellmer::chat_openrouter()`; `work` calls `ellmer::chat_openai_compatible()`, falling back to `ellmer::chat_openai(base_url=)` on an ellmer that predates it.

Two roles, because the two kinds of call want different things.

| Role | Calls | Sees | Model |
|------------------|------------------|------------------|------------------|
| worker | one per segment or factor | that latent variable only | small; the job is mechanical and repeated |
| editor | harmonisation, and one domain reading per demographic | every label, or every level of a demographic | larger; both need the whole set in view |

Isolation in the drafting step is deliberate -- a joint prompt lets one confusion corrupt two labels -- so the harmonisation pass exists precisely to do the thing drafting is built to prevent, and that is the call worth a larger model. An empty `llm_model_editor` entry runs both roles on the worker and the setup chunk says so on every render. Whether the editor is called at all is decided by `label_harmonise`: `"on_collision"` runs a mechanical word-overlap filter, which catches a reordered synonym and does not catch a derivational one; `"always"` skips the filter. Labels are frozen to a CSV after the first render, so `"always"` costs one extra editor call per output directory, not per render. Both freeze files record which model drafted and which, if any, edited. Keys are never written in the config or passed as arguments, so one cannot reach a saved object or a traceback. Each provider reads its own environment variable:

```         
OPENROUTER_API_KEY=<key>
OPENAI_API_KEY=<key>
```

in `~/.Renviron`, one per line, **unquoted**. A quoted value keeps its quotes on some platforms, they travel into the `Authorization` header, and the endpoint answers 401 with nothing that says why; the workflow strips a stray pair rather than letting that happen. `llm_check(cfg)` runs in the setup chunk of both reports, halts there if the key for the configured provider is missing, and prints the provider, the model and the key's length -- never the key. The label freeze files record the provider and model that drafted them, so a name can be attributed to an endpoint.

## Requirements

R 4.1 or later, for the native pipe. `pacman::p_load()` at the top of each report installs what is missing. `recode_values()` in `survey_data_read.R` is dplyr 1.2.0 and later; on an older dplyr the file defines a strict fallback with the same two behaviours it depends on and says so in a message. The fallback is this project's code, not dplyr's, and has not been diffed against it -- read the recode audit.

## Reproducibility

Starting values are drawn in the main session from a deterministic seed sequence and passed to the workers as data, so no worker touches the random number generator and results are identical under sequential and parallel plans and under any number of workers. The one loop that must draw inside the workers, the parallel analysis in the factor arm, takes L'Ecuyer streams from the same seed instead, which reproduces under any plan for the same reason.

For the names the language model drafts, the mechanism is a freeze file rather than a seed. When the label file exists it is used and validated; otherwise the model drafts once and writes it. Editing that file is how the analyst takes over naming; deleting it triggers a redraft. A seed would be reproducible only for a fixed model, endpoint and package version.

## Language model steps

Two, both bounded, both after every number is settled.

**Naming.** The model receives the response probabilities or loadings and the item wording, one call per segment or factor, and returns a draft name. It never sees a covariance matrix, an estimator, or a fit statistic. It is forbidden to add probabilities across response categories, because that arithmetic is unverifiable and the model treats the categories as unordered anyway.

**Reading the domain tables.** R computes which pairs of levels have intervals that do not overlap, and how far the design moves each estimate; the model receives that list and translates it. It never sees a standard error and never decides whether a difference is real. Its output is capped at four findings, because the point is to say where to look rather than to narrate every cell.

Names and readings never enter a computation. A wrong one is a presentation error, not a statistical one, and every table is verifiable against the estimates above it.

## Statistical decisions

- **Variance is the point.** The same replicates drive every standard error in both arms.
- **Design effects are a specification check.** A covariate constant within a cluster must have a design effect equal to the mean cluster size. If it does, the cluster identifier and the variance formula are jointly correct. A design effect below one indicates composition controlled at the final selection stage, and that precision is a fieldwork artifact.
- **Singleton strata are a hard stop.** `survey.lonely.psu` governs linearization, not replicate construction, so a singleton would silently contribute zero variance. The check runs on the analysis frame, since case exclusions can create singletons the released file does not have.
- Weights enter every estimation step, so estimates describe the population.
- Information criteria rescale the log-likelihood to the sum-to-n scale; without it BIC leans toward too many segments.
- The bootstrap likelihood ratio test is omitted: its resampling presumes independent observations.
- Bivariate residuals are total variation distance between the observed and model-implied two-way table. Bounded in [0, 1], zero under exact local independence, and no division by a possibly empty expected cell. No reference distribution is valid here, so this ranks rather than tests.
- Modification indices and factor-count fit indices are rankings, not tests. Both ignore clustering and are inflated by roughly the design effect.
- The structure search and the confirmatory fit use the same data, and the factor report says so rather than pretending otherwise. The guard against reading noise as structure is the replicate interval on each eigenvalue, which costs no cases; it is weaker than an independent sample and is not offered as a substitute for one. Refitting the loading pattern inside every replicate would answer the stability question more directly and is not implemented.
- Attenuation is corrected and reported, not assumed away. Modal assignment and shrunken factor scores both understate group differences, so a difference that survives is real while a null is not evidence of absence.
- The delivered file carries posteriors and correction weights, not only the assignment. Cross-tabulating the assignment alone reintroduces exactly the attenuation the correction removes, and the variable label says so.

## What is checked automatically

These halt the render rather than produce a wrong number: a response label unmatched by an explicit recode rule; a configured column absent from the data, or present and entirely missing; a stratum containing a single primary sampling unit in the analysis frame; a mismatch between the number of estimated domain quantities and their metadata; a label file whose row count or factor names do not match the chosen dimension; a factor specification naming an item that was dropped; a language model endpoint whose key is not in the environment, which fails in the setup chunk rather than after the search; and, in the factor arm, a scored frame whose row count does not match the estimation frame.

These are repaired rather than halted, with a message naming what changed: a demographic level left with no respondents in the analysis or scored frame is dropped, so `svyby` and the tabulations cannot silently disagree about how many levels there are; and an API key written into `.Renviron` with quotes around it has them stripped before the request is signed.

## What the analyst must check

The recode audit, once per dataset. The weight coefficient of variation and unequal weighting effect, which say whether weighting is doing anything in this file. The search evidence, to choose the dimension. Assignment quality by number of items answered, to set the information floor. The drafted names against the profiles or loadings behind them. And the shift between estimators relative to its standard error, to decide which to report.

## Related work

`baysc` (Wu, Williams, Savitsky and Stephenson 2024, *Biometrics* 80(4) ujae122) fits a weighted objective in a Bayesian pseudo-posterior with a post-hoc variance adjustment. It selects the number of classes through an overfitted mixture, obtains uncertainty from an adjusted posterior rather than replicate weights, and needs no attenuation correction because Bayesian estimation propagates classification uncertainty through the posterior draws. It is distributed through GitHub and requires a compiler toolchain, which places it outside what can be installed in some restricted analytic environments. When it reaches CRAN it becomes the natural Bayesian comparator for this work.

Mplus (`TYPE = MIXTURE COMPLEX`) and Stata (`gsem` under `svy`) maximize the same objective with the same weight convention, so point estimates and information criteria should match up to optimizer tolerance and mode selection. Their standard errors are linearization-based where these are replication-based; both are design-consistent and neither corrects the other. The unweighted special case is validated at runtime against `poLCA`.

## Data acknowledgment

The demonstration uses the 2023 AmericasBarometer for Ecuador by the LAPOP Lab at Vanderbilt University. Obtain the data from LAPOP under their terms; nothing here redistributes it. Note that the released weights for this file are nearly constant, so the demonstration exercises the variance machinery rather than the weighting machinery. Both are exercised on production data with informative weights.
