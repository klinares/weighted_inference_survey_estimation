# source_code.R for WISE repo
# Engine for both arms of the workflow. Holds no analysis-specific state:
#   everything arrives as an argument. Sourced before the method config.

#   1. Plotting and tables            SHARED
#   2. Weighted EM and diagnostics    LCA
#   3. Design and replicate variance  SHARED
#   4. Prediction                     LCA
#   5. LLM labeling                   SHARED
#   6. Weighted EFA and CFA           CFA

# Sections 1, 3 and 5 serve both arms. Nothing in 2 or 4 is called by the CFA
#   report, and nothing in 6 is called by the LCA report, so the two can be
#   edited independently.

# Data cleaning lives in survey_data_read.R and the two method configs.

`%||%` <- function(x, y) if (is.null(x)) y else x


# Section 1 holds the look of the output: theme, tables, line wrapping, 
#   and the raw-data plot. Used by both reports.
#______________________________________________________________________________

theme_lca <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(face = "bold", size = rel(0.85)),
          plot.title = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = rel(0.85)),
          plot.caption = element_text(hjust = 0, size = rel(0.78), 
                                      color = "grey30"))
}

# Wrap long lines before printing. 
# Verbatim output does not wrap on its own and the overflow is clipped; 
# fixing that in the preamble would be LaTeX-only, so it is done here 
#   instead and holds for LaTeX, Typst, HTML, and docx alike. 
# Existing newlines and leading indentation are preserved, so structured 
#   text keeps its shape and only overlong lines are broken.

wrap_text <- function(x, width = 88L) {
  strsplit(paste(x, collapse = "\n"), "\n", fixed = TRUE)[[1]] |>
    map_chr(function(line) {
      if (nchar(line) <= width) return(line)
      pad = str_extract(line, "^[ ]*")
      strwrap(str_squish(line), width = width,
              prefix = paste0(pad, "  "), initial = pad) |>
        paste(collapse = "\n")
    }) |>
    paste(collapse = "\n")
}

# Every table goes through here, so pagination and styling are set in one place
# With other format, including Typst, it emits a markdown pipe table, which
# Quarto renders natively and paginates on its own. 
# widths is ignored in that path because column sizing is the renderer's 
#   job there, so it is a LaTeX hint rather than a requirement and 
#   nothing breaks if it is absent.

fit_widths <- function(n_col, first = 2, total = 44) {
  share = total / (n_col - 1 + first)
  paste0(round(c(first * share, rep(share, n_col - 1)), 1), "em")
}

lca_table <- function(df, ..., caption = NULL, widths = NULL, font_size = 8) {
  if (isTRUE(knitr::pandoc_to("typst")))
    return(lca_table_typst(df, ..., caption = caption, widths = widths,
                           font_size = font_size))
  if (!is_latex_output())
    return(knitr::kable(df, format = "pipe", caption = caption, ...))
  if (!is.null(caption))
    caption = str_replace_all(caption, "([#$%&_{}])", "\\\\\\1")
  out = kable(df, format = "latex", longtable = TRUE, booktabs = TRUE,
                      linesep = "", caption = caption, ...) |>
    kable_styling(latex_options = c("repeat_header", "hold_position"),
                              font_size = font_size)
  if (is.null(widths)) return(out)
  reduce(seq_along(widths), function(tbl, i) {
    if (nzchar(widths[i])) kableExtra::column_spec(tbl, i, width = widths[i]) else tbl
  }, .init = out)
}

# Typst renders tables from this branch. A plain pipe table handed to pandoc
#   arrives with auto-sized columns at body text size, which is what ran off the
#   page: the widths and font size the LaTeX branch applies never reach it, and
#   pandoc's Typst writer does not reliably turn pipe-table dash counts into
#   column widths, so encoding the widths in the markdown would be a fix that
#   depends on which pandoc Quarto bundles. The table is written as Typst.
# Widths are the same em values the LaTeX branch takes, plus 8pt. A LaTeX p{w}
#   column excludes its padding and a Typst column width includes it, so the
#   same number left each cell 8pt less room and single words like "Homemaker"
#   collided with the next column.
# Every cell and caption goes in as a quoted string, never as markup, so an
#   asterisk in a level name or a dagger in a cell is printed rather than
#   interpreted. jsonlite does the quoting: a whitespace-squished JSON string
#   uses only the \" and \\ escapes, which Typst reads identically; the gsub
#   covers \uXXXX, which Typst spells \u{XXXX}, in case jsonlite ever emits one.
# The figure is breakable so a long table continues onto the next page, and
#   table.header() repeats at the top of each page -- what longtable and
#   repeat_header did under LaTeX.
lca_table_typst <- function(df, ..., caption = NULL, widths = NULL,
                            font_size = 8) {
  dots = list(...)
  n = ncol(df)
  
  typ_str = function(x) {
    x = enc2utf8(as.character(x))
    x[is.na(x)] = "NA"
    s = sub("^\\[(.*)\\]$", "\\1", as.character(jsonlite::toJSON(str_squish(x))))
    gsub("(?<!\\\\)\\\\u([0-9a-fA-F]{4})", "\\\\u{\\1}", s, perl = TRUE)
  }
  
  hdr = if (is.null(dots$col.names)) names(df) else dots$col.names
  
  al = dots$align
  if (is.null(al)) al = if_else(map_lgl(df, is.numeric), "r", "l")
  if (length(al) == 1L) al = strsplit(al, "")[[1]]
  al = coalesce(unname(c(l = "left", r = "right", c = "center")[rep_len(al, n)]),
                "left")
  
  w = c(widths, rep("", n))[seq_len(n)]
  cols = if_else(str_detect(w, "^[0-9.]+em$"), paste0(w, " + 8pt"), "auto")
  
  m = df |>
    mutate(across(where(is.numeric), \(x) format(x, trim = TRUE)),
           across(everything(), as.character)) |>
    as.matrix()
  rows = map_chr(seq_len(nrow(m)), \(i) paste0("        ", typ_str(m[i, ]), ","))
  
  cap = if (is.null(caption)) character(0) else
    paste0("    caption: figure.caption(position: top, ", typ_str(caption), "),")
  
  knitr::asis_output(paste(c(
    "", "```{=typst}",
    "#{",
    "  show figure: set block(breakable: true)",
    "  figure(",
    "    {",
    paste0("      set text(size: ", font_size, "pt)"),
    "      table(",
    paste0("        columns: (", paste(cols, collapse = ", "), ",),"),
    paste0("        align: (", paste(al, collapse = ", "), ",),"),
    "        inset: (x: 4pt, y: 3pt),",
    "        stroke: none,",
    "        table.hline(),",
    paste0("        table.header(", typ_str(hdr), "),"),
    "        table.hline(stroke: 0.5pt),",
    rows,
    "        table.hline(),",
    "      )",
    "    },",
    cap,
    "    kind: table,",
    "  )",
    "}",
    "```", ""), collapse = "\n"))
}


init_parallel <- function(cfg) {
  if (isTRUE(cfg$parallel)) {
    future::plan(future::multisession,
                 workers = cfg$workers %||% max(1L, future::availableCores() - 1L))
  } else {
    future::plan(future::sequential)
  }
  invisible(NULL)
}

plot_item_stack <- function(df, items, title, show_missing = TRUE) {
  long = df |>
    select(all_of(items)) |>
    mutate(across(everything(), as.numeric)) |>
    pivot_longer(everything(), names_to = "item", values_to = "value")
  if (!show_missing) long = filter(long, !is.na(value))
  lev = as.character(sort(unique(long$value[!is.na(long$value)])))
  long |>
    mutate(value = factor(if_else(is.na(value), "Missing", as.character(value)),
                          levels = c(lev, if (show_missing) "Missing"))) |>
    count(item, value) |>
    ggplot(aes(item, n, fill = value)) +
    geom_col(position = "fill") +
    scale_fill_manual(name = "Response",
                      values = c(set_names(viridis(length(lev)), lev),
                                 Missing = "grey75")) +
    labs(x = NULL, y = "Proportion", title = title) +
    theme_lca() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# Section 2 is the LCA measurement model: 
# weighted EM, label alignment across fits, and the two fit diagnostics. 
# LCA only
#______________________________________________________________________________

rand_init <- function(cats, K) { # pass K size and categories
  list(pi = {x = runif(K); x / sum(x)},
       rho = map(cats, function(Cj) {
         m = matrix(runif(Cj * K) + 0.1, Cj, K)
         sweep(m, 2, colSums(m), "/")
       }))
}

# One EM run as a fold. Y is a list of integer item vectors, 
# OH a list of one-hot category matrices. 
# A missing answer contributes 0 on the log scale, so it drops
#   out of the within-segment product

#__________ AI Assistance w/ this section, careful to not modify _______
em_run <- function(Y, OH, cats, w, K, init = NULL, maxit = 800L, tol = 1e-8) {
  nn = length(Y[[1]])
  st0 = c(init %||% rand_init(cats, K),
           list(post = NULL, ll = -Inf, iter = 0L, done = FALSE))

  step = function(state, .iter) {
    if (isTRUE(state$done)) return(state)
    log_terms = map2(state$rho, Y, function(rho_j, y) {
      lp = log(rho_j)[y, , drop = FALSE]
      lp[is.na(lp)] = 0
      lp
    })
    
    logdens = reduce(log_terms, `+`) + matrix(log(state$pi), nn, K, byrow = TRUE)
    lse = matrixStats::rowLogSumExps(logdens)
    post = exp(logdens - lse)
    ll = sum(w * lse)
    wp = w * post
    den = colSums(wp)
    rho_n = map(OH, function(oh) {
      num = pmax(crossprod(oh, wp), 1e-12)
      sweep(num, 2, colSums(num), "/")
    })
    list(pi = den / sum(den), rho = rho_n, post = post, ll = ll,
         iter = state$iter + 1L,
         done = abs(ll - state$ll) < tol * (abs(state$ll) + 1))
  }

  out = reduce(seq_len(maxit), step, .init = st0)
  out$converged = out$done

  # step() computes post and ll from the parameters it was HANDED and returns
  #   them beside the parameters it produced, so out$post is one M-step behind
  #   out$pi and out$rho. At convergence the gap is below tol and nothing
  #   notices; on a run that stopped at maxit it is a posterior that is not the
  #   E-step of the model being reported, and entropy is read off it. One more
  #   E-step settles it.
  # out$ll is deliberately left where it was: it is what the enumeration table
  #   and BIC are formed from, and moving it by an epsilon would change which
  #   model the criteria rank first for no methodological gain. The lag it
  #   carries is smaller than the convergence tolerance wherever it is used.
  out$post = posterior_of(out$pi, out$rho, Y)
  out
}

make_inputs <- function(df, items, cats) {
  Y = map(items, function(it) as.integer(df[[it]]))
  OH = map2(items, cats, function(it, Cj) {
    oh = outer(as.integer(df[[it]]), seq_len(Cj), `==`) + 0
    oh[is.na(oh)] = 0
    oh
  })
  list(Y = Y, OH = OH)
}

# E-step under fixed parameters.
posterior_of <- function(pi, rho, Y) {
  nn = length(Y[[1]])
  K = length(pi)
  log_terms = map2(rho, Y, function(rho_j, y) {
    lp = log(rho_j)[y, , drop = FALSE]
    lp[is.na(lp)] = 0
    lp
  })
  logdens = reduce(log_terms, `+`) + matrix(log(pi), nn, K, byrow = TRUE)
  exp(logdens - matrixStats::rowLogSumExps(logdens))
}

# Segment labels are arbitrary. 
# Match any fit to a reference by response profile so segments are 
#   comparable across starts, fits, and replicates
profiles_of <- function(rho) do.call(cbind, map(rho, t))

align_to <- function(fit, ref) {
  K = length(fit$pi)
  Pf = profiles_of(fit$rho)
  Pr = profiles_of(ref$rho)
  cost = outer(seq_len(K), seq_len(K),
                Vectorize(function(a, b) sum((Pf[a, ] - Pr[b, ])^2)))
  inv = integer(K)
  inv[as.integer(clue::solve_LSAP(cost))] = seq_len(K)
  list(pi = fit$pi[inv],
       rho = map(fit$rho, function(m) m[, inv, drop = FALSE]),
       post = if (!is.null(fit$post)) fit$post[, inv, drop = FALSE] else NULL,
       ll = fit$ll,
       converged = fit$converged %||% NA)
}

# Seeds are passed as data rather than drawn inside the worker, 
#   so sequential and parallel plans return identical results
start_seeds <- function(cfg, K) as.integer(cfg$seed + 1000L * K + seq_len(cfg$n_starts))

fit_lca <- function(df, w, cats, items, K, seeds, ref = NULL,
                    maxit = 800L, tol = 1e-8) {
  inp = make_inputs(df, items, cats)
  cands = map(seeds, function(s) {
    set.seed(s)
    em_run(inp$Y, inp$OH, cats, w, K, maxit = maxit, tol = tol)
  })
  best = cands[[which.max(map_dbl(cands, "ll"))]]
  if (is.null(ref)) best else align_to(best, ref)
}

df_k <- function(K, cats) (K - 1) + K * sum(cats - 1)

# Relative entropy on the weighted scale, so it describes the population model
#   rather than the achieved sample.
entropy_R2 <- function(post, w, K) {
  if (K == 1) return(NA_real_)
  1 + sum(w * rowSums(post * log(pmax(post, 1e-12)))) / (sum(w) * log(K))
}

# Two views of how much an item separates the segments, because they answer
#   different questions and a battery of mixed formats needs both.

# discrimination is the mean over segment pairs of the total variation 
#   distance between their response distributions. 
# It is bounded in [0, 1] for any number of categories and it is sensitive to 
#   shape: a segment that avoids the middle of a scale registers here even 
# if its mean sits where everyone else's does. 
# That suits a model which treats categories as unordered, which is what 
#   this one does.

# Range is how far the expected response travels across segments, as 
#   a share of the scale. 
# It assumes the categories are ordered, which the model does not, but
#   it is the quantity an analyst reads off a profile plot and it puts a 
#   binary item and a seven-point item on the same footing.

# An item can score high on one and low on the other. Where they disagree the
#   item is worth looking at rather than dropping.
item_discrimination <- function(fit, items) {
  pairs = combn(length(fit$pi), 2, simplify = FALSE)
  tibble(item = items,
         discrimination = map_dbl(fit$rho, function(rho_j) {
           mean(map_dbl(pairs,
                        function(p) 0.5 * sum(abs(rho_j[, p[1]] - rho_j[, p[2]]))))
         }),
         range = map_dbl(fit$rho, function(rho_j) {
           ev = as.numeric(seq_len(nrow(rho_j)) %*% rho_j)
           (max(ev) - min(ev)) / (nrow(rho_j) - 1)
         })) |>
    arrange(desc(discrimination))
}

# How much of the difference between segments is level and how much is pattern.
# Each item's expected response is scaled to run from 0 to 1 so that a binary
#   item and a four-category item contribute the same amount of possible spread;
#   without that the ratio is an artifact of the response formats. 
# A value well under 1 says the segments differ mainly in how high they 
#   answer overall, which is a continuum a factor model would describe 
#   with far fewer parameters. 
# Near or above 1 says they reorder the items, which is structure no single 
#   factor can hold.
# The pattern term is the segment-by-item INTERACTION,
#   e_kj = m_kj - level_k - item_j + grand, not the deviation m_kj - level_k of
#   an item from its own segment's level. Those are not the same quantity. A
#   deviation from the segment level still carries the item main effect -- the
#   fact that some questions are endorsed more than others -- which is a
#   property of the battery and says nothing about whether the segments reorder
#   anything. Left in, the statistic is strictly positive under the only null
#   it claims to read on: when m_kj = level_k + item_j every segment answers in
#   the same order, the interaction is zero and the ratio should be zero, but
#   the deviation form returns sd(item_j) / sd(level_k), which is unbounded. It
#   also moved when a constant was added to one item across every segment,
#   which must not change the answer.
# Both dispersions and both marginals are weighted by segment prevalence.
#   Unweighted, sd_level is the spread of K numbers in which a segment holding
#   four per cent of the population counts as much as one holding forty, so the
#   ratio tracked the size of the smallest segment rather than the shape of the
#   battery. Every marginal in the decomposition has to be taken over the same
#   population or the residual is not an interaction with respect to anything.
# On the Ecuador fit the old form gives 3.78, the old form weighted gives 5.9,
#   and this one gives 4.3. The reading does not change; the number does.
# The ratio is descriptive and comparable within a battery, not across
#   batteries: the interaction residuals sum to zero over segments and over
#   items, so their spread depends on both counts. Nothing selects on it and no
#   threshold is attached to it.
level_pattern_ratio <- function(fit, items) {
  K = length(fit$pi)
  pi_k = fit$pi
  d = map(seq_len(K), function(k)
    tibble(segment = k, share = pi_k[k], item = items,
           m = map_dbl(fit$rho, function(r)
             (sum(seq_len(nrow(r)) * r[, k]) - 1) / (nrow(r) - 1)))) |>
    list_rbind() |>
    group_by(segment) |>
    mutate(level = mean(m)) |>
    ungroup() |>
    group_by(item) |>
    mutate(item_mean = sum(share * m) / sum(share)) |>
    ungroup() |>
    mutate(grand = sum(share * level) / sum(share),
           pattern = m - level - item_mean + grand)

  wsd = function(x, wt) {
    wt = wt / sum(wt)
    sqrt(sum(wt * (x - sum(wt * x))^2))
  }

  lev = distinct(d, segment, share, level)
  tibble(sd_level = wsd(lev$level, lev$share),
         sd_pattern = wsd(d$pattern, d$share)) |>
    mutate(ratio = sd_pattern / sd_level)
}

# Bivariate residual: total variation distance between the weighted observed
#   two-way table and the model-implied one. Bounded in [0, 1], zero under 
#  exact local independence. No reference distribution applies under a 
#  design-weighted pseudo-likelihood, so this ranks rather than tests.
bvr_pairs <- function(df, w, items, fit) {
  pr = t(combn(seq_along(items), 2L))
  map(seq_len(nrow(pr)), function(i) {
    a = pr[i, 1]
    b = pr[i, 2]
    # xtabs drops a row with a missing answer on either item, so the divisor 
    # has to be the weight of the pairwise-complete rows and not the whole 
    # sample.
    # With item-complete estimation the two coincide; with partial responders
    # they do not, and using sum(w) would leave the observed table short of 
    # one and inflate every residual.
    ok = !is.na(df[[items[a]]]) & !is.na(df[[items[b]]])
    obs = as.matrix(xtabs(w ~ factor(df[[items[a]]], seq_len(nrow(fit$rho[[a]]))) +
                             factor(df[[items[b]]], seq_len(nrow(fit$rho[[b]]))))) / sum(w[ok])
    exp_p = fit$rho[[a]] %*% (fit$pi * t(fit$rho[[b]]))
    tibble(item_a = items[a], item_b = items[b],
           bvr = 0.5 * sum(abs(obs - exp_p)))
  }) |>
    list_rbind() |>
    arrange(desc(bvr))
}


# Section 3 builds the replicate design and computes variance from it. 
# Both arms use this, which is the point: one design, every standard error.
#______________________________________________________________________________

# The configuration names the columns everything downstream reads by position in
#   a formula, and a name that is not there fails late and unhelpfully: svydesign
#   reports a missing object, or a demographic arrives as an all-NA column and
#   every domain estimate built on it is empty. Checked once, by name, before
#   any of that.
check_config_columns <- function(dat, cfg) {
  need = c(cfg$strata, cfg$psu, cfg$weight, cfg$id, cfg$items, cfg$aux)
  gone = setdiff(need, names(dat))
  if(length(gone))
    stop("The configuration names columns the data does not have: ",
         paste(gone, collapse = ", "),
         ". Check item_codes and demo_codes against the source file.",
         call. = FALSE)
  empty = keep(c(cfg$items, cfg$aux), function(v) all(is.na(dat[[v]])))
  if(length(empty))
    stop("These configured columns are entirely missing values: ",
         paste(empty, collapse = ", "),
         ". A recode arm or a nonresponse code has emptied them.", call. = FALSE)
  invisible(TRUE)
}

# A demographic level with no rows left in the analysis frame is a live hazard
#   rather than a cosmetic one. svyby returns no row for it while
#   count(.drop = FALSE) returns one, so the estimator frames stop lining up and
#   a positional comparison between them silently pairs the wrong cells; and the
#   domain theta function divides by a zero weight total, which puts NaN into the
#   replicate covariance and drops the level out of the Wald tests without
#   saying so. Dropping empty levels once, and naming them, keeps every frame
#   the same shape.
drop_empty_levels <- function(dat, aux, label = "analysis frame") {
  gone = map(set_names(aux), function(v) {
    if(!is.factor(dat[[v]])) return(character(0))
    setdiff(levels(dat[[v]]), levels(droplevels(dat[[v]])))
  }) |>
    keep(function(x) length(x) > 0)
  if(length(gone))
    message("Dropped empty levels from the ", label, ": ",
            paste(imap_chr(gone, function(lv, v)
              paste0(v, " (", paste(lv, collapse = ", "), ")")), collapse = "; "))
  mutate(dat, across(all_of(aux),
                     function(x) if(is.factor(x)) droplevels(x) else x))
}

# Stratified jackknife design. 
# Singleton strata are a hard stop: they cannot take the n_h / (n_h - 1) 
#   replicate scaling, and survey.lonely.psu governs linearization rather 
#   than replicate construction, so continuing would  understate variance 
#   in the strata with least information.
build_rep_design <- function(dat, cfg) {
  lonely = dat |>
    distinct(.data[[cfg$strata]], .data[[cfg$psu]]) |>
    count(.data[[cfg$strata]], name = "n_psu") |>
    filter(n_psu < 2)

  if (nrow(lonely) > 0) {
    print(lonely)
    stop(nrow(lonely), " stratum/strata contain a single PSU in the analysis ",
         "frame. Collapse them in the method config before continuing.")
  }

  des = svydesign(ids = reformulate(cfg$psu), strata = reformulate(cfg$strata),
                   weights = reformulate(cfg$weight), data = dat, nest = TRUE)

  # mse = TRUE, explicitly. survey's default is
  #   getOption("survey.replicates.mse"), which is FALSE, and svrVar() then
  #   centres the replicate spread on the MEAN OF THE REPLICATES.
  #   replicate_variance() below centres on the full-sample estimate, which is
  #   the JKn formula the document states. Left at the default, the
  #   design-based rows of the domain tables (svyby) and the corrected rows
  #   (replicate_variance) come from two different variance estimators, and the
  #   document compares their widths as though they were one. This changes
  #   every standard error svyby produces.
  list(des = des, rep_des = as.svrepdesign(des, type = "JKn", mse = TRUE))
}

# Same estimator survey::withReplicates uses,
#    V = scale * sum_r rscale_r (theta_r - theta_hat)(theta_r - theta_hat)',
#    but the expensive part (one refit per replicate) is mapped, not looped.
# keep is a logical index into the rows of the FULL design. Restricting the
#   replicate weights to those rows, rather than rebuilding the design on those
#   rows, is the unconditional subpopulation approach: the replicate structure,
#   the a_h / (a_h - 1) scaling and the degrees of freedom all stay those of the
#   sample that was drawn. Rebuilding is the conditional approach, which drops
#   any PSU that contributed no surviving respondent and changes all three.
#   SURV701 states the rule -- subset the design, not the data.
# Optional, so existing call sites are unaffected. Moving those call sites onto
#   it is the remaining half of the change: build_rep_design() on the whole
#   frame once, then pass keep here instead of a rebuilt design.
replicate_variance <- function(rep_des, theta_fun, theta_hat, keep = NULL) {
  Wm = weights(rep_des, type = "analysis")

  if (!is.null(keep)) {
    if (length(keep) != nrow(Wm))
      stop("The keep index has ", length(keep), " entries but the replicate ",
           "weights have ", nrow(Wm), " rows. It must index the full design ",
           "the replicate set was built on, not an already-subset frame.",
           call. = FALSE)
    Wm = Wm[keep, , drop = FALSE]
  }

  Theta = do.call(rbind, future_map(seq_len(ncol(Wm)),
                                     function(r) theta_fun(Wm[, r]),
                                     .options = furrr_options(seed = NULL)))
  d = sweep(Theta, 2, theta_hat, "-")
  rep_des$scale * crossprod(d * sqrt(rep_des$rscales))
}

# Modal assignment is an error-prone measurement of true segment, and cross
#    tabbing it against anything pulls the association toward the marginal. 
# D holds the design-weighted classification error rates, 
#   P(assigned s | truly k), and each respondent's hard assignment is 
#   replaced by row W of its inverse. 
# Entries can come out negative, which is a property of the correction rather 
# than a fault, and rows still sum to one because D's rows do.
# How well conditioned D has to be before its inverse is worth anything. An
#   exactly singular table stops the run because solve() refuses it; a
#   near-singular one -- a segment that took very few assignments -- does not,
#   and the correction it produces is unstable rather than wrong-looking. The
#   reciprocal condition number is attached to every call so a caller can count
#   the replicates that fell through this floor and disclose them, which is the
#   only honest treatment available: there is nothing to repair.
BCH_RCOND_MIN <- 1e-8

bch_weights <- function(post, modal, w) {
  K = ncol(post)
  num = crossprod(w * post, outer(modal, seq_len(K), `==`) + 0)
  D = sweep(num, 1, rowSums(num), "/")
  Dinv = try(solve(D), silent = TRUE)
  if(inherits(Dinv, "try-error"))
    stop("The classification table D is singular, so the BCH correction has no ",
         "inverse to apply. This happens when a segment takes no modal ",
         "assignments in a replicate, which is a sign the segment is too small ",
         "to survive deleting one PSU.", call. = FALSE)
  rc = rcond(D)
  if (rc < BCH_RCOND_MIN)
    warning("The classification table is nearly singular (reciprocal condition ",
            "number ", signif(rc, 3), "). The correction it produces is ",
            "unstable. Count these and disclose them rather than reading the ",
            "corrected column as though they had not happened.", call. = FALSE)
  structure(Dinv[modal, , drop = FALSE], rcond = rc)
}


# Section 4 scores respondents from a fitted LCA, including those who skipped
#   items. 
# LCA only; the CFA scores with lavPredict.
#______________________________________________________________________________

# Posterior segment membership for any respondents carrying the item columns.
# Items arrive already recoded by the method config, so the fitted and the
#   predicted frames are on the same coding by construction.
predict_segments <- function(df, fit, items, min_items) {
  K = length(fit$pi)
  Y = map(items, function(it) as.integer(df[[it]]))
  post = posterior_of(fit$pi, fit$rho, Y)
  answered = reduce(Y, function(a, y) a + as.integer(!is.na(y)),
                     .init = integer(nrow(df)))

  seg = max.col(post, ties.method = "first")
  seg[answered < min_items] = NA_integer_

  colnames(post) = paste0("post_segment", seq_len(K))
  bind_cols(
    tibble(segment = seg,
           max_posterior = if_else(is.na(seg), NA_real_, matrixStats::rowMaxs(post)),
           n_items_answered = answered),
    as_tibble(post))
}


# Section 5 drafts names for whatever the latent variable turned out to be.
# Shared: the prompt takes a parameter table, so segments or loadings both work.
#______________________________________________________________________________
# One call per segment. 
# A joint prompt confuses near-neighbor segments, because a forced one-to-one 
#   assignment lets one confusion corrupt two labels. 
# Labels are drafts for the analyst to verify against the response profiles; 
#   they never feed back into estimation. The JSON keys stay 
#   label/description/class for stability.

# Personas and rules are plain strings. 
# They never take an argument and the certification harness diffs them 
#   between runs, so a function would only get in the way.

persona_lca <- paste(
  "You are a senior survey methodologist who reads latent class analysis",
  "(LCA) measurement models. In this work each latent class is called a",
  "SEGMENT; that is a word-choice preference and the statistical object is",
  "unchanged. Each segment is described only by its item-response",
  "probabilities: for every survey item, the probability that a member of",
  "that segment gives each answer. A segment leans toward the answers with",
  "high probability. You interpret a segment strictly from these",
  "probabilities and the item wording, never from outside assumptions.")

persona_cfa <- paste(
  "You are a senior survey methodologist who reads confirmatory factor",
  "analysis (CFA) measurement models. A factor is a single continuum running",
  "from low to high, and each item is a fallible measurement of it. An item's",
  "standardized loading says how strongly that item tracks the factor: near 1",
  "means the item almost is the factor, near 0 means it carries something",
  "else. You name the factor from the items that load on it most strongly and",
  "the wording of those items, never from outside assumptions. A high loading",
  "tells you the item belongs, not which end of the scale is which; the item",
  "wording tells you that.")

# Same rules for both arms. Rule 4 is the one that keeps output parseable and
#    rule 3 is the one that stops a diffuse profile from being written up as 
# if it were sharp.
rules_label <- paste(
  "RULES:",
  "1. Use only the numbers and item wording shown. Survey context only",
  "   clarifies what the items refer to; attribute nothing that the numbers",
  "   do not show.",
  "2. Quote a probability exactly as it is printed, for one response category",
  "   at a time. Never add probabilities across categories and never describe a",
  "   combined or total probability. If two adjacent answers both matter, name",
  "   them separately with their own numbers.",
  "3. Anchor every statement to the items that stand out most.",
  "4. If nothing stands out, say the profile is diffuse rather than inventing",
  "   a theme.",
  "5. Return only valid JSON: no prose before or after, no markdown fences.",
  sep = "\n")

# Domain rules are stricter because the reader will act on them. 
# The analyst has already decided which differences clear the interval; 
#   the model is told the answer and only translates it. 
# It never sees a standard error and never decides significance for itself.
rules_domain <- paste(
  "RULES:",
  "1. Describe a difference only where it appears in the list above. For any",
  "   pair not listed, say the data do not separate the groups.",
  "2. Do not rank levels whose intervals overlap.",
  "3. Never use causal language. Groups differ in composition; being in a",
  "   group does not cause membership.",
  "4. Say nothing about a level flagged as too small.",
  "5. Report at most the four clearest differences. The point is to tell the",
  "   analyst where to look, not to narrate every cell.",
  "6. When the design changes an estimate or an interval, say so plainly and use",
  "   the numbers given. Do not treat a narrower interval as better or a wider",
  "   one as worse; the design-based figure is the honest one either way.",
  "7. Return only valid JSON: no prose before or after, no markdown fences.",
  "8. Each estimate is followed by a 95 percent confidence interval, which",
  "   expresses sampling uncertainty. Where the analysis did not resolve a",
  "   pair, say the data do not separate them; do not say they are equal or",
  "   similar, since an unresolved pair may differ by more than this sample",
  "   can detect.",
  sep = "\n")

# dictionary supplies the question wording and the response labels, in the 
#   same order as the fitted category indices.
format_segment_block <- function(fit, k, dictionary, items) {
  lines = map_chr(seq_along(items), function(j) {
    d = filter(dictionary, item == items[j])
    probs = paste(sprintf("P(%s)=%.2f", d$responses[[1]], fit$rho[[j]][, k]),
                   collapse = ", ")
    str_glue('  {items[j]} "{d$question}"\n      {probs}')
  })
  str_glue("SEGMENT {k} (estimated prevalence {round(100 * fit$pi[k])}%):\n",
           paste(lines, collapse = "\n"))
}

prompt_segment_label <- function(fit, k, dictionary, items, context = NULL) {
  ctx = if (!is.null(context) && nzchar(context))
    str_glue("SURVEY CONTEXT\n{context}\n\n") else ""
  str_glue(
    "{ctx}",
    "ONE SEGMENT FROM A LATENT CLASS ANALYSIS (LCA) MEASUREMENT MODEL\n",
    "{format_segment_block(fit, k, dictionary, items)}\n\n",
    "TASK\n",
    "Read this single segment and return: a short DRAFT label (2 to 5 words) ",
    "for an analyst to refine, and a one or two sentence factual description ",
    "anchored to its high-probability answers. Each probability above belongs ",
    "to one response category; they are not yours to add together.\n\n",
    "{rules_label}\n",
    'JSON (one object): {{"label": "...", "description": "..."}}')
}

# ---- Endpoint ---------------------------------------------------------------
# One word in the method config decides where the drafting call goes, because
#   the two arms run in different places: OpenRouter outside work, an
#   OpenAI-compatible gateway inside it.
#
#   cfg$llm_provider   "openrouter" or "work"
#   cfg$llm_model      named vector, one model id per provider
#   cfg$llm_base_url   named vector, the work gateway's URL. Unused on openrouter
#
# Keys are read from the environment and never passed as arguments, so a key
#   cannot reach a saved fit object, a traceback, or the rendered report:
#
#   openrouter -> OPENROUTER_API_KEY
#   work       -> OPENAI_API_KEY
#
# Write them one per line in ~/.Renviron, unquoted, no trailing spaces:
#
#   OPENAI_API_KEY=sk-...
#
# A quoted value keeps its quotes on some platforms, they travel into the
#   Authorization header, and the endpoint answers 401 with nothing to say why.
#   llm_key() strips a stray pair and writes the cleaned value back into the
#   session, so ellmer reads what was meant rather than what was typed.

llm_providers <- c("openrouter", "work")

# Two roles, because the two kinds of call want different things. The worker
#   drafts one label at a time and never sees another segment, which is a small
#   mechanical job repeated K times. The editor is handed every label at once and
#   asked to tell near neighbours apart, and it writes the domain readings, which
#   are prose an analyst acts on; both need to hold the whole set in view, which
#   is exactly what the drafting calls are built not to do. An empty editor entry
#   falls back to the worker, so the workflow runs unchanged on one model and a
#   second is opted into rather than required.
llm_roles <- c("worker", "editor")

# The key is never passed as an argument. ellmer's constructors read their own
#   environment variable, so the whole of the handling here is: read it, clean
#   it, and make sure the name ellmer reads holds the cleaned value. That is what
#   keeps a key out of a saved object, a traceback, and a printed cfg.
#
# The name is ellmer's choice, not the config's -- chat_openrouter() reads
#   OPENROUTER_API_KEY and the compatible endpoint reads OPENAI_API_KEY -- so
#   cfg$llm_key_var says where the key actually lives on this machine when that
#   is somewhere else, one OPENAI_API_KEY serving both endpoints being the case
#   that matters. The value is read from that name and mirrored into the name
#   ellmer reads, for this session only; nothing is written to disk and nothing
#   is passed as an argument either way.
llm_key_var_default <- function(provider)
  c(openrouter = "OPENROUTER_API_KEY", work = "OPENAI_API_KEY")[[provider]]

llm_key_var <- function(cfg, provider) {
  v = llm_field(cfg$llm_key_var, provider)
  if (is.na(v)) llm_key_var_default(provider) else v
}

llm_key <- function(cfg, provider) {
  var = llm_key_var(cfg, provider)
  ellmer_var = llm_key_var_default(provider)
  raw = trimws(Sys.getenv(var, ""))
  # A .Renviron entry written with quotes keeps them on some platforms; they
  #   travel into the Authorization header and the endpoint answers 401 with
  #   nothing that says why.
  key = str_remove_all(raw, "^[\"']|[\"']$")
  if (nzchar(key)) {
    vars = unique(c(var, ellmer_var))
    do.call(Sys.setenv, set_names(as.list(rep(key, length(vars))), vars))
  }
  key
}

# One place that reads the endpoint fields, so the report, the preflight and the
#   chat constructor cannot disagree about which endpoint is live.
# A field that is absent, NULL, unnamed for this provider, or empty all mean the
#   same thing here, and all of them have to answer is.na() rather than come back
#   length zero, or the check below fails with R's error instead of this file's.
llm_field <- function(v, nm) {
  out = unname(v[nm])
  if (length(out) != 1 || is.na(out) || !nzchar(out)) NA_character_ else out
}

llm_spec <- function(cfg, role = "worker") {
  if (!role %in% llm_roles)
    stop("role must be one of: ", paste(llm_roles, collapse = ", "), ".",
         call. = FALSE)
  provider = cfg$llm_provider %||% "openrouter"
  if (!provider %in% llm_providers)
    stop("cfg$llm_provider is '", provider, "'. It must be one of: ",
         paste(llm_providers, collapse = ", "), ".", call. = FALSE)
  worker = llm_field(cfg$llm_model_worker, provider)
  if (is.na(worker))
    stop("cfg$llm_model_worker has no entry named '", provider,
         "'. Give one model id per provider, e.g. ",
         'llm_model_worker = c(openrouter = "...", work = "...").', call. = FALSE)
  editor = llm_field(cfg$llm_model_editor, provider)
  fell_back = is.na(editor)
  model = if (role == "worker") worker else if (fell_back) worker else editor
  base_url = llm_field(cfg$llm_base_url, provider)
  if (provider == "work" && is.na(base_url))
    stop("cfg$llm_base_url has no entry named 'work'. An OpenAI-compatible ",
         "endpoint has no default URL; set the gateway's base URL, ending in ",
         "/v1, in the config.", call. = FALSE)
  list(provider = provider, role = role, model = model, base_url = base_url,
       editor_fell_back = fell_back,
       key_var = llm_key_var(cfg, provider),
       ellmer_var = llm_key_var_default(provider),
       key = llm_key(cfg, provider))
}

# chat_openai_compatible(base_url, name, system_prompt, api_key, credentials,
#   model, params, api_args, api_headers, preserve_thinking, echo) is the current
#   entry point for a gateway that speaks the OpenAI API. A locked-down library
#   can still be on an ellmer that predates it, where chat_openai(base_url=)
#   reaches the same endpoint; both read OPENAI_API_KEY, so the fallback changes
#   the call and not the credentials.
#
# Neither api_key nor credentials is passed. api_key is deprecated in current
#   ellmer, and the environment default is the one path that behaves identically
#   on both constructors and on both versions. See llm_key() above for what is
#   done to the environment instead.
#
# A corporate gateway often wants something extra on every request: a tenant id
#   or an api-version header, or a body field the endpoint requires.
#   cfg$llm_api_headers and cfg$llm_api_args pass those straight through, and are
#   only sent when the installed ellmer has the argument, so setting one cannot
#   turn into an unused-argument error on an older library.
openai_compatible_chat <- function(cfg, base_url, model, system_prompt, params) {
  fn = get0("chat_openai_compatible", asNamespace("ellmer"), mode = "function")
  ctor = fn %||% ellmer::chat_openai
  args = list(base_url = base_url, model = model,
              system_prompt = system_prompt, params = params)
  fml = names(formals(ctor))
  if ("name" %in% fml)
    args$name = cfg$llm_endpoint_name %||% "work gateway"
  if ("api_args" %in% fml && length(cfg$llm_api_args))
    args$api_args = cfg$llm_api_args
  if ("api_headers" %in% fml && length(cfg$llm_api_headers))
    args$api_headers = cfg$llm_api_headers
  do.call(ctor, args)
}

llm_chat <- function(cfg, persona = persona_lca, role = "worker") {
  s = llm_spec(cfg, role)
  p = ellmer::params(temperature = 0, seed = cfg$seed)
  if (s$provider == "openrouter")
    ellmer::chat_openrouter(model = s$model, system_prompt = persona, params = p)
  else
    openai_compatible_chat(cfg, s$base_url, s$model, persona, p)
}

# Called once from the setup chunk. A missing key fails here, in the first
#   second of the render, rather than after the enumeration has run; and the
#   line it returns is the provenance of every drafted name below it. The key
#   itself is never printed, only its length, which is enough to tell a real
#   key from an empty string or a stray pair of quotes.
llm_check <- function(cfg) {
  if (!requireNamespace("ellmer", quietly = TRUE))
    stop("Package 'ellmer' is not installed; the labelling sections cannot run.",
         call. = FALSE)
  w = llm_spec(cfg, "worker")
  e = llm_spec(cfg, "editor")
  if (!nzchar(w$key))
    stop(w$key_var, " is empty, so provider '", w$provider, "' cannot be used.\n",
         "Add it to ~/.Renviron, unquoted, and restart R:\n  ",
         w$key_var, "=<key>\n",
         "Or, if the key on this machine lives under another name, point the ",
         "config at it:\n  llm_key_var = c(", w$provider, ' = "THAT_NAME")',
         call. = FALSE)
  str_glue("LLM endpoint: {w$provider}",
           if (w$provider == "work") str_glue(" | {w$base_url}") else "",
           " | key read from {w$key_var} ({nchar(w$key)} characters)",
           if (!identical(w$key_var, w$ellmer_var))
             str_glue(", mirrored into {w$ellmer_var} for ellmer") else "",
           "\n",
           "  worker (one call per segment or factor): {w$model}\n",
           "  editor (harmonisation, domain readings): {e$model}",
           if (e$editor_fell_back)
             "  <- no cfg$llm_model_editor entry, falling back to the worker"
           else "")
}

# Some models wrap valid JSON despite rule 4, so pull the object out by pattern.
parse_json_block <- function(txt, pattern = "(?s)\\{.*\\}") {
  m = regmatches(txt, regexpr(pattern, txt, perl = TRUE))
  if (length(m) == 0) stop("No JSON found in the model reply:\n", txt)
  jsonlite::fromJSON(m, simplifyVector = FALSE)
}

label_segments_llm <- function(fit, dictionary, items, cfg) {
  map(seq_along(fit$pi), function(k) {
    obj = parse_json_block(
      llm_chat(cfg, role = "worker")$chat(
        prompt_segment_label(fit, k, dictionary, items, cfg$survey_context),
        echo = FALSE))
    tibble(K = k,
           Label = pluck(obj, "label", .default = NA_character_),
           Description = pluck(obj, "description", .default = NA_character_))
  }) |>
    list_rbind()
}

# Per-segment isolation has one blind spot: two neighbors can draft the same
#   label, since neither call saw the other. 
# One closing call edits only the labels  that collide, and runs only when 
#   this mechanical check fires.
# The collision that actually happens is a word-order synonym: "Institutional
#   trust" against "Trust in institutions". Comparing raw word sets scores that
#   pair 0.25 and lets it through, which is how a gate can look like a guard and
#   never fire. Function words are dropped and a crude suffix strip stands in for
#   a stemmer, which is enough to make those two sets equal without taking on a
#   dependency. It is a mechanical pre-filter, not a synonym detector: a
#   derivational pair such as "Economic vulnerability" against "Economically
#   vulnerable" still slips past, and the analyst reading the labels is the
#   check that catches it. Set label_harmonise = "always" in the config to stop
#   relying on the filter at all.
label_tokens <- function(x) {
  stop_words = c("a", "an", "and", "in", "of", "on", "the", "to", "with", "for",
                 "by", "or", "at", "from")
  tolower(x) |>
    str_replace_all("[^a-z ]", " ") |>
    str_squish() |>
    strsplit(" ") |>
    map(function(w) {
      w = setdiff(w, stop_words)
      stem = str_remove(w, "(ness|ality|ities|ity|ally|al|ing|ers|er|ies|es|s|y)$")
      w = if_else(nchar(stem) >= 4, stem, w)
      unique(w[nzchar(w)])
    })
}

labels_collide <- function(labels, cutoff = 0.5) {
  # One label cannot collide with anything, and combn() has no pairs to form.
  if (length(labels) < 2 || anyNA(labels)) return(FALSE)
  ws = label_tokens(labels)
  pr = t(combn(length(labels), 2L))
  any(map_dbl(seq_len(nrow(pr)), function(i) {
    a = ws[[pr[i, 1]]]
    b = ws[[pr[i, 2]]]
    if (!length(a) || !length(b)) return(0)
    length(intersect(a, b)) / length(union(a, b))
  }) >= cutoff)
}

# Whether the editor call happens at all. The gate is a house convention and the
#   config says which policy is in force, because "the larger model harmonises"
#   and "the larger model is never called" differ only by whether this returns
#   TRUE. Labels are frozen to a CSV after the first render, so "always" costs
#   one extra editor call per output directory rather than one per render.
harmonise_due <- function(labels, cfg) {
  policy = cfg$label_harmonise %||% "on_collision"
  if (!policy %in% c("on_collision", "always"))
    stop('cfg$label_harmonise is "', policy,
         '". It must be "on_collision" or "always".', call. = FALSE)
  if (length(labels) < 2 || anyNA(labels)) return(FALSE)
  policy == "always" ||
    labels_collide(labels, cfg$label_collision_cutoff %||% 0.5)
}

# unit is the word the arm uses for its latent variable, so the factor report can
#   send the same instrument without the prompt calling a factor a segment. Rows
#   are numbered by position rather than by a K column, because the factor arm
#   has no K and a position is what the reply is joined back on.
prompt_harmonize <- function(lab, unit = "SEGMENT") {
  rows = str_glue("{unit} {seq_len(nrow(lab))}: LABEL \"{lab$Label}\" | ",
                  "DESCRIPTION: {lab$Description}")
  str_glue(
    "DRAFT LABELS FOR THE {unit}S OF ONE MEASUREMENT MODEL\n",
    "{paste(rows, collapse = '\n')}\n\n",
    "TASK\n",
    "Some labels are too similar to tell apart. Edit ONLY the labels that ",
    "overlap, as little as possible, so every label is distinct; anchor each ",
    "edit to that segment's own description. Keep every non-overlapping label ",
    "verbatim. Do not change any description. Labels stay 2 to 5 words.\n\n",
    "{rules_label}\n",
    'JSON (one array, all segments): [{{"class": 1, "label": "..."}}, ...]')
}

harmonize_labels <- function(lab, cfg, unit = "SEGMENT",
                             persona = persona_lca) {
  if (!harmonise_due(lab$Label, cfg)) return(lab)
  arr = parse_json_block(
    llm_chat(cfg, persona, role = "editor")$chat(prompt_harmonize(lab, unit),
                                                 echo = FALSE),
    "(?s)\\[.*\\]")
  new_lab = map(arr, function(x) tibble(.row = as.integer(x$class),
                                         new = as.character(x$label))) |>
    list_rbind()
  lab |>
    mutate(.row = row_number()) |>
    left_join(new_lab, by = ".row") |>
    mutate(Label = coalesce(new, Label)) |>
    select(-new, -.row)
}

# lca_dir/segment_labels.csv is used when it exists, otherwise the model 
#   drafts once and writes it. Editing that file is taking over the naming.
get_segment_labels <- function(fit, dictionary, items, cfg,
                               cache = file.path(cfg$lca_dir, "segment_labels.csv")) {
  need = c("K", "Label", "Description")

  if (file.exists(cache)) {
    lab = read_csv(cache, show_col_types = FALSE)
    if (!all(need %in% names(lab)) || nrow(lab) != length(fit$pi))
      stop(cache, " does not match this model (needs ", length(fit$pi),
           " rows and columns K, Label, Description). Delete or fix it.")
    return(lab |> arrange(K) |> select(all_of(need)))
  }

  w = llm_spec(cfg, "worker")
  e = llm_spec(cfg, "editor")
  lab = label_segments_llm(fit, dictionary, items, cfg) |>
    mutate(Label_draft = Label)
  collided = harmonise_due(lab$Label, cfg)
  lab = harmonize_labels(lab, cfg) |>
    mutate(drafted_by = paste(w$provider, w$model),
           harmonised_by = if (collided) paste(e$provider, e$model) else NA_character_,
           drafted_on = as.character(Sys.Date()))
  write_csv(lab, cache)
  select(lab, all_of(need))
}


# The loading table, one line per item, sorted so the analyst and the model 
#   read the strongest indicators first.
format_factor_block <- function(fit, dictionary, factor_name = "f") {
  L = as_tibble(unclass(lavInspect(fit, "std")$lambda), rownames = "item") |>
    pivot_longer(-item, names_to = "factor", values_to = "loading") |>
    filter(factor == factor_name) |>
    arrange(desc(abs(loading)))
  lines = map_chr(seq_len(nrow(L)), function(i) {
    d = filter(dictionary, item == L$item[i])
    str_glue('  {L$item[i]} loading {sprintf("%.2f", L$loading[i])} "{d$question}"')
  })
  str_glue("FACTOR {factor_name}, items ordered by loading:\n",
           paste(lines, collapse = "\n"))
}

prompt_factor_label <- function(fit, dictionary, factor_name = "f",
                                scale_desc = NULL, context = NULL) {
  ctx = if(!is.null(context) && nzchar(context)) str_glue("SURVEY CONTEXT\n{context}\n\n") else ""
  sc = if(!is.null(scale_desc)) str_glue("RESPONSE SCALE\n{scale_desc}\n\n") else ""
  str_glue(
    "{ctx}{sc}",
    "ONE FACTOR FROM A CONFIRMATORY FACTOR ANALYSIS (CFA)\n",
    "{format_factor_block(fit, dictionary, factor_name)}\n\n",
    "TASK\n",
    "Name what this factor measures, in two to five words, and describe it in ",
    "one or two sentences. Lean on the items with the largest loadings. State ",
    "which end of the scale is high using the response scale above, since the ",
    "loadings do not tell you that.\n\n",
    "{rules_label}\n",
    'JSON (one object): {{"label": "...", "description": "...", "high_end": "..."}}')
}


# Section 6 is the CFA arm: a design-weighted correlation matrix, EFA over a
#   range of factor counts, and the CFA itself. 
# Model syntax is built from the item vector it is handed, 
#   so there is no second item list to keep in sync.
#______________________________________________________________________________

# lavaan's efa() hands back an efaList, which is a list of fits rather than a
#   fit. lavInspect and fitMeasures choke on it, so unwrap first.
as_fit <- function(f) {
  if(inherits(f, "efaList")) f[[1]] else f
}

# Builds the model string. factors is a named list of item vectors, one per
#   factor, and the names carry through to the output. 
# NULL gives one factor  over everything passed. 
# free takes residual covariances or constraints as character strings, e.g. 
#   "armed_forces ~~ police" for a pair the modification indices
#   flag, or "g ~~ 0*support" to make a bifactor orthogonal.
cfa_syntax <- function(items, factors = NULL, free = NULL) {
  spec = if(is.null(factors)) paste("f =~", paste(items, collapse = " + "))
         else imap_chr(factors, function(it, nm) paste(nm, "=~", paste(it, collapse = " + ")))
  paste(c(spec, free), collapse = "\n  ")
}

# Marker method rather than std.lv. 
# Fixing the first loading anchors the sign as well as the scale, and that 
#   matters because lavaan starts cold on every replicate refit; a sign flip 
#   would land in the variance as a huge fake deviation. 
# Same trick as align_to() in the LCA arm, different mechanism.
fit_cfa <- function(w, items, data, factors = NULL, free = NULL, ordered = TRUE) {
  d = mutate(data, .w = w)
  if(ordered) {
    cfa(cfa_syntax(items, factors, free), data = d, ordered = items,
        estimator = "WLSMV", sampling.weights = ".w")
  } else {
    cfa(cfa_syntax(items, factors, free),
        data = mutate(d, across(all_of(items), as.numeric)),
        estimator = "ML", sampling.weights = ".w", missing = "ml")
  }
}

# Every item named in the factor spec has to be in the analysis set, or lavaan
#   fails somewhere unhelpful. 
# Usually this fires because an item was commented out of item_codes and not
#   removed from cfa_factors.
check_factors <- function(factors, items) {
  missing = setdiff(unlist(factors), items)
  if(length(missing))
    stop("cfg$cfa_factors names items that are not in cfg$items: ",
         paste(missing, collapse = ", "),
         ". Either restore them to item_codes in the config or remove them ",
         "from cfa_factors.", call. = FALSE)
  invisible(TRUE)
}



# Weighted polychoric correlations, falling back to weighted Pearson if 
#  lavCor turns down the arguments. Feeds the eigenvalue search.
wcor <- function(w, items, data) {
  d = mutate(data, .w = w)
  # lavCor drops the sampling weight from the variable set on its own, so the
  #   returned matrix is over the items only. Checked rather than assumed.
  out = try(lavCor(select(d, all_of(items), .w), ordered = items,
                   sampling.weights = ".w", output = "cor"), silent = TRUE)
  if(!inherits(out, "try-error")) {
    R = as.matrix(out)
    if(!identical(colnames(R), items))
      stop("lavCor returned a matrix over ", paste(colnames(R), collapse = ", "),
           " rather than over the items. Check the lavaan version before ",
           "reading anything below.", call. = FALSE)
    return(R)
  }
  # The fallback is Pearson on the raw category codes, which is a different
  #   estimator, not a slower route to the same number. It is announced, because
  #   an eigenvalue read off a Pearson matrix and one read off a polychoric
  #   matrix are not comparable and the report would not otherwise say which
  #   it printed.
  warning("lavCor refused these arguments; falling back to weighted Pearson ",
          "correlations. Eigenvalues below are Pearson, not polychoric.",
          call. = FALSE)
  cov2cor(as.matrix(svyvar(reformulate(items),
                           svydesign(ids = ~1, weights = ~.w, data = d),
                           na.rm = TRUE)))
}

# Which items load where, at the usual 0.40 cutoff, plus the flags worth acting
#   on: an item that loads nowhere, one that loads on two factors, and one 
#   whose communality says it shares almost nothing with the battery.
efa_loadings <- function(f, salient = 0.40) {
  fit = as_fit(f)
  if(inherits(fit, "try-error")) return(NULL)
  std = lavInspect(fit, "std")
  L = unclass(std$lambda)
  # fit_efa() rotates with geomin, which is oblique, so the factors correlate
  #   and an item's communality is diag(L Phi L') rather than the sum of its
  #   squared loadings. Summing the squares understates it by 2 r l1 l2 per
  #   pair of factors: at a factor correlation of 0.5 and a 0.25 secondary
  #   loading on a 0.55 primary that is 0.14, which is enough on its own to
  #   push an item under the 0.30 line and have this table recommend dropping
  #   an item that shares plenty with the battery. Phi is the standardised
  #   factor covariance, which is their correlation matrix.
  Phi = unclass(std$psi)
  h2_item = diag(L %*% Phi %*% t(L))
  as_tibble(L, rownames = "item") |>
    pivot_longer(-item, names_to = "factor", values_to = "loading") |>
    group_by(item) |>
    mutate(n_salient = sum(abs(loading) >= salient),
           h2 = h2_item[[dplyr::first(item)]]) |>
    ungroup() |>
    mutate(flag = case_when(n_salient == 0 ~ "loads nowhere",
                            n_salient > 1 ~ "cross-loads",
                            h2 < 0.30 ~ "low communality",
                            TRUE ~ NA_character_))
}

# The search fit. Same estimator and weighting as fit_cfa, so the exploratory
#   pass and the confirmatory model are on the same footing.
#
# The rotation is asked for through an EFA block inside cfa() rather than
#   through efa(), and the difference is not stylistic. efa() takes a data frame
#   and treats every column in it as an indicator, so the sampling weight column
#   the design requires is factor-analysed alongside the items: it appears in the
#   loading table, it consumes degrees of freedom, and because a weight vector on
#   a narrow scale has a tiny variance it becomes the minimum residual variance,
#   which is the number the admissibility check reads. Naming the items in a
#   model string is what keeps the weight a weight.
fit_efa <- function(k, w, items, data) {
  d = mutate(data, .w = w)
  spec = paste0(paste0(sprintf('efa("efa")*f%d', seq_len(k)), collapse = " + "),
                " =~ ", paste(items, collapse = " + "))
  try(cfa(spec, data = d, ordered = items, estimator = "WLSMV",
          sampling.weights = ".w", rotation = "geomin"),
      silent = TRUE)
}



# Section 7 turns the domain table into a short read for the analyst. 
# This is the step that says where to look. The analyst resolves which 
# differences clear the interval before anything is sent, so the model 
# translates a verdict rather than reaching one.
#______________________________________________________________________________

# The prompt is built from the three-estimator domain frame, and the rows 
# that drive it are named rather than taken by position. 
# A frame that has already  been filtered somewhere upstream cannot be 
# translated honestly, so it halts.
pick_estimator <- function(dom, est) {
  if(!"estimator" %in% names(dom))
    stop("dom has no estimator column. Pass the full three-estimator domain ",
         "frame from the domains chunk, not a filtered copy.")
  if(!est %in% dom$estimator)
    stop("estimator '", est, "' is not present in dom. Levels found: ",
         paste(unique(dom$estimator), collapse = ", "))
  filter(dom, estimator == !!est)
}

# Lays one demographic out segment by segment, because that is the way the
#   result gets read and written up. Levels under min_n are marked rather than
#   dropped so the model can see they exist and still be told to leave them
#   alone. values_header names what the numbers are, because the class arm
#   reports shares and the factor arm reports mean positions. est names the
#   estimator whose rows are shown, rather than taking one by position.
format_domain_block <- function(
    dom, marg, variable, labels = NULL, min_n = 30,
    values_header = "Share of each level falling in each segment:",
    est = "Design-based") {
  d = pick_estimator(dom, est) |> filter(variable == !!variable)
  m = filter(marg, variable == !!variable)
  segs = sort(unique(d$segment))
  lines = map_chr(segs, function(k) {
    nm = if(is.null(labels)) paste0("Segment ", k) else labels[k]
    dd = filter(d, segment == k)
    cells = map_chr(seq_len(nrow(dd)), function(i) {
      n_lv = m$n[m$level == dd$level[i]]
      # A level the marginals did not count gives n_lv of length 0 or NA. Either
      # way it is not evidence that the level is small, so it carries no flag
      # rather than halting the prompt build on a missing value.
      flag = if(length(n_lv) == 1 && !is.na(n_lv) && n_lv < min_n)
        " [too small]" else ""
      sprintf("%s %.2f [%.2f, %.2f]%s", dd$level[i], dd$p[i], dd$lo[i],
              dd$hi[i], flag)
    })
    str_glue("  {nm}\n      {paste(cells, collapse = ', ')}")
  })
  shares = map_chr(seq_len(nrow(m)), function(i)
    sprintf("%s %d%% (n = %d)", m$level[i], round(100 * m$weighted[i]), m$n[i]))
  str_glue("{variable} in the population: {paste(shares, collapse = ', ')}\n",
           "{values_header}\n",
           paste(lines, collapse = "\n"))
}

# What changes when the design is taken into account: how far the point
#   estimate moves and whether the interval widens or narrows. The comparison 
#   is unweighted against design-based by name, so reordering the estimator 
#   levels cannot silently change what is compared.
format_estimator_shift <- function(dom, variable, labels = NULL) {
  base = pick_estimator(dom, "Unweighted") |> filter(variable == !!variable)
  desg = pick_estimator(dom, "Design-based") |> filter(variable == !!variable)
  w = inner_join(
    transmute(base, level, segment, p0 = p, w0 = hi - lo),
    transmute(desg, level, segment, p1 = p, w1 = hi - lo),
    by = c("level", "segment"))
  lines = map_chr(seq_len(nrow(w)), function(i) {
    nm = if(is.null(labels)) paste0("Segment ", w$segment[i]) else labels[w$segment[i]]
    str_glue("  {nm}, {w$level[i]}: design moves the estimate by ",
             "{sprintf('%+.3f', w$p1[i] - w$p0[i])} and makes the interval ",
             "{sprintf('%.2f', w$w1[i] / w$w0[i])} times as wide")
  })
  paste(lines, collapse = "\n")
}

# Which level pairs actually separate, worked out here rather than by the model.
#   With wald supplied, a pair counts as apart when the design-based test
#   on its difference clears alpha after Holm adjustment within the 
#   demographic, using the replicate covariance between the two estimates. 
# Without it, the old rule applies: intervals that miss each other, which is 
# more conservative than a test and is kept as the fallback for the factor arm.
domain_separations <- function(dom, variable, labels = NULL,
                               est = "Design-based", wald = NULL,
                               alpha = 0.05) {
  seg_name = function(k) if(is.null(labels)) paste0("Segment ", k) else labels[k]
  if(is.null(wald)) {
    d = pick_estimator(dom, est) |> filter(variable == !!variable)
    hits = crossing(a = unique(d$level), b = unique(d$level),
                    segment = unique(d$segment)) |>
      filter(a < b) |>
      left_join(select(d, level, segment, lo_a = lo, hi_a = hi),
                by = c("a" = "level", "segment")) |>
      left_join(select(d, level, segment, lo_b = lo, hi_b = hi),
                by = c("b" = "level", "segment")) |>
      filter(lo_a > hi_b | lo_b > hi_a) |>
      arrange(segment)
    crit_line = "  Criterion: 95 percent intervals that do not overlap."
  } else {
    m = filter(wald$meta, variable == !!variable)
    v_diag = diag(wald$V)
    hits = crossing(a = unique(m$level), b = unique(m$level),
                    segment = unique(m$segment)) |>
      filter(a < b) |>
      left_join(select(m, level, segment, idx_a = idx),
                by = c("a" = "level", "segment")) |>
      left_join(select(m, level, segment, idx_b = idx),
                by = c("b" = "level", "segment")) |>
      mutate(delta = wald$est[idx_a] - wald$est[idx_b],
             se = sqrt(pmax(v_diag[idx_a] + v_diag[idx_b]
                            - 2 * wald$V[cbind(idx_a, idx_b)], 0)),
             p = if_else(se > 0, 2 * pt(-abs(delta / se), wald$df),
                         if_else(abs(delta) > 0, 0, 1)),
             p_adj = p.adjust(p, "holm")) |>
      filter(!is.na(p_adj), p_adj < alpha) |>
      arrange(segment, p_adj)
    crit_line = paste0("  Criterion: pairwise design-based tests on the ",
                       "replicate covariance, Holm-adjusted within this ",
                       "demographic.")
  }
  if(nrow(hits) == 0)
    return(paste(crit_line, "  None: no pair of levels is resolved.", sep = "\n"))
  paste(c(crit_line,
          map_chr(seq_len(nrow(hits)), function(i)
            str_glue("  {seg_name(hits$segment[i])}: {hits$a[i]} differs from {hits$b[i]}"))),
        collapse = "\n")
}
prompt_domain_read <- function(
    dom, marg, variable, labels = NULL, context = NULL, min_n = 30,
    values_header = "Share of each level falling in each segment:",
    est = "Design-based", wald = NULL) {
  ctx = if(!is.null(context) && nzchar(context)) str_glue("SURVEY CONTEXT\n{context}\n\n") else ""
  str_glue(
    "{ctx}",
    "COMPOSITION OF THE POPULATION AND OF EACH GROUP\n",
    "{format_domain_block(dom, marg, variable, labels, min_n, values_header, est)}\n\n",
    "DIFFERENCES THE ANALYSIS RESOLVED\n",
    "{domain_separations(dom, variable, labels, est, wald)}\n\n",
    "WHAT THE SURVEY DESIGN CHANGES\n",
    "{format_estimator_shift(dom, variable, labels)}\n\n",
    "TASK\n",
    "Write two or three sentences telling an analyst what this variable shows ",
    "and where to look. Name only the differences listed above. In the caution ",
    "field, say what accounting for the survey design changed: whether it moved ",
    "any estimate enough to matter and whether it made the intervals wider or ",
    "narrower.\n\n",
    "{rules_domain}\n",
    'JSON (one object): {{"finding": "...", "caution": "..."}}')
}
