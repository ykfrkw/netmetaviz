# netmetaviz

**Visualization tools for network meta-analysis (`{netmeta}`) results**

Network meta-analysis (NMA) is a powerful method for simultaneously comparing multiple treatments. However, NMA produces a large number of pairwise estimates — for *k* treatments, *k(k–1)/2* comparisons — making it challenging to present findings in a way that is both complete and interpretable. `{netmetaviz}` addresses this by providing ready-made, publication-quality visualizations that organize and communicate NMA results effectively.

---

## Functions

| Function | Output | Coloring basis |
|---|---|---|
| `color_league()` | `.xlsx` | CINeMA confidence / Schneider-Thoma 2026 / solid |
| `color_league_multi()` | `.xlsx` (multi-sheet) | Same as above, across outcomes |
| `color_forest()` | plot (side effect) | CINeMA confidence vs reference |
| `color_netgraph()` | plot (side effect) | CINeMA confidence per edge |
| `kilim()` | `.xlsx` / `.docx` | Signed p-value gradient / Schneider-Thoma 2026 |
| `vitruvian()` | `ggplot` / `.png` / `.pdf` / `.svg` | Signed p-value gradient |
| `min_context()` | data frame | — (statistical grouping) |
| `part_context()` | data frame | — (threshold-based grouping) |

Sample data from the **W2I NMA** (Furukawa Y et al. 2024) is bundled for testing.

---

## Installation

```r
# Install from GitHub:
remotes::install_github("ykfrkw/netmetaviz")
```

Dependencies: `netmeta`, `meta`, `flextable`, `officer`, `openxlsx`, `ggplot2`, `dplyr`, `tidyr`, `purrr`, `stringr`, `tibble`

---

## Quick start

```r
remotes::install_github("ykfrkw/netmetaviz")
library(netmetaviz)

# Load W2I sample data and build netmeta objects
w2i         <- load_w2i()
net         <- build_w2i_netmeta("remission_lt")
cinema_path <- system.file("extdata", "w2i_cinema.csv", package = "netmetaviz")

# League table with CINeMA coloring
color_league(x = net, cinema = cinema_path, file = "league.xlsx")

# Forest plot with CI squares colored by CINeMA
color_forest(x = net, cinema = cinema_path)
```

See [sample.R](sample.R) for a complete runnable script covering all functions.

---

## 1. `color_league()` — League table with CINeMA coloring

Creates a league table (NMA results matrix) with cell colors representing CINeMA confidence ratings.

**Color scheme (default `"pastel"`):**

| CINeMA rating | Background | Text |
|---|---|---|
| High | `#d7e8d3` | `#238b21` |
| Moderate | `#cccce9` | `#01008b` |
| Low | `#f8edd7` | `#daa521` |
| Very low | `#e8d0d0` | `#8b0000` |

```r
# Single outcome, P-score sort
color_league(
  x       = net,
  cinema  = cinema_path,
  sort_by = "pscore",
  file    = "color_league.xlsx"
)

# Dual outcome (lower-left = remission, upper-right = dropout)
color_league(
  x      = net_lt,   cinema = cinema_path,
  x2     = net_dlt,
  label1 = "Remission (long-term)",
  label2 = "Dropout (long-term)",
  sort_by = "pscore",
  file   = "color_league_dual.xlsx"
)

# Quad outcome (4 outcomes in one sheet, cells split into top/bottom halves)
color_league(
  x = net_lt, cinema = cinema_path, x2 = net_pt, x3 = net_dlt, x4 = net_dpt,
  label1 = "Remission (LT)", label2 = "Remission (post-tx)",
  label3 = "Dropout (LT)",   label4 = "Dropout (post-tx)",
  sort_by = "pscore", file = "color_league_quad.xlsx"
)
```

**Key arguments:**

| Argument | Default | Description |
|---|---|---|
| `x` | — | `netmeta` object |
| `cinema` | — | Path to CINeMA CSV, or data frame |
| `sort_by` | `"alphabet"` | `"alphabet"` / `"pscore"` / `"es"` / `"es_rev"` / `"pvalue"` / `"zscore"` / `"custom"` |
| `sort_order` | `NULL` | Custom order vector (when `sort_by = "custom"`) |
| `treat_labels` | `NULL` | Named vector to rename treatments |
| `digits` | `2` | Decimal places |
| `common` | `FALSE` | Use common-effects model? |
| `palette_type` | `"pastel"` | `"pastel"` / `"classic"` / `"colorblind"` / `"SchneiderThoma2026"` / `"solid"` |
| `trivial_range` | `NULL` | `c(lo, hi)` on log scale (required for `"SchneiderThoma2026"`) |
| `fill_color` | `"#E2EFDA"` | Cell fill for outcome 1 (`palette_type = "solid"`) |
| `fill_color2` | `"#D0E4F7"` | Cell fill for outcome 2 in dual/quad mode |
| `file` | `"color_league.xlsx"` | Output path (`.xlsx`) |

**CINeMA CSV format** (exported from the [CINeMA web tool](https://cinema.ispm.unibe.ch/)):

```
"Comparison","Confidence rating",...
"CBT-I:Pharmacotherapy","Moderate",...
```

`"A:B"` and `"B:A"` are matched automatically.

---

## 2. `color_forest()` — Forest plot with CINeMA-colored CI squares

Wrapper around `netmeta::forest()` that colors each treatment's CI square by its CINeMA confidence rating versus the reference. All other arguments are forwarded to `forest.netmeta()`.

```r
color_forest(
  x               = net,
  cinema          = cinema_path,
  reference.group = "Pharmacotherapy"
)

# All forest() arguments work normally:
color_forest(
  x               = net,
  cinema          = cinema_path,
  reference.group = "Pharmacotherapy",
  sortvar         = TE,
  common          = FALSE,
  digits          = 2
)
```

**CINeMA-derived arguments** (applied automatically unless overridden by the user):

| Argument | Description |
|---|---|
| `col.square` | Fill colour of the CI square |
| `col.square.lines` | Border colour of the CI square |
| `col.study` | Label colour of the treatment name |

**Key arguments:**

| Argument | Default | Description |
|---|---|---|
| `x` | — | `netmeta` object |
| `cinema` | — | CINeMA CSV path or data frame |
| `reference.group` | `x$reference.group` | Reference treatment |
| `palette_type` | `"pastel"` | `"pastel"` / `"classic"` / `"colorblind"` |
| `col_no_cinema` | `"grey80"` | Fallback colour for treatments without a CINeMA rating |
| `...` | | Passed to `forest.netmeta()` |

Colours are applied in `x$trts` order (excluding the reference), matching the default row order of `forest.netmeta()`.

---

## 3. `color_netgraph()` — Network graph with CINeMA edge coloring

Colors each edge (direct comparison) by its CINeMA confidence rating. Node size defaults to the total number of participants in trials including each treatment.

```r
color_netgraph(x = net, cinema = cinema_path)

color_netgraph(
  x             = net,
  cinema        = cinema_path,
  palette_type  = "classic",
  col_no_cinema = "steelblue"
)
```

**Key arguments:**

| Argument | Default | Description |
|---|---|---|
| `x` | — | `netmeta` object |
| `cinema` | `NULL` | CINeMA CSV path or data frame |
| `palette_type` | `"pastel"` | `"pastel"` / `"classic"` / `"colorblind"` |
| `col_no_cinema` | `"grey60"` | Edge colour for comparisons without CINeMA rating |
| `...` | | Passed to `netmeta::netgraph()` |

---

## 4. `kilim()` — Kilim plot

Creates a multi-outcome summary table. Each cell shows the effect size (± 95% CI) and is colored using a signed p-value gradient or the Schneider-Thoma 2026 scheme.

**Gradient coloring (default `palette = "GrRd"`):**

- Deep green: p < 0.01, favorable direction
- Deep red: p < 0.01, unfavorable direction
- White text when p ≤ 0.05

```r
kilim(
  outcomes = list(
    list(
      x            = net_lt,
      name         = "remission_lt",
      reference    = "Pharmacotherapy",
      small_values = "undesirable",
      label        = "Remission\n(long-term)",
      digits       = 2
    ),
    list(
      x            = net_dlt,
      name         = "dropout_lt",
      reference    = "Pharmacotherapy",
      small_values = "desirable",
      label        = "Dropout\n(long-term)",
      digits       = 2
    )
  ),
  sort_by = "pscore",
  file    = "kilim.xlsx"
)
```

**Outcome list fields:**

| Field | Required | Description |
|---|---|---|
| `x` | Yes | `netmeta` object, or path to `.rds` file |
| `name` | Yes | Internal identifier |
| `reference` | Yes | Reference treatment name |
| `small_values` | Yes | `"desirable"` (lower = better) or `"undesirable"` (higher = better) |
| `label` | No | Column header |
| `digits` | No | Decimal places (default `2`) |
| `trivial_range` | No | Per-outcome override for trivial zone (log scale for OR/RR) |

**Top-level arguments:**

| Argument | Default | Description |
|---|---|---|
| `sort_by` | `"alphabet"` | Treatment sort order |
| `trivial_range` | `NULL` | Trivial zone applied to all outcomes |
| `palette` | `"GrRd"` | `"GrYlRd"` / `"GrRd"` / `"SchneiderThoma2026"` |
| `file` | `"kilim.docx"` | Output path (`.docx` or `.xlsx`) |

---

## 5. `vitruvian()` — Vitruvian plot

Creates a circular polar chart showing **absolute** effects for each treatment across multiple outcomes simultaneously.

- **Binary outcomes** (OR, RR, RD): converts to absolute risk using the Control Event Rate (CER). Auto-estimated from the reference arm if not supplied.
- **Continuous outcomes** (SMD): converts via the log-OR approximation (lnOR = π/√3 × SMD) when `cer` is supplied.
- **Continuous outcomes** (MD): converts to SMD using pooled SD (auto-estimated from `seTE`/`n1`/`n2`), then applies the same approximation.

Bar height = absolute risk (%); bar color = signed p-value gradient.

```r
vitruvian(
  outcomes = list(
    list(x = net_lt,  name = "remission_lt", label = "Remission\n(LT)",
         small_values = "undesirable", cer = 0.28, group = "Long-term"),
    list(x = net_dlt, name = "dropout_lt",   label = "Dropout\n(LT)",
         small_values = "desirable",  cer = 0.39, group = "Long-term"),
    list(x = net_pt,  name = "remission_pt", label = "Remission\n(post-tx)",
         small_values = "undesirable", cer = 0.28, group = "Post-treatment"),
    list(x = net_dpt, name = "dropout_pt",   label = "Dropout\n(post-tx)",
         small_values = "desirable",  cer = 0.16, group = "Post-treatment")
  ),
  reference = "Pharmacotherapy",
  digits    = 1,
  ncol      = 3,
  file      = "vitruvian.png",
  width     = 12, height = 5
)
```

**Outcome list fields:**

| Field | Description |
|---|---|
| `x` | `netmeta` object, or path to `.rds` file |
| `label` | Spoke label on the chart |
| `small_values` | `"desirable"` or `"undesirable"` |
| `cer` | Control event rate (0–1); auto-estimated (GLMM) if `NULL` |
| `pooled_sd` | For MD outcomes; auto-estimated if `NULL` |
| `group` | Group name for arc coloring |
| `trivial_range` | Per-outcome trivial zone on log/raw scale |

**Top-level arguments:**

| Argument | Default | Description |
|---|---|---|
| `reference` | first treatment | Reference treatment |
| `trivial_range` | `NULL` | Trivial zone applied to all outcomes |
| `common` | `FALSE` | Use common-effects model? |
| `digits` | `1` | Decimal places for axis labels |
| `ncol` | `NULL` | Number of columns in facet layout |
| `as_percent` | `TRUE` | Display absolute risks as percentages |
| `file` | `NULL` | Output path (`.png`, `.pdf`, `.svg`); `NULL` = show in Viewer |
| `width` | `NULL` | Width in inches. Auto-computed as `ncol × 4 + 1.8` (legend) when `NULL` |
| `height` | `NULL` | Height in inches. Auto-computed as `ceiling(n_treatments / ncol) × 4` when `NULL` |

---
## 6. `min_context()` — Minimally contextualized evidence framework

Classifies treatments into ordered evidence groups versus a reference, following the BMJ minimally contextualized framework (Tikkinen et al. 2021).

**Group assignment algorithm:**

```
Group  0 : p ≥ α  (no significant difference vs reference)
Group +1 : p < α, favorable direction
Group +2 : significantly better than ALL Group +1 treatments (p < α each)
           → iterates to +3, +4 ...
Group -1 : p < α, unfavorable direction
Group -2 : significantly worse than ALL Group -1 treatments (iterates)
```

```r
mc <- min_context(
  x            = net,
  cinema       = cinema_path,
  reference    = "Pharmacotherapy",
  small_values = "undesirable"
)
print(mc)
#>           treatment group   cinema n_total n_quality
#> 1             CBT-I     1 Moderate     ...      High
#> 2       Combination     0      ...     ...       ...
#> 3   Pharmacotherapy     0     <NA>     ...       ...

# Cross-tabulation (Group × CINeMA)
table_min_context(mc, file = "min_context.xlsx")

# Multi-outcome summary
table_min_context_multi(
  outcome_list = list("Remission (LT)" = mc_lt, "Dropout (LT)" = mc_dlt),
  file         = "min_context_multi.xlsx"
)
```

**Arguments:**

| Argument | Default | Description |
|---|---|---|
| `x` | — | `netmeta` object |
| `cinema` | `NULL` | CINeMA CSV path or data frame (optional) |
| `reference` | `x$reference.group` | Reference treatment |
| `small_values` | `"undesirable"` | Direction of benefit |
| `alpha` | `0.05` | Significance threshold |
| `n_thresholds` | `c(100, 400)` | N cutoffs for n_quality column (`NULL` to omit) |

---

## 7. `part_context()` — Partially contextualized evidence framework

Classifies treatments by converting effects to an absolute scale and comparing against user-defined clinical threshold(s), following the BMJ partially contextualized framework (Tikkinen et al. 2021).

```r
pc <- part_context(
  x            = net,
  reference    = "Pharmacotherapy",
  thresholds   = c(0.10),    # ARD ≥ 10% = smallest worthwhile difference
  cer          = 0.28,
  small_values = "undesirable",
  cinema       = cinema_path
)
print(pc)
#>           treatment abs_effect group   cinema n_total n_quality
#> 1   Pharmacotherapy  0.000      0       <NA>     ...       ...
#> 2             CBT-I  0.12...    1       High     ...      High
#> 3       Combination  0.07...    0        ...     ...       ...

attr(pc, "threshold_labels")   # group boundary labels
```

**Key arguments:**

| Argument | Default | Description |
|---|---|---|
| `x` | — | `netmeta` object |
| `reference` | `x$reference.group` | Reference treatment |
| `thresholds` | — | Numeric vector of ARD thresholds (absolute scale) |
| `cer` | — | Control event rate for binary outcomes |
| `small_values` | `"undesirable"` | Direction of benefit |
| `cinema` | `NULL` | CINeMA CSV or data frame (optional) |
| `n_thresholds` | `c(100, 400)` | N cutoffs for n_quality (`NULL` to omit) |

**Threshold groups** (example: `thresholds = c(0.10)`):

```
Group -1 : ARD < -0.10   (meaningfully harmful)
Group  0 : -0.10 ≤ ARD < 0.10  (comparable to reference)
Group +1 : ARD ≥ 0.10    (smallest worthwhile difference achieved)
```

---
## 8. Schneider-Thoma 2026 colour scheme

Available in `color_league()` (`palette_type = "SchneiderThoma2026"`) and `kilim()` (`palette = "SchneiderThoma2026"`). Colors cells categorically based on the relationship between the 95% CI and a user-defined trivial (very small) effects zone:

| Color | Condition |
|---|---|
| Blue (`#4E88B4`) | Entire 95% CI within the trivial zone |
| Yellow (`#FFD700`) | Point estimate beyond trivial, CI significant but overlaps trivial |
| Orange (`#F08000`) | Point estimate **and** entire CI beyond trivial |
| White | All other cases (non-significant, mixed) |

`trivial_range` must be specified on the same scale as the effect estimates (log scale for OR/RR/HR; raw scale for MD/SMD).

```r
# League table
color_league(
  x             = net,
  sort_by       = "pscore",
  palette_type  = "SchneiderThoma2026",
  trivial_range = log(c(1/1.1, 1.1)),   # OR 0.91–1.10 defined as trivial
  file          = "league_st2026.xlsx"
)

# Kilim table
kilim(
  outcomes      = list(...),
  trivial_range = log(c(1/1.1, 1.1)),
  palette       = "SchneiderThoma2026",
  file          = "kilim_st2026.xlsx"
)
```


---

## Sample data

The bundled **W2I dataset** contains data from a three-treatment NMA (CBT-I vs Combination vs Pharmacotherapy) for insomnia remission. Four outcomes are available:

| Outcome | Direction |
|---|---|
| `remission_lt` | Long-term remission (OR > 1 = better) |
| `dropout_lt` | Long-term dropout (OR < 1 = better) |
| `remission_pt` | Post-treatment remission (OR > 1 = better) |
| `dropout_pt` | Post-treatment dropout (OR < 1 = better) |

CINeMA confidence ratings are available for `remission_lt` only.

```r
system.file("extdata", "w2i_trials.csv", package = "netmetaviz")
system.file("extdata", "w2i_cinema.csv", package = "netmetaviz")

w2i <- load_w2i()                        # list: $trials, $cinema
net <- build_w2i_netmeta("remission_lt") # netmeta object
```

**Source:** Furukawa Y, Sakata M, Furukawa TA, Efthimiou O, Perlis M. Initial treatment choices for long-term remission of chronic insomnia disorder in adults: a systematic review and network meta-analysis. *Psychiatry Clin Neurosci*. 2024;78(11):646–653. doi:[10.1111/pcn.13730](https://doi.org/10.1111/pcn.13730)

---

## Development workflow

```r
devtools::load_all()    # reload package after editing R/ files
devtools::document()    # rebuild man/ from roxygen2 comments
devtools::check()       # R CMD CHECK
devtools::install()     # install locally

# Install latest version from GitHub
remotes::install_github("ykfrkw/netmetaviz")
```

---

## References

**Visualization methods**

- **Kilim plot**: Seo M, Furukawa TA, Veroniki AA, et al. The Kilim plot: A tool for visualizing network meta-analysis results for multiple outcomes. *Res Synth Methods*. 2021;12(1):86–95. doi:[10.1002/jrsm.1428](https://doi.org/10.1002/jrsm.1428)

- **Vitruvian plot**: Ostinelli EG, Efthimiou O, Naci H, et al. Vitruvian plot: a visualisation tool for multiple outcomes in network meta-analysis. *Evid Based Ment Health*. 2022;25(e1):e65–e70. doi:[10.1136/ebmental-2022-300457](https://doi.org/10.1136/ebmental-2022-300457)

- **Schneider-Thoma 2026 colour scheme**: Schneider-Thoma J, Zhu Y, Qin M, et al. Comparative efficacy and tolerability of antidopaminergic and muscarinic antipsychotics for acute schizophrenia: a network meta-analysis. *Lancet*. 2026;407(10531):876–891. doi:[10.1016/S0140-6736(25)02365-7](https://doi.org/10.1016/S0140-6736(25)02365-7)

- **Minimally contextualised framework**: Tikkinen KAO, Guyatt GH, Dening SM, et al. Drug effects and natural history of disease in minimally and partially contextualised evidence frameworks. *BMJ*. 2021;372:m3900. doi:[10.1136/bmj.m3900](https://doi.org/10.1136/bmj.m3900)

- **Partially contextualised framework**: Brignardello-Petersen R, Izcovich A, Rochwerg B, et al. GRADE approach to drawing conclusions from a network meta-analysis using a partially contextualised framework. *BMJ*. 2020;371:m3907. doi:[10.1136/bmj.m3907](https://doi.org/10.1136/bmj.m3907)

**Evidence quality**

- **CINeMA**: Nikolakopoulou A, Higgins JPT, Papakonstantinou T, et al. CINeMA: An approach for assessing confidence in the results of a network meta-analysis. *PLOS Med*. 2020;17(4):e1003082. doi:[10.1371/journal.pmed.1003082](https://doi.org/10.1371/journal.pmed.1003082)

**Software**

- **netmeta**: Rücker G, Krahn U, König J, Efthimiou O, Davies A, Papakonstantinou T, Schwarzer G. netmeta: Network Meta-Analysis using Frequentist Methods. R package. https://CRAN.R-project.org/package=netmeta

**Sample dataset**

- Furukawa Y, Sakata M, Furukawa TA, Efthimiou O, Perlis M. Initial treatment choices for long-term remission of chronic insomnia disorder in adults: a systematic review and network meta-analysis. *Psychiatry Clin Neurosci*. 2024;78(11):646–653. doi:[10.1111/pcn.13730](https://doi.org/10.1111/pcn.13730)
