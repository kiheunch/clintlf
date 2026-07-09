# ClinTLF

> **Clin**ical Trial **T**able, **L**isting, and **F**igure Utilities

<!-- badges: start -->
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/clintlf)](https://cran.r-project.org/package=clintlf) -->
[![R-CMD-check](https://github.com/kiheunch/clintlf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kiheunch/clintlf/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**clintlf** is a lightweight, standalone collection of formatting and data
utilities for producing tables, listings, and figures (TLFs). Whether you work
in base R, the tidyverse, or a proprietary pipeline, it integrates into
existing workflows without tying you to a specific framework, package
ecosystem, or dependency stack.

While developed for clinical trial reporting, colleagues in other domains
may find it just as useful for reproducible reporting. It covers:

* Half-away-from-zero (commercial) rounding that handles floating-point representation issues
* Display-ready character output for numeric results using decimal-place specifications rather than significant figures
* Confidence intervals (`"xx (xx, xx)"`), percentages (`"xx (xx%)"`), and p-values (`"<xx"`) formatted for table cells
* Partial date imputation across the formats and separators found in international clinical datasets
* PARAM metadata and variable label extraction from ADaM/CDISC datasets
* Multiple imputation pooling by Rubin's rules, consistent with SAS `PROC MIANALYZE` and R `mice`, accepting estimates and standard errors from any model without requiring adoption of an entire multiple imputation framework

---

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("kiheunch/clintlf")
```

Once published to CRAN, install with:

```r
install.packages("clintlf")
```

---

## Overview

**clintlf** is organized into four functional areas:

1. [Table Formatting](#1-table-formatting)
2. [Date Imputation](#2-date-imputation)
3. [ADaM Metadata Extraction](#3-adam-metadata-extraction)
4. [Framework-free Calculation](#4-framework-free-calculation)

---

## 1. Table Formatting

A family of `fmt_*` functions converts numeric values into consistently
formatted character strings ready for insertion into clinical summary tables.

### `fmt_round()` — Half-away-from-zero rounding

Rounds half away from zero (commercial rounding), matching SAS `round()`;
base R rounds half to even. Returns character output with trailing zeros
kept, at the requested number of decimal places.

```r
library(clintlf)

# Base R rounds to even; fmt_round rounds half away from zero
round(c(9.5, 10.5), 0)
#> [1] 10 10

fmt_round(c(9.5, 10.5), 0)
#> [1] "10" "11"

fmt_round(c(2.5, 0.035, NA), c(0, 2, 2))
#> [1] "3"    "0.04" "NA"
```

### `fmt_ci()` — Estimate with confidence interval

Combines a point estimate and its confidence interval bounds into a single
string (`"xx (xx, xx)"`), with independent decimal place control for the
estimate and bounds.

```r
fmt_ci(3.14159, 1.23456, 5.67890)
#> [1] "3.14 (1.23, 5.68)"

fmt_ci(3.14159, 1.23456, 5.67890, est_n = 2, ci_n = 1)
#> [1] "3.14 (1.2, 5.7)"

# Vectorised
fmt_ci(c(1.111, 2.222), c(0.500, 1.500), c(1.700, 2.900))
#> [1] "1.11 (0.50, 1.70)" "2.22 (1.50, 2.90)"
```

### `fmt_percent()` — Value with percentage

Formats a count or value alongside its percentage (`"xx (xx%)"`), with
independent decimal control and a `"<"` threshold for percentages that round
to zero. A zero denominator prints `zero_denom` (default `"-"`) in place of
the undefined percentage.

```r
fmt_percent(15, 100)
#> [1] "15 (15%)"

fmt_percent(c(30, 50, 20), n = 0, perc_n = 2, symbol = "")
#> [1] "30 (30.00)" "50 (50.00)" "20 (20.00)"

fmt_percent(1, 1000, perc_n = 0)
#> [1] "1 (<1%)"
fmt_percent(1, 1000, perc_n = 1)
#> [1] "1 (0.1%)"

fmt_percent(0, 0)
#> [1] "0 (-)"
```

### `fmt_pval()` — P-value with significance threshold

Displays p-values below the significance threshold as `"<alpha"` and rounds
the rest half away from zero.

```r
fmt_pval(0.055, alpha = "0.05")
#> [1] "0.06"

fmt_pval(c(0.051, 0.001), n = 3, alpha = "0.05")
#> [1] "0.051"   "<0.05"

# Per-element thresholds
fmt_pval(c(0.051, 0.001), n = 3, alpha = c("0.05", "0.10"))
#> [1] "0.051"   "<0.10"
```

---

## 2. Date Imputation

### `impute_date()` — Impute partial dates to `yyyy-mm-dd`

Imputes missing month and/or day components from partial date strings,
supporting `"ymd"`, `"mdy"`, and `"dmy"` input orderings and common separators
(`-`, `/`, `.`). Time components in ISO 8601, US, and European/UK datetime
formats are automatically stripped before imputation. Output is always returned
as a `yyyy-mm-dd` `character` vector.

> **Note:** Out-of-range month or day components raise a warning, but
> calendar validity is not checked - validate with `as.Date()` or
> `lubridate::ymd()` when it matters.

```r
# Standard partial date imputation (ymd)
impute_date(c("2025-07-19", "2025-07", "2025-7", "2025", NA))
#> [1] "2025-07-19" "2025-07-01" "2025-07-01" "2025-01-01" NA

# Custom fill values
impute_date(c("2025-07-19", "2025-07", "2025"),
            fill_month = "12", fill_day = "31")
#> [1] "2025-07-19" "2025-07-31" "2025-12-31"

# Datetime strings — time component stripped automatically
impute_date(c("2026-05-22T15:30:45", "2026-05T15:30:45"))
#> [1] "2026-05-22" "2026-05-01"

# Non-ymd formats
impute_date(c("07-19-2025", "07-2025", "2025"), fmt = "mdy")
#> [1] "2025-07-19" "2025-07-01" "2025-01-01"

impute_date(c("19-07-2025", "07-2025", "2025"), fmt = "dmy")
#> [1] "2025-07-19" "2025-07-01" "2025-01-01"

# Separator auto-detection
impute_date(c("2025/07/19", "2025/07", "2025"))
#> [1] "2025-07-19" "2025-07-01" "2025-01-01"
```

---

## 3. ADaM Metadata Extraction

Utility functions for extracting parameter and variable metadata from CDISC ADaM 
datasets, supporting both single data frames and named lists. Useful in 
cross-reference tables, data dictionaries, or quick visual inspection.

### `dict_param()` — Extract PARAM metadata

Summarizes unique combinations of `PARAM`, `PARAMCD`, and `PARAMN` from 
ADaM datasets (e.g., ADLB, ADTTE, ADQS). When a named list is provided, 
datasets without a `PARAM` column are dropped with a message naming them.

```r
library(haven)

# Single ADaM data frame
adlb <- read_xpt("adlb.xpt")
dict_param(adlb)
#>   PARAMN PARAMCD                           PARAM
#> 1      1     ALT   Alanine Aminotransferase (U/L)
#> 2      2     AST  Aspartate Aminotransferase (U/L)

# Named list of datasets
dict_param(list(adlb = read_xpt("adlb.xpt"),
                advs = read_xpt("advs.xpt")))
```

### `dict_label()` — Extract variable labels

Extracts the `"label"` attribute from each column of a CDISC dataset,
returning a data frame of variable codes and labels.

```r
adsl <- read_xpt("adsl.xpt")
dict_label(adsl)
#>   varcode                    label
#> 1 STUDYID            Study Identifier
#> 2 USUBJID            Unique Subject Identifier
#> ...

# Named list of datasets
dict_label(list(adsl = read_xpt("adsl.xpt"),
                adae = read_xpt("adae.xpt")))
```

---

## 4. Framework-free Calculation

### `calc_poolmi()` — Pool multiple imputation analysis results using Rubin's rules

Multiple imputation pooling via Rubin's rules, numerically consistent with SAS PROC MIANALYZE and R mice, 
accepting estimates and standard errors directly from any model without requiring adoption of an entire 
multiple imputation framework. Supply the per-imputation estimates and standard errors directly as
vectors, or as columns of a stacked data frame (one row per imputation).

It is particularly useful in workflows where a model is fitted across a list of imputed datasets and results are stacked into a single data frame,
one row per imputation, before pooling. Two degrees-of-freedom methods are supported: 

| `df` argument | Method | Approximates |
|---|---|---|
| `"1987"` (default) | Rubin (1987) | SAS `PROC MIANALYZE` default |
| `"1999"` | Barnard & Rubin (1999) | R `mice::pool()` or SAS `PROC MIANALYZE` with EDF |

```r
# Vectors of estimates and standard errors across 5 imputed datasets
calc_poolmi(est = c(-2.57, -2.11, -1.85, -2.07, -2.13),
            se  = c( 1.05,  0.91,  0.92,  0.85,  0.90))
#>   pool.est   pool.se     lower      upper      p.val
#> 1   -2.146 0.9717592 -4.055013 -0.2369871 0.02765002

# Or stacked into a data frame, one row per imputation
imp_result <- data.frame(
  IMPNO = 1:5,
  est   = c(-2.57, -2.11, -1.85, -2.07, -2.13),
  SE    = c( 1.05,  0.91,  0.92,  0.85,  0.90)
)

# Pool using Rubin (1987)
calc_poolmi(est = "est", se = "SE", data = imp_result)
#>   pool.est   pool.se     lower      upper      p.val
#> 1   -2.146 0.9717592 -4.055013 -0.2369871 0.02765002
```

#### Pooling results from independently fitted models

In practice, the estimates come from a model fitted once per imputed dataset.
Assume `admi` is a stacked, multiply imputed analysis dataset — one row per
subject per visit, with `IMPNO` identifying the imputation. Split it into one
data frame per imputation:

```r
admi_list <- split(admi, admi$IMPNO)
```

Fit the same model within each imputation and keep the estimate and standard
error of the effect of interest. Mixed models are the usual choice for
longitudinal endpoints, but for simplicity, `lm()` is used:

```r
model_results <- do.call(rbind, lapply(admi_list, function(dat) {
  wk24  <- subset(dat, AVISIT == "Week 24")
  coefs <- summary(lm(CHG ~ BASE + TRTP, data = wk24))$coefficients
  data.frame(est = coefs["TRTPDrug", "Estimate"],
             SE  = coefs["TRTPDrug", "Std. Error"])
}))
```

`model_results` now holds one row per imputation, ready to pool:

```r
calc_poolmi(est = "est", se = "SE", data = model_results)
```

Any model — mixed models included — slots into the same pattern, as long as
each imputation-specific fit supplies an estimate and its standard error.


**References:**

- Rubin, D.B. (1987). *Multiple Imputation for Nonresponse in Surveys*. Wiley.
- Barnard, J. and Rubin, D.B. (1999). Small-sample degrees of freedom with
  multiple imputation. *Biometrika*, 86(4), 948-955.

---

## Function Reference

| Function | Area | Description |
|---|---|---|
| `fmt_round()` | Formatting | Half-away-from-zero rounding with character output |
| `fmt_ci()` | Formatting | Estimate with confidence interval — `"xx (xx, xx)"` |
| `fmt_percent()` | Formatting | Value with percentage — `"xx (xx%)"` |
| `fmt_pval()` | Formatting | P-value with significance threshold label |
| `impute_date()` | Data Preparation | Impute partial dates to `yyyy-mm-dd` character format |
| `dict_param()` | Data Dictionary | Extract `PARAM`/`PARAMCD`/`PARAMN` metadata from ADaM datasets |
| `dict_label()` | Data Dictionary | Extract column label attributes from CDISC datasets |
| `calc_poolmi()` | Calculation | Pool estimates across imputations via Rubin's rules |

---

## License

MIT © Ki Heun Chung