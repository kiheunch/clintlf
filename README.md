# ClinTLF

> **Clin**ical Trial **T**able, **L**isting, and **F**igure Utilities

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/clintlf)](https://cran.r-project.org/package=clintlf)
[![R-CMD-check](https://github.com/kiheunch/clintlf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kiheunch/clintlf/actions/workflows/R-CMD-check.yaml)
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

**clintlf** is organized into three functional areas:

1. [Table Formatting](#1-table-formatting)
2. [Data Preparation](#2-data-preparation)
3. [ADaM Metadata Extraction](#3-adam-metadata-extraction)

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

### `fmt_stat()` — Descriptive statistics cells

One builder for summary cells: a lead value followed by a bracketed group
(`"xx (xx, xx)"`), with independent decimal control for each part. Mean (SD),
median (Q1, Q3), an estimate with its confidence bounds, and min, max all
come from the same call.

```r
fmt_stat(23.456, 4.5678, inner_n = 2)                    # mean (SD)
#> [1] "23.5 (4.57)"

fmt_stat(5.678, 2.345, 8.901)                            # median (Q1, Q3)
#> [1] "5.7 (2.3, 8.9)"

fmt_stat(3.14159, 1.23456, 5.6789, n = 2, inner_n = 2)   # estimate (95% CI)
#> [1] "3.14 (1.23, 5.68)"

fmt_stat(NULL, 0.123, 9.876, bracket = "")               # min, max
#> [1] "0.1, 9.9"

# NA replacement, e.g. the SD of a single observation
fmt_stat(c(12.3, 15.8), c(4.56, NA), na = "-")
#> [1] "12.3 (4.6)" "15.8 (-)"
```

Since it is vectorized, it can be used in functions like `dplyr::mutate()`:

```r
results %>%
  mutate(output = fmt_stat(estimate, lowerci, upperci, n = 2, inner_n = 2))
#>   estimate lowerci upperci            output
#> 1    1.234   0.512   2.099 1.23 (0.51, 2.10)
#> 2    5.678   4.444   6.912 5.68 (4.44, 6.91)
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

## 2. Data Preparation

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

## Function Reference

| Function | Area | Description |
|---|---|---|
| `fmt_round()` | Formatting | Half-away-from-zero rounding with character output |
| `fmt_stat()` | Formatting | Descriptive statistics cells — `"xx (xx, xx)"` |
| `fmt_percent()` | Formatting | Value with percentage — `"xx (xx%)"` |
| `fmt_pval()` | Formatting | P-value with significance threshold label |
| `impute_date()` | Data Preparation | Impute partial dates to `yyyy-mm-dd` character format |
| `dict_param()` | Data Dictionary | Extract `PARAM`/`PARAMCD`/`PARAMN` metadata from ADaM datasets |
| `dict_label()` | Data Dictionary | Extract column label attributes from CDISC datasets |

---

## License

MIT © Ki Heun Chung