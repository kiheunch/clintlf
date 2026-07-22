# ClinTLF

> **Clin**ical Trial **T**able, **L**isting, and **F**igure Utilities

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/clintlf)](https://cran.r-project.org/package=clintlf)
[![R-CMD-check](https://github.com/kiheunch/clintlf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kiheunch/clintlf/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**clintlf** is a small, dependency-free collection of formatting and data
utilities for preparing tables, listings, and figures (TLFs). 
It handles the formatting and partial-date chores that come up in every
reporting pipeline and leaves the rest to you. Whether you work in base R, the
tidyverse, or a proprietary pipeline, it integrates into existing workflows
without tying you to a specific framework, package ecosystem, or dependency
stack.

While developed for clinical trial reporting, colleagues in other domains may 
find it just as useful for reproducible reporting. It covers:

* Half-away-from-zero rounding that handles floating-point representation issues
* Numeric results rendered to a fixed number of decimals, trailing zeros kept
* Ready-made table cells: confidence intervals (`"xx (xx, xx)"`), percentages (`"xx (xx%)"`), p-values (`"<xx"`)
* Vectorized!
* Partial-date imputation across common international formats and separators
* Variable-label extraction for building a data dictionary

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

The `fmt_*` functions take numeric results and hand back the character strings
you drop straight into a summary table, formatted the same way every time.

### `fmt_round()` - Half-away-from-zero rounding

Rounds half away from zero (commercial rounding), the convention behind most
clinical reporting, where base R would round half to even instead. Output is
character, fixed at the number of decimals you ask for, trailing zeros and all.

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

### `fmt_stat()` - Statistics cells

Many analyses result cells in TLFs are a lead value followed by a bracketed group, e.g.,
`"xx (xx, xx)"`, so `fmt_stat()` builds all of them: mean (SD), median (Q1, Q3),
an estimate with its confidence bounds, or min-max with no lead at all. The
lead and the bracketed values round to their own decimal places.

```r
fmt_stat(23.456, 4.5678, inner_n = 2)                    # mean (SD)
#> [1] "23.5 (4.57)"

fmt_stat(5.678, 2.345, 8.901)                            # median (Q1, Q3)
#> [1] "5.7 (2.3, 8.9)"

fmt_stat(3.14159, 1.23456, 5.6789, n = 2, inner_n = 2)   # estimate (95% CI)
#> [1] "3.14 (1.23, 5.68)"

fmt_stat(NULL, 0.123, 9.876)                             # min, max (no lead, no brackets)
#> [1] "0.1, 9.9"

fmt_stat(NULL, 0.5, 2.5, bracket = "[)")                 # inclusive/exclusive range
#> [1] "[0.5, 2.5)"

# NA replacement, e.g. the SD of a single observation
fmt_stat(c(12.3, 15.8), c(4.56, NA), na = "-")
#> [1] "12.3 (4.6)" "15.8 (-)"
```

Being vectorized, it drops straight into `dplyr::mutate()`:

```r
results %>%
  mutate(output = fmt_stat(estimate, lowerci, upperci, n = 1, inner_n = 2))
#>   estimate lowerci upperci            output
#> 1    1.234   0.512   2.099  1.2 (0.51, 2.10)
#> 2    5.678   4.444   6.912  5.7 (4.44, 6.91)
```

### `fmt_percent()` - Value with percentage

Pairs a count with its percentage, `"xx (xx%)"`. When a nonzero count rounds to
0%, it prints a `"<"` threshold instead (say `"<1%"`), so the cell never misreads
as a flat zero. A zero denominator has no percentage to show, so those cells
print `zero_denom` (default `"-"`).

```r
# Specify numerator and denominator
fmt_percent(15, 100)
#> [1] "15 (15%)"

# Denominator as sum of numerator
fmt_percent(c(30, 50, 20), n = 0, perc_n = 2, symbol = "")
#> [1] "30 (30.00)" "50 (50.00)" "20 (20.00)"

fmt_percent(1, 1000, perc_n = 0)
#> [1] "1 (<1%)"
fmt_percent(1, 1000, perc_n = 1)
#> [1] "1 (0.1%)"

fmt_percent(0, 0)
#> [1] "0 (-)"
```

### `fmt_pval()` - P-value with significance threshold

Shows any p-value below the threshold as `"<alpha"` and rounds the rest half
away from zero. Pass `alpha` as a string to keep its trailing zeros in the
label.

```r
fmt_pval(0.055, alpha = "0.05")
#> [1] "0.06"

fmt_pval(c(0.051, 0.001), n = 3, alpha = "0.05")
#> [1] "0.051"   "<0.05"

# Per-element thresholds for hierarchical multiple testing
fmt_pval(c(0.051, 0.001), n = 3, alpha = c("0.05", "0.10"))
#> [1] "0.051"   "<0.10"
```

---

## 2. Data Preparation

### `impute_date()` - Impute partial dates to `yyyy-mm-dd`

Fills the missing month and day of a partial date, accepting `"ymd"`, `"mdy"`,
and `"dmy"` orderings and the usual separators (`-`, `/`, `.`). 
Datetime strings get their time component stripped first. 
Whatever goes in, a `yyyy-mm-dd` character vector comes out.

The defaults follow the end-of-period convention: a missing month becomes `"12"`
and a missing day becomes the last day of the resolved month, leap years
included. For the earliest-date convention, pass `fill_month = "01"`,
`fill_day = "01"`.

> **Note:** An out-of-range month or day raises a warning, but calendar validity
> does not. `"2025-02-31"` will pass, so validate with `as.Date()` or
> `lubridate::ymd()` when it matters.

```r
# Standard partial date imputation (ymd) - default fills last month / last day
impute_date(c("2025-07-19", "2025-07", "2025-7", "2025", NA))
#> [1] "2025-07-19" "2025-07-31" "2025-07-31" "2025-12-31" NA

# Earliest-date convention via custom fill values
impute_date(c("2025-07-19", "2025-07", "2025"),
            fill_month = "01", fill_day = "01")
#> [1] "2025-07-19" "2025-07-01" "2025-01-01"

# Datetime strings - time component stripped automatically
impute_date(c("2026-05-22T15:30:45", "2026-05T15:30:45"))
#> [1] "2026-05-22" "2026-05-31"

# Non-ymd formats
impute_date(c("07-19-2025", "07-2025", "2025"), fmt = "mdy")
#> [1] "2025-07-19" "2025-07-31" "2025-12-31"

impute_date(c("19-07-2025", "07-2025", "2025"), fmt = "dmy")
#> [1] "2025-07-19" "2025-07-31" "2025-12-31"

# Separator auto-detection
impute_date(c("2025/07/19", "2025/07", "2025"))
#> [1] "2025-07-19" "2025-07-31" "2025-12-31"
```

---

## 3. ADaM Metadata Extraction

Two helpers for pulling parameter and variable metadata out of CDISC ADaM
datasets, each accepting a single data frame or a named list of them. Handy for
cross-reference tables, data dictionaries, or a quick look at what a dataset
holds.

### `dict_param()` - Extract PARAM metadata

Returns the unique `PARAM`, `PARAMCD`, and `PARAMN` combinations in an ADaM
dataset such as ADLB, ADTTE, or ADQS. Given a named list, any dataset without a
`PARAM` column is dropped, and a message tells you which.

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

### `dict_label()` - Extract variable labels

Reads the `"label"` attribute off each column of a CDISC dataset and returns
the variable codes with their labels as a data frame.

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
| `fmt_round()` | Table Formatting | Half-away-from-zero rounding with character output |
| `fmt_stat()` | Table Formatting | Descriptive statistics cells - `"xx (xx, xx)"` |
| `fmt_percent()` | Table Formatting | Value with percentage - `"xx (xx%)"` |
| `fmt_pval()` | Table Formatting | P-value with significance threshold label |
| `impute_date()` | Data Preparation | Impute partial dates to `yyyy-mm-dd` character format |
| `dict_param()` | Data Dictionary | Extract `PARAM`/`PARAMCD`/`PARAMN` metadata from ADaM datasets |
| `dict_label()` | Data Dictionary | Extract column label attributes from CDISC datasets |

---

## License

MIT © Ki Heun Chung