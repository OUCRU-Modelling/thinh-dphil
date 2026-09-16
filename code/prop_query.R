# ===========================================================================
# proportion_queries.R
#
# Pooled proportions within arbitrary date windows, for writing up results.
#
# Usage:
#   source("proportion_queries.R")
#   prop_help()
#
# Sourcing defines functions only. Nothing runs, and nothing here depends on
# objects in your global environment.
#
# Requires: dplyr, tidyr, tibble, lubridate (attached or installed).
# ===========================================================================

local({
  need <- c("dplyr", "tidyr", "tibble", "lubridate")
  missing <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("proportion_queries.R needs: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
})

# Delete this line if you would rather sourcing were silent
message("proportion_queries.R loaded. Run prop_help() for usage.")


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Accepts Date objects and ISO strings. Day-first strings such as "01-07-2019"
# are rejected with a pointer to lubridate::dmy(), because as.Date() would
# either error or silently misread them.
.as_date <- function(x, arg) {
  if (inherits(x, "Date")) return(x)
  out <- suppressWarnings(as.Date(x))
  if (length(out) == 0 || all(is.na(out))) {
    stop("`", arg, "` must be a Date or an ISO string such as \"2019-07-01\". ",
         "For day-first strings use dmy(\"01-07-2019\").", call. = FALSE)
  }
  out
}

.check_window_args <- function(from, to) {
  if (from > to) {
    stop("`from` (", format(from), ") is later than `to` (", format(to), ").",
         call. = FALSE)
  }
  invisible(TRUE)
}

# Case-level data to one row per week per group, with the within-week
# proportion. If the group column is a factor, unused levels are retained so
# they can be filled with zeros later; if it is character, only observed
# values appear.
.weekly <- function(data, group, week_col) {
  out <- data |>
    dplyr::filter(!is.na({{ group }}), !is.na({{ week_col }})) |>
    dplyr::count({{ week_col }}, grp = {{ group }}, name = "n", .drop = FALSE) |>
    dplyr::rename(week = {{ week_col }})
  
  if (nrow(out) == 0) {
    stop("No rows left after dropping missing values in the grouping or week ",
         "column. Check the column names passed to `group` and `week_col`.",
         call. = FALSE)
  }
  
  out |> dplyr::mutate(prop = n / sum(n), .by = week)
}

.warn_if_empty <- function(d, from, to) {
  if (nrow(d) == 0) {
    warning("No data between ", format(from), " and ", format(to),
            ". If this window crosses an axis break or a plot trim, query ",
            "the untrimmed data instead.", call. = FALSE)
    return(TRUE)
  }
  FALSE
}


# ---------------------------------------------------------------------------
# check_window()
#
#   data      case-level data, one row per case
#   from, to  window bounds, Date or ISO string
#   week_col  column holding the week start date (default `week`)
#
#   Returns one row: expected weeks, weeks actually containing data, case
#   count, and the first and last week present. Run this before quoting any
#   figure from a window that might cross a gap.
# ---------------------------------------------------------------------------

check_window <- function(data, from, to, week_col = week) {
  from <- .as_date(from, "from")
  to   <- .as_date(to,   "to")
  .check_window_args(from, to)
  
  present <- data |>
    dplyr::filter(!is.na({{ week_col }}),
                  dplyr::between({{ week_col }}, from, to)) |>
    dplyr::rename(week = {{ week_col }})
  
  expected <- seq(lubridate::floor_date(from, "week"),
                  lubridate::floor_date(to, "week"), by = "week")
  
  tibble::tibble(
    from = from,
    to   = to,
    weeks_expected  = length(expected),
    weeks_with_data = dplyr::n_distinct(present$week),
    cases = nrow(present),
    first_week = if (nrow(present) > 0) min(present$week) else as.Date(NA),
    last_week  = if (nrow(present) > 0) max(present$week) else as.Date(NA)
  )
}


# ---------------------------------------------------------------------------
# prop_window()
#
#   data      case-level data
#   group     unquoted column name, e.g. cohort or age_group
#   from, to  window bounds
#   groups    optional character vector to restrict the output rows
#   digits    decimal places in the `write_up` string
#
#   Returns one row per group: cases, total, pooled pct, the mean of the
#   weekly proportions, and a formatted string for pasting into text.
#
#   Report `pct`. `mean_weekly_pct` weights every week equally regardless of
#   how many cases it contains, so it is only a check on how much the weekly
#   denominators vary; a large gap between the two means sparse weeks are
#   pulling the average around.
# ---------------------------------------------------------------------------

prop_window <- function(data, group, from, to, groups = NULL,
                        week_col = week, digits = 1) {
  from <- .as_date(from, "from")
  to   <- .as_date(to,   "to")
  .check_window_args(from, to)
  
  sub <- .weekly(data, {{ group }}, {{ week_col }}) |>
    dplyr::filter(dplyr::between(week, from, to))
  
  if (.warn_if_empty(sub, from, to)) {
    return(tibble::tibble(grp = character(), cases = integer(),
                          total = integer(), pct = numeric(),
                          mean_weekly_pct = numeric(), write_up = character()))
  }
  
  res <- sub |>
    dplyr::summarise(cases = sum(n), mean_weekly = mean(prop), .by = grp) |>
    tidyr::complete(grp, fill = list(cases = 0, mean_weekly = 0)) |>
    dplyr::mutate(
      total = sum(cases),
      pct   = 100 * cases / total,
      mean_weekly_pct = 100 * mean_weekly,
      write_up = sprintf(paste0("%.", digits, "f%% (%d/%d)"),
                         pct, cases, total)
    ) |>
    dplyr::select(grp, cases, total, pct, mean_weekly_pct, write_up) |>
    dplyr::arrange(grp)
  
  if (!is.null(groups)) res <- dplyr::filter(res, grp %in% groups)
  res
}


# ---------------------------------------------------------------------------
# prop_by()
#
#   As prop_window(), but binned within the window.
#   unit      "month", "quarter" or "year"
#
#   Bins are assigned from the week start date, so a week spanning a month
#   boundary falls entirely in the earlier month. For month-exact figures,
#   pass the admission date column as `week_col` instead of the week column.
#
#   Groups with no cases in a bin are filled with zero, so the series has no
#   holes. This only works if the group column is a factor.
# ---------------------------------------------------------------------------

prop_by <- function(data, group, from, to, unit = "month", groups = NULL,
                    week_col = week, digits = 1) {
  from <- .as_date(from, "from")
  to   <- .as_date(to,   "to")
  .check_window_args(from, to)
  unit <- match.arg(unit, c("month", "quarter", "year"))
  
  sub <- .weekly(data, {{ group }}, {{ week_col }}) |>
    dplyr::filter(dplyr::between(week, from, to))
  
  if (.warn_if_empty(sub, from, to)) {
    return(tibble::tibble(bin = as.Date(character()), grp = character(),
                          cases = integer(), total = integer(),
                          pct = numeric(), write_up = character()))
  }
  
  res <- sub |>
    dplyr::mutate(bin = lubridate::floor_date(week, unit)) |>
    dplyr::summarise(cases = sum(n), .by = c(bin, grp)) |>
    tidyr::complete(bin, grp, fill = list(cases = 0)) |>
    dplyr::mutate(total = sum(cases), pct = 100 * cases / total, .by = bin) |>
    dplyr::mutate(write_up = sprintf(paste0("%.", digits, "f%% (%d/%d)"),
                                     pct, cases, total)) |>
    dplyr::arrange(bin, grp)
  
  if (!is.null(groups)) res <- dplyr::filter(res, grp %in% groups)
  res
}


# ---------------------------------------------------------------------------
# trend_test()
#
#   Binomial GLM of one group's share against time, on binned counts.
#   level     the single group level to model, e.g. "2019-2023"
#
#   Returns the odds ratio per bin with a Wald confidence interval.
#
#   Two caveats. The model assumes a constant change in log-odds per bin, so
#   a level shift at a campaign date will be summarised as a slope that fits
#   neither the before nor the after period. And these are admissions, not a
#   random sample, so the interval describes sampling variation under a
#   hypothetical repetition rather than uncertainty about a wider population.
#   Inspect prop_by() output first and only quote this if the series actually
#   looks monotonic.
# ---------------------------------------------------------------------------

trend_test <- function(data, group, from, to, level, unit = "month",
                       week_col = week) {
  dat <- prop_by(data, {{ group }}, from, to, unit = unit,
                 groups = level, week_col = {{ week_col }})
  
  if (nrow(dat) < 3) {
    stop("Need at least 3 bins to fit a trend; got ", nrow(dat),
         ". Try a wider window or unit = \"month\".", call. = FALSE)
  }
  
  per <- c(month = 30.44, quarter = 91.31, year = 365.25)[[unit]]
  dat$t <- as.numeric(dat$bin - min(dat$bin)) / per
  
  fit <- stats::glm(cbind(cases, total - cases) ~ t,
                    family = stats::binomial, data = dat)
  ci <- stats::confint.default(fit)["t", ]
  
  tibble::tibble(
    group  = level,
    unit   = unit,
    n_bins = nrow(dat),
    or_per_unit = exp(stats::coef(fit)[["t"]]),
    ci_low  = exp(ci[[1]]),
    ci_high = exp(ci[[2]]),
    p_value = summary(fit)$coefficients["t", 4],
    first_pct = dat$pct[which.min(dat$bin)],
    last_pct  = dat$pct[which.max(dat$bin)]
  )
}


# ---------------------------------------------------------------------------
# prop_help()
# ---------------------------------------------------------------------------

prop_help <- function() {
  cat(
    "proportion_queries.R\n\n",
    "check_window(data, from, to)\n",
    "    Weeks expected vs weeks with data. Run first on any window that\n",
    "    might cross a gap.\n\n",
    "prop_window(data, group, from, to, groups = NULL, digits = 1)\n",
    "    Pooled percentage per group across the whole window.\n\n",
    "prop_by(data, group, from, to, unit = \"month\", groups = NULL)\n",
    "    Same, binned by month, quarter or year.\n\n",
    "trend_test(data, group, from, to, level, unit = \"month\")\n",
    "    Odds ratio per bin for one group. Check prop_by() first.\n\n",
    "Dates: Date objects or ISO strings. Use dmy() for day-first strings.\n",
    "The `write_up` column is formatted for pasting into text.\n\n",
    "Example:\n",
    "    check_window(df_week, dmy(\"01-07-2019\"), dmy(\"30-04-2020\"))\n",
    "    prop_window(df_week, cohort, dmy(\"01-07-2019\"), dmy(\"30-04-2020\"))\n",
    "    prop_by(df_week, cohort, dmy(\"01-07-2019\"), dmy(\"30-04-2020\"),\n",
    "            groups = \"2019-2023\")\n",
    sep = ""
  )
  invisible(NULL)
}