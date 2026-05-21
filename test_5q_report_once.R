# 5q validation report for lfsclean_5q
# Run from package root:
#   Rscript test_5q_report_once.R

suppressPackageStartupMessages({
  library(data.table)
})

for (pkg in c("here", "Hmisc", "crayon", "dplyr", "stringr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, quiet = TRUE)
  }
}

if (dir.exists("R")) {
  r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  if (length(r_files) > 0) {
    for (f in r_files) {
      source(f, local = .GlobalEnv)
    }
  }
}

if (!exists("lfsclean_5q", mode = "function") && !requireNamespace("lfsclean", quietly = TRUE)) {
  stop("Could not find lfsclean_5q. Ensure you run this from package root so R/ can be sourced, or install lfsclean.")
}

run_lfsclean_5q <- function(...) {
  if (exists("lfsclean_5q", mode = "function")) {
    return(lfsclean_5q(...))
  }
  lfsclean::lfsclean_5q(...)
}

root <- "X:/"
file <- "HAR_PR/PR/LFS/Data/longitudinal/tab/"
years <- 2012:2022

required_vars <- c(
  "id", "persid", "year", "quarter", "month", "sex", "empl_sequence",
  paste0("age", 1:5),
  paste0("hiqual", 1:5),
  paste0("eth2cat", 1:5),
  paste0("region", 1:5),
  paste0("empstat2cat", 1:5),
  paste0("empstat3cat", 1:5),
  paste0("empstat8cat", 1:5),
  paste0("disab", 1:5),
  paste0("benclaim", 1:5),
  paste0("uhours", 1:5),
  "weekly_earnings1", "weekly_earnings5",
  "weekly_earnings_nom1", "weekly_earnings_nom5",
  "uwage1", "uwage5",
  "uwage_nom1", "uwage_nom5"
)

summarise_var <- function(dt, v, yr) {
  if (!(v %in% names(dt))) {
    return(data.table(
      year = yr,
      variable = v,
      exists = FALSE,
      n = nrow(dt),
      non_missing = NA_integer_,
      pct_non_missing = NA_real_,
      n_unique_non_missing = NA_integer_,
      sample_values = NA_character_
    ))
  }

  x <- dt[[v]]
  non_missing <- sum(!is.na(x))
  pct_non_missing <- if (length(x) == 0) NA_real_ else round(non_missing / length(x), 4)
  x_non_na <- x[!is.na(x)]

  sample_values <- NA_character_
  if (length(x_non_na) > 0) {
    sample_values <- paste(head(unique(as.character(x_non_na)), 5), collapse = " | ")
  }

  data.table(
    year = yr,
    variable = v,
    exists = TRUE,
    n = nrow(dt),
    non_missing = non_missing,
    pct_non_missing = pct_non_missing,
    n_unique_non_missing = uniqueN(x_non_na),
    sample_values = sample_values
  )
}

cat("Running lfsclean_5q once for years:", paste(years, collapse = ", "), "\n")

all_data <- tryCatch(
  run_lfsclean_5q(
    root = root,
    file = file,
    year = years,
    ages = 16:89,
    keep_vars = NULL,
    complete_vars = NULL,
    deflator = "cpih"
  ),
  error = function(e) e
)

if (inherits(all_data, "error")) {
  stop("lfsclean_5q failed: ", conditionMessage(all_data))
}

all_data <- as.data.table(all_data)

if (!("year" %in% names(all_data))) {
  stop("lfsclean_5q output does not contain 'year', so variables cannot be checked over time.")
}

row_report <- data.table(year = years)
row_report[, n_rows := all_data[, .N, by = year][.SD, on = "year", x.N]]
row_report[is.na(n_rows), n_rows := 0L]

var_report <- list()

for (yr in years) {
  dt <- all_data[year == yr]

  var_report[[as.character(yr)]] <- rbindlist(
    lapply(required_vars, function(v) summarise_var(dt, v, yr)),
    fill = TRUE
  )
}

variable_report <- rbindlist(var_report, fill = TRUE)
missing_vars_report <- variable_report[exists == FALSE, .(year, variable)]

cat("\nRows by year:\n")
print(row_report[order(year)])

cat("\nMissing required variables by year:\n")
if (nrow(missing_vars_report) == 0) {
  cat("None\n")
} else {
  print(missing_vars_report[order(year, variable)])
}

cat("\nVariable completeness tabulation (required vars, by year):\n")
print(variable_report[order(year, variable)])

cat("\nVariable completeness summary (avg pct non-missing by variable across years):\n")
print(
  variable_report[
    exists == TRUE,
    .(
      years_present = .N,
      avg_pct_non_missing = round(mean(pct_non_missing, na.rm = TRUE), 4),
      min_pct_non_missing = round(min(pct_non_missing, na.rm = TRUE), 4),
      max_pct_non_missing = round(max(pct_non_missing, na.rm = TRUE), 4)
    ),
    by = .(variable)
  ][order(avg_pct_non_missing, variable)]
)

if (any(row_report$n_rows == 0L) || nrow(missing_vars_report) > 0) {
  stop("Variable-over-time validation failed: at least one year has 0 rows or missing required variables.")
} else {
  cat("\nVariable-over-time validation passed.\n")
}
