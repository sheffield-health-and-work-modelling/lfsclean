# 5q validation report for lfsclean_5q
# Run from package root:
#   Rscript test_5q_report_once.R

suppressPackageStartupMessages({
  library(data.table)
})

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
}

if (!exists("lfsclean_5q", mode = "function") && !requireNamespace("lfsclean", quietly = TRUE)) {
  stop("Could not find lfsclean_5q. Install package or run with devtools available.")
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

checks <- list()
var_report <- list()

for (yr in years) {
  dt <- all_data[year == yr]

  if (nrow(dt) == 0) {
    checks[[paste0("rows_", yr)]] <- data.table(
      year = yr,
      status = "FAIL",
      check = "rows_present",
      detail = "No rows returned for this year"
    )
    next
  }

  checks[[paste0("rows_", yr)]] <- data.table(
    year = yr,
    status = "PASS",
    check = "rows_present",
    detail = paste("Rows:", nrow(dt))
  )

  missing_vars <- setdiff(required_vars, names(dt))
  checks[[paste0("required_", yr)]] <- data.table(
    year = yr,
    status = if (length(missing_vars) == 0) "PASS" else "FAIL",
    check = "required_columns_exist",
    detail = if (length(missing_vars) == 0) "" else paste(missing_vars, collapse = ", ")
  )

  eth_cols <- paste0("eth2cat", 1:5)
  if (all(eth_cols %in% names(dt))) {
    eth_non_missing <- sum(!is.na(unlist(dt[, ..eth_cols])))
    bad_levels <- FALSE

    for (ec in eth_cols) {
      vals <- unique(as.character(dt[[ec]][!is.na(dt[[ec]])]))
      if (any(!(vals %in% c("white", "non_white")))) {
        bad_levels <- TRUE
        break
      }
    }

    checks[[paste0("eth_nonmiss_", yr)]] <- data.table(
      year = yr,
      status = if (eth_non_missing > 0) "PASS" else "FAIL",
      check = "ethnicity_has_non_missing",
      detail = paste("Total non-missing eth2cat1-5:", eth_non_missing)
    )

    checks[[paste0("eth_levels_", yr)]] <- data.table(
      year = yr,
      status = if (!bad_levels) "PASS" else "FAIL",
      check = "ethnicity_levels_valid",
      detail = if (!bad_levels) "" else "Found values outside white/non_white"
    )
  }

  var_report[[as.character(yr)]] <- rbindlist(
    lapply(required_vars, function(v) summarise_var(dt, v, yr)),
    fill = TRUE
  )
}

checks_report <- rbindlist(checks, fill = TRUE)
variable_report <- rbindlist(var_report, fill = TRUE)

fwrite(checks_report, "5q_checks_report.csv")
fwrite(variable_report, "5q_variable_report.csv")

cat("\nSaved reports:\n")
cat(" - 5q_checks_report.csv\n")
cat(" - 5q_variable_report.csv\n\n")

cat("Check summary:\n")
print(checks_report[, .N, by = .(year, status)][order(year, status)])

if (any(checks_report$status == "FAIL")) {
  stop("One or more checks failed. See 5q_checks_report.csv")
} else {
  cat("\nAll checks passed.\n")
}
