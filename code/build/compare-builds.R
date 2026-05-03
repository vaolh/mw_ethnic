#################################################
############# Build Parity Check ################
#################################################

### REPLICATION FILE: compare-builds.R
### R VERSION:        4.5+
### AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
### DATE:             2026-05-03

### Compares two .dta files (Stata-built vs R-built versions of the same
### dataset). Asserts:
###   1. Identical variable lists
###   2. Identical observation count (N)
###   3. Per-variable mean / SD agreement within numeric tolerance
###
### Writes a markdown report to:
###   quality_reports/build-parity/YYYY-MM-DD_<dataset>_parity.md
###
### Exit non-zero on failure (so Make can stop the build).
###
### Usage:
###   Rscript compare-builds.R <stata_dta> <r_dta> <dataset_name>
### Example:
###   Rscript compare-builds.R \
###     ../../data/clean/enigh/enigh-month.dta \
###     ../../data/clean/enigh/enigh-month-R.dta \
###     enigh-month

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

TOLERANCE <- 1e-6

#################################################
############### Argument parsing ################
#################################################

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: Rscript compare-builds.R <stata_dta> <r_dta> <dataset_name>")
}
stata_path <- args[1]
r_path     <- args[2]
ds_name    <- args[3]

if (!file.exists(stata_path)) stop(sprintf("Stata file not found: %s", stata_path))
if (!file.exists(r_path))     stop(sprintf("R file not found: %s",     r_path))

#################################################
############### Load both files #################
#################################################

stata_df <- as.data.frame(read_dta(stata_path))
r_df     <- as.data.frame(read_dta(r_path))

#################################################
############### Compare lengths #################
#################################################

n_diff   <- nrow(stata_df) - nrow(r_df)
nvar_s   <- ncol(stata_df)
nvar_r   <- ncol(r_df)

#################################################
############# Variable list overlap #############
#################################################

vars_only_stata <- setdiff(names(stata_df), names(r_df))
vars_only_r     <- setdiff(names(r_df),     names(stata_df))
vars_both       <- intersect(names(stata_df), names(r_df))

#################################################
######### Per-variable mean / SD compare ########
#################################################

compare_numeric <- function(s, r) {
  s <- suppressWarnings(as.numeric(s))
  r <- suppressWarnings(as.numeric(r))
  if (all(is.na(s)) && all(is.na(r))) {
    return(list(s_mean = NA, r_mean = NA, mean_diff = 0,
                s_sd = NA, r_sd = NA, sd_diff = 0,
                s_n = sum(!is.na(s)), r_n = sum(!is.na(r)), kind = "all-NA"))
  }
  list(
    s_mean = mean(s, na.rm = TRUE),
    r_mean = mean(r, na.rm = TRUE),
    mean_diff = mean(s, na.rm = TRUE) - mean(r, na.rm = TRUE),
    s_sd = sd(s, na.rm = TRUE),
    r_sd = sd(r, na.rm = TRUE),
    sd_diff = sd(s, na.rm = TRUE) - sd(r, na.rm = TRUE),
    s_n = sum(!is.na(s)),
    r_n = sum(!is.na(r)),
    kind = "numeric"
  )
}

results <- lapply(vars_both, function(v) {
  s <- stata_df[[v]]
  r <- r_df[[v]]
  if (is.numeric(s) || is.integer(s) || is.numeric(r) || is.integer(r)) {
    out <- compare_numeric(s, r)
    out$var <- v
    out
  } else {
    list(var = v, kind = "non-numeric",
         s_mean = NA, r_mean = NA, mean_diff = 0,
         s_sd = NA, r_sd = NA, sd_diff = 0,
         s_n = sum(!is.na(s)), r_n = sum(!is.na(r)))
  }
})

result_df <- do.call(rbind, lapply(results, as.data.frame))
result_df$mean_ok <- ifelse(
  is.na(result_df$mean_diff), TRUE,
  abs(result_df$mean_diff) <= TOLERANCE * pmax(1, abs(result_df$s_mean), na.rm = TRUE)
)
result_df$sd_ok <- ifelse(
  is.na(result_df$sd_diff), TRUE,
  abs(result_df$sd_diff) <= TOLERANCE * pmax(1, abs(result_df$s_sd), na.rm = TRUE)
)

#################################################
################ Verdict ########################
#################################################

failures <- result_df |>
  filter(!mean_ok | !sd_ok | s_n != r_n)

PASS <- length(vars_only_stata) == 0 &&
        length(vars_only_r)     == 0 &&
        n_diff                  == 0 &&
        nrow(failures)          == 0

#################################################
############### Write report ####################
#################################################

report_dir <- "../../quality_reports/build-parity"
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
date_str   <- format(Sys.Date(), "%Y-%m-%d")
report_path <- file.path(report_dir, sprintf("%s_%s_parity.md", date_str, ds_name))

cat(
  sprintf("# Parity Report — %s\n\n", ds_name),
  sprintf("**Date:** %s\n", date_str),
  sprintf("**Stata file:** `%s`\n", stata_path),
  sprintf("**R file:** `%s`\n\n", r_path),
  sprintf("**Verdict:** %s\n\n", ifelse(PASS, "PASS", "FAIL")),
  sprintf("- Tolerance: %g\n", TOLERANCE),
  sprintf("- Stata N: %d, R N: %d (Δ = %d)\n",
          nrow(stata_df), nrow(r_df), n_diff),
  sprintf("- Stata vars: %d, R vars: %d\n", nvar_s, nvar_r),
  sprintf("- Common vars: %d\n", length(vars_both)),
  sprintf("- Vars only in Stata: %d\n", length(vars_only_stata)),
  sprintf("- Vars only in R: %d\n\n", length(vars_only_r)),
  sep = "", file = report_path
)

if (length(vars_only_stata) > 0) {
  cat("## Variables only in Stata\n\n", file = report_path, append = TRUE)
  cat(paste0("- `", vars_only_stata, "`\n", collapse = ""),
      file = report_path, append = TRUE)
  cat("\n", file = report_path, append = TRUE)
}
if (length(vars_only_r) > 0) {
  cat("## Variables only in R\n\n", file = report_path, append = TRUE)
  cat(paste0("- `", vars_only_r, "`\n", collapse = ""),
      file = report_path, append = TRUE)
  cat("\n", file = report_path, append = TRUE)
}
if (nrow(failures) > 0) {
  cat("## Numeric divergences (above tolerance)\n\n",
      file = report_path, append = TRUE)
  cat("| Variable | Stata mean | R mean | Δ mean | Stata sd | R sd | Δ sd | N (S,R) |\n",
      file = report_path, append = TRUE)
  cat("|---|---:|---:|---:|---:|---:|---:|---|\n",
      file = report_path, append = TRUE)
  for (i in seq_len(nrow(failures))) {
    cat(sprintf("| `%s` | %.6g | %.6g | %.3g | %.6g | %.6g | %.3g | (%d, %d) |\n",
                failures$var[i], failures$s_mean[i], failures$r_mean[i],
                failures$mean_diff[i], failures$s_sd[i], failures$r_sd[i],
                failures$sd_diff[i], failures$s_n[i], failures$r_n[i]),
        file = report_path, append = TRUE)
  }
  cat("\n", file = report_path, append = TRUE)
}

#################################################
################ Stdout summary #################
#################################################

cat(sprintf("\n=== Parity check: %s ===\n", ds_name))
cat(sprintf("  Stata N: %d, R N: %d, Δ = %d\n", nrow(stata_df), nrow(r_df), n_diff))
cat(sprintf("  Common vars: %d  |  only Stata: %d  |  only R: %d\n",
            length(vars_both), length(vars_only_stata), length(vars_only_r)))
cat(sprintf("  Numeric divergences: %d / %d\n", nrow(failures), length(vars_both)))
cat(sprintf("  Verdict: %s\n", ifelse(PASS, "PASS", "FAIL")))
cat(sprintf("  Report: %s\n", report_path))

if (!PASS) {
  quit(status = 1, save = "no")
}
