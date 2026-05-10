#################################################
################# Clean Memory ##################
#################################################

rm(list = ls())
options(scipen = 999)

### REPLICATION FILE: enigh-indlevel-inc-year.R
### R VERSION:        4.5+
### AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
### DATE:             2026-05-02

### Builds the individual × year ENIGH cross-section by collapsing
### enigh-month.dta over the 6-month reference period and merging
### CONEVAL poverty / formality flags.
###
### Canonical variable list — same as enigh-month (modulo the dropped
### `month` and `time` index, plus the merged CONEVAL columns).
###
### Outputs:
###   ../../data/clean/enigh/enigh-indlevel-inc-year.dta
###   ../../data/clean/enigh/enigh-indlevel-inc-year.RData
###
### Companion .do script: enigh-indlevel-inc-year.do (must produce the same N + sums
### within 1e-6, validated by compare-builds.R).

#################################################
################ Load + Helpers #################
#################################################

if (!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, tidyr, data.table, haven, stringr,
               srvyr, survey, Hmisc, dineq)

source("_helpers.R")

YEARS <- c(2016, 2018, 2020, 2022, 2024)
REF_PERIOD_MONTHS <- 6

#################################################
############### Load enigh-month ################
#################################################

month_path <- "../../data/clean/enigh/enigh-indlevel-inc-month.dta"
if (!file.exists(month_path)) {
  stop(sprintf("enigh-month.dta not found at %s — build enigh-month first.",
               month_path))
}
em <- read_dta(month_path)
message(sprintf("Loaded enigh-month: N = %d rows, vars = %d", nrow(em), ncol(em)))

#################################################
############# Aggregate to year #################
#################################################

### Income variables: SUM over months / REF_PERIOD_MONTHS = monthly average
### over the survey reference period (treating no-income months as 0).
income_bases <- c("wages","non_wage_income","gov_transfers","rentas",
                  "fin_capital","negocio","ventas","other","lab","mon")
income_cols  <- c(paste0("ing_", income_bases, "_nom"),
                  paste0("ing_", income_bases))

### First-non-missing reducer for invariant individual attributes.
first_non_na <- function(x) {
  ok <- which(!is.na(x))
  if (length(ok) == 0) NA else x[ok[1]]
}

invariant_cols <- c(
  "ubica_geo","state","ent_name","reg_num","reg_name","macro_num","macro_name",
  "tam_loc","factor","upm","est_dis",
  "zlfn","post","treat_post",
  "gender","female","edad","edad_pob","edadsq","years_of_study","hoursworked",
  "employed","school_attendance","motherhome","fatherhome",
  "etnia","indspeaker","indund","smg",
  ### work-related main and secondary job
  "scian1","sinco1","ind1","subor1","indep1","personal1","pago1",
  "contrato1","tipocontr1","hours1",
  "scian2","sinco2","ind2","subor2","indep2","personal2","pago2",
  "contrato2","tipocontr2","hours2",
  ### benefits (main job)
  "incapacidad1","aguinaldo1","vacaciones1","utilidades1","credito_vivienda1",
  "guarderias1","cuidados_parentales1","sar_afore1","seguro_vida1","prestamos1",
  "prima_vacacional1","becas1","comedor1","fonacot1","despensa1",
  "servicios_publicos1","pension_invalidez1","pension_familia1",
  "otras_prestaciones1","sin_prestaciones1",
  ### benefits (secondary job)
  "incapacidad2","aguinaldo2","vacaciones2","utilidades2","credito_vivienda2",
  "guarderias2","cuidados_parentales2","sar_afore2","seguro_vida2","prestamos2",
  "prima_vacacional2","becas2","comedor2","fonacot2","despensa2",
  "servicios_publicos2","pension_invalidez2","pension_familia2",
  "otras_prestaciones2","sin_prestaciones2",
  ### housing
  "tipo_viv","mat_pared","mat_techos","mat_pisos","antiguedad","cuart_dorm",
  "num_cuarto","disp_agua","dotac_agua","excusado","disp_elect","combustible",
  "eli_basura","tenencia","renta","estim_pago","pago_viv",
  "tot_resid","tot_hom","tot_muj","tot_hog","est_socio",
  ### misc
  "gaspers_tri","gaspers_ntri","parentesco","num_trabaj"
)
invariant_cols <- intersect(invariant_cols, names(em))

em_dt <- as.data.table(em)

agg <- em_dt[, c(
    lapply(.SD[, intersect(income_cols, names(em_dt)), with = FALSE],
           function(x) sum(x, na.rm = TRUE) / REF_PERIOD_MONTHS),
    lapply(.SD[, intersect(invariant_cols, names(em_dt)), with = FALSE],
           first_non_na)
  ), by = .(folioviv, foliohog, numren, year)]

### Re-derive logs from real income columns.
mk_log <- function(x) ifelse(x > 0 & !is.na(x), log(x), NA_real_)
for (b in income_bases) {
  real_col <- paste0("ing_", b)
  log_col  <- switch(b,
    "wages"            = "lnw",
    "non_wage_income"  = "lnnwi",
    "gov_transfers"    = "lngt",
    "rentas"           = "lnr",
    "fin_capital"      = "lnfc",
    "negocio"          = "lnn",
    "ventas"           = "lnv",
    "other"            = "lno",
    "lab"              = "lni",
    "mon"              = "lnmon"
  )
  agg[[log_col]] <- mk_log(agg[[real_col]])
}

#################################################
############### CONEVAL merge ###################
#################################################

coneval_dir <- "../../data/source/coneval"
load_coneval <- function(year_int) {
  path <- file.path(coneval_dir, sprintf("pobreza%d.dta", year_int))
  if (!file.exists(path)) {
    warning(sprintf("CONEVAL file not found for %d: %s", year_int, path))
    return(NULL)
  }
  cv <- read_dta(path) |> rename_with(tolower)
  cv <- cv |> filter(pea == 1 | pea == 2)
  ### CONEVAL ictpc, ict, nomon are nominal monthly per CONEVAL convention;
  ### deflate to Aug-2024 pesos using the August INPC of the survey year.
  defl <- load_deflators()
  ago_def <- defl$deflator[defl$year == year_int & defl$month == 8]
  if (length(ago_def) == 0) ago_def <- 1
  cv <- cv |> mutate(
    hli      = ifelse(is.na(hli), 2, hli),
    informal = ifelse(pea == 1 & ss_dir == 0, 1L, 0L),
    ictpc_nom = ictpc, ict_nom = ict, nomon_nom = nomon,
    ictpc    = ictpc / ago_def,
    ict      = ict   / ago_def,
    nomon    = nomon / ago_def,
    reg_esp  = reg_esp  / ago_def,
    pago_esp = pago_esp / ago_def
  )
  ### Drop CONEVAL income aggregates that we already compute from ENIGH.
  drop_cols <- intersect(c("ing_mon","ing_lab","ing_ren","ing_tra"), names(cv))
  cv <- cv[, setdiff(names(cv), drop_cols)]
  cv$year <- year_int
  cv
}

coneval_all <- bind_rows(lapply(YEARS, load_coneval))

### Restrict CONEVAL columns to the canonical poverty / formality set
### plus identifiers, to avoid name collisions with already-merged vars.
cv_keep <- intersect(
  c("folioviv","foliohog","numren","year",
    "hli","hlio","hlm","informal","pea","ss_dir",
    "ictpc","ict","nomon","reg_esp","pago_esp",
    "ictpc_nom","ict_nom","nomon_nom",
    "clas_emp","tam_emp","sector"),
  names(coneval_all)
)
coneval_all <- coneval_all[, cv_keep]

cross <- agg |>
  left_join(coneval_all, by = c("folioviv","foliohog","numren","year"))

#################################################
############ Distributional ranks ###############
#################################################

ntiles_wtd <- function(x, n, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  out <- rep(NA_integer_, length(x))
  if (sum(ok) == 0) return(out)
  ord <- order(x[ok])
  cw  <- cumsum(w[ok][ord]) / sum(w[ok])
  br  <- findInterval(cw, seq_len(n) / n, rightmost.closed = TRUE) + 1L
  br[br > n] <- n
  out_ok <- integer(sum(ok))
  out_ok[ord] <- br
  out[ok] <- out_ok
  out
}

cross <- cross |>
  group_by(year) |>
  mutate(
    deciles_ictpc   = ntiles_wtd(ictpc,   10,  factor),
    deciles_inglab  = ntiles_wtd(ing_lab, 10,  factor),
    centiles_ictpc  = ntiles_wtd(ictpc,   100, factor),
    centiles_inglab = ntiles_wtd(ing_lab, 100, factor)
  ) |>
  ungroup()

#################################################
################# Save outputs ##################
#################################################

cross_dta <- as.data.frame(cross)
write_dta_safe(cross_dta, "../../data/clean/enigh/enigh-indlevel-inc-year.dta")
message(sprintf("Saved enigh-year.dta — N = %d, vars = %d",
                nrow(cross_dta), ncol(cross_dta)))

save(cross, file = "../../data/clean/enigh/enigh-indlevel-inc-year.RData")
message("Saved enigh-indlevel-inc-year.RData")
