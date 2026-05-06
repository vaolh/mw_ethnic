#################################################
################# Clean Memory ##################
#################################################

rm(list = ls())
options(scipen = 999)

### REPLICATION FILE: enigh-hhlevel-year.R
### R VERSION:        4.5+
### AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
### DATE:             2026-05-03

### Builds the household × year ENIGH cross-section.
### Parity port of enigh-hhlevel-year.do — see that file for the full design.
###
### Output: ../../data/clean/enigh/enigh-hhlevel-year.dta

#################################################
################ Load + Helpers #################
#################################################

if (!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, tidyr, data.table, haven, stringr, purrr)

source("_helpers.R")

YEARS <- c(2016, 2018, 2020, 2022, 2024)
REF_PERIOD_MONTHS <- 6
deflators <- load_deflators()

#################################################
########### Aggregate from enigh-month ##########
#################################################

em <- read_dta("../../data/clean/enigh/enigh-indlevel-month.dta")
em <- as.data.table(em)
message(sprintf("Loaded enigh-month: N = %d", nrow(em)))

income_bases <- c("wages","non_wage_income","gov_transfers","rentas",
                  "fin_capital","negocio","ventas","other","lab","mon")
income_cols  <- c(paste0("ing_", income_bases, "_nom"),
                  paste0("ing_", income_bases))

first_non_na <- function(x) { ok <- which(!is.na(x)); if (length(ok)==0) NA else x[ok[1]] }
mean_safe    <- function(x) mean(x, na.rm = TRUE)

### Person × year: sum income over the 6-month reference period.
person_year_invariant <- intersect(c(
  "ubica_geo","state","ent_name","reg_num","reg_name","macro_num","macro_name",
  "tam_loc","factor","upm","est_dis","zlfn","post","treat_post","smg",
  "gender","female","edad","edad_pob","years_of_study","hoursworked",
  "employed","school_attendance","etnia","indspeaker","indund","parentesco",
  "tipo_viv","mat_pared","mat_techos","mat_pisos","antiguedad","cuart_dorm",
  "num_cuarto","disp_agua","dotac_agua","excusado","disp_elect","combustible",
  "eli_basura","tenencia","renta","estim_pago","pago_viv","tot_resid",
  "tot_hom","tot_muj","tot_hog","est_socio","sar_afore1","indep1","ind1",
  "gaspers_tri","gaspers_ntri"
), names(em))

py <- em[, c(
    lapply(.SD[, intersect(income_cols, names(em)), with = FALSE],
           function(x) sum(x, na.rm = TRUE) / REF_PERIOD_MONTHS),
    lapply(.SD[, person_year_invariant, with = FALSE], first_non_na)
  ), by = .(folioviv, foliohog, numren, year)]

#################################################
########### HH × year aggregation ###############
#################################################

py[, `:=`(
  is_kid       = as.integer(edad < 15),
  is_adult     = as.integer(edad >= 15),
  is_worker    = as.integer(employed == 1),
  is_indig     = as.integer(etnia == 1),
  is_hli       = as.integer(indspeaker == 1),
  is_female    = as.integer(gender == 0),
  is_informal  = as.integer(sar_afore1 == 0 & employed == 1)
)]

hh_invariant <- intersect(c(
  "ubica_geo","state","ent_name","reg_num","reg_name","macro_num","macro_name",
  "tam_loc","factor","upm","est_dis","zlfn","post","treat_post","smg",
  "tipo_viv","mat_pared","mat_techos","mat_pisos","antiguedad","cuart_dorm",
  "num_cuarto","disp_agua","dotac_agua","excusado","disp_elect","combustible",
  "eli_basura","tenencia","renta","estim_pago","pago_viv","tot_resid",
  "tot_hom","tot_muj","tot_hog","est_socio","gaspers_tri","gaspers_ntri"
), names(py))

hh_panel <- py[, c(
    list(
      n_kids       = sum(is_kid,      na.rm = TRUE),
      n_adults     = sum(is_adult,    na.rm = TRUE),
      n_workers    = sum(is_worker,   na.rm = TRUE),
      n_employed   = sum(is_worker,   na.rm = TRUE),
      n_indigenous = sum(is_indig,    na.rm = TRUE),
      n_hli        = sum(is_hli,      na.rm = TRUE),
      n_informal   = sum(is_informal, na.rm = TRUE),
      mean_age     = mean(edad,           na.rm = TRUE),
      mean_educ    = mean(years_of_study, na.rm = TRUE),
      share_female = mean(is_female,      na.rm = TRUE)
    ),
    lapply(.SD[, intersect(income_cols, names(py)), with = FALSE], function(x) sum(x, na.rm = TRUE)),
    lapply(.SD[, hh_invariant, with = FALSE], first_non_na)
  ), by = .(folioviv, foliohog, year)]

hh_panel[, hh_size := n_kids + n_adults]

mk_log <- function(x) ifelse(x > 0 & !is.na(x), log(x), NA_real_)
log_map <- list(wages = "lnw", non_wage_income = "lnnwi",
                gov_transfers = "lngt", rentas = "lnr",
                fin_capital = "lnfc", negocio = "lnn",
                ventas = "lnv", other = "lno",
                lab = "lni", mon = "lnmon")
for (b in income_bases) {
  hh_panel[[ log_map[[b]] ]] <- mk_log(hh_panel[[paste0("ing_", b)]])
}

#################################################
###### HH expenditure from concentradohogar #####
#################################################

base <- "../../data/source/enigh"
load_hh_exp <- function(year_int) {
  path <- file.path(base, sprintf("concentradohogar%d.dta", year_int))
  dt <- read_dta(path) |> rename_with(tolower)
  keep_cols <- c("folioviv","foliohog","ing_cor","ing_mon","ing_no_mon",
                 "gas_nm_pa","ictpch","alimentos","vesti_calz","vivienda",
                 "salud","transporte","educa_espa","personales","transf_gas",
                 "erogac_tot")
  for (c in keep_cols) if (!c %in% names(dt)) dt[[c]] <- NA
  dt <- dt[, keep_cols]
  for (v in setdiff(keep_cols, c("folioviv","foliohog"))) {
    dt[[v]] <- suppressWarnings(as.numeric(as.character(dt[[v]])))
  }
  dt <- dt |>
    rename(
      ing_cor_nom    = ing_cor,
      ing_mon_hh_nom = ing_mon,
      ing_no_mon_nom = ing_no_mon,
      gas_nm_pa_nom  = gas_nm_pa,
      ictpch_nom     = ictpch,
      gas_food_nom   = alimentos
    ) |>
    mutate(
      gas_nonfood_nom = rowSums(across(c(vesti_calz, vivienda, salud,
                                          transporte, educa_espa, personales,
                                          transf_gas)), na.rm = TRUE)
    ) |>
    select(-vesti_calz, -vivienda, -salud, -transporte, -educa_espa,
           -personales, -transf_gas)

  ago_def <- deflators$deflator[deflators$year == year_int & deflators$month == 8]
  if (length(ago_def) == 0) ago_def <- 1
  for (v in c("ing_cor_nom","ing_mon_hh_nom","ing_no_mon_nom","gas_nm_pa_nom",
              "ictpch_nom","gas_food_nom","gas_nonfood_nom","erogac_tot")) {
    real_name <- sub("_nom$", "_real", v)
    if (real_name == v) real_name <- paste0(v, "_real")
    dt[[real_name]] <- dt[[v]] / ago_def
  }
  dt$year <- year_int
  dt
}

hh_exp_all <- bind_rows(lapply(YEARS, load_hh_exp))
hh_panel <- hh_panel |>
  left_join(hh_exp_all, by = c("folioviv","foliohog","year"))

#################################################
######## HH head from poblacion #################
#################################################

load_head <- function(year_int) {
  pop <- read_dta(file.path(base, sprintf("poblacion%d.dta", year_int))) |>
    rename_with(tolower)
  pop <- pop |>
    select(folioviv, foliohog, parentesco, sexo, edad, etnia, hablaind,
           nivelaprob, gradoaprob, trabajo_mp) |>
    mutate(
      parentesco = suppressWarnings(as.numeric(as.character(parentesco))),
      sexo       = suppressWarnings(as.numeric(as.character(sexo))),
      edad       = suppressWarnings(as.numeric(as.character(edad)))
    ) |>
    filter(parentesco == 101) |>
    distinct(folioviv, foliohog, .keep_all = TRUE) |>
    mutate(
      head_age      = edad,
      head_sex      = as.integer(sexo == 1),
      head_etnia    = as.integer(etnia == "1"),
      head_indspeaker = as.integer(hablaind == "1"),
      head_employed = as.integer(trabajo_mp == "1"),
      head_educ     = build_years_of_study(nivelaprob, gradoaprob)
    ) |>
    select(folioviv, foliohog, head_age, head_sex, head_etnia,
           head_indspeaker, head_employed, head_educ) |>
    mutate(year = year_int)
  pop
}

head_all <- bind_rows(lapply(YEARS, load_head))
hh_panel <- hh_panel |>
  left_join(head_all, by = c("folioviv","foliohog","year"))

#################################################
########### HH type from hogares ################
#################################################

load_class <- function(year_int) {
  hg <- read_dta(file.path(base, sprintf("hogares%d.dta", year_int))) |>
    rename_with(tolower)
  if (!"clase_hog" %in% names(hg)) return(NULL)
  hg |>
    select(folioviv, foliohog, clase_hog) |>
    distinct(folioviv, foliohog, .keep_all = TRUE) |>
    mutate(
      clase_hog = suppressWarnings(as.numeric(as.character(clase_hog))),
      year = year_int
    )
}
class_all <- bind_rows(lapply(YEARS, load_class))
hh_panel <- hh_panel |>
  left_join(class_all, by = c("folioviv","foliohog","year"))

#################################################
################# Save output ###################
#################################################

ordered_cols <- c(
  "folioviv","foliohog","year","ubica_geo","state","ent_name",
  "reg_num","reg_name","macro_num","macro_name",
  "tam_loc","factor","upm","est_dis","zlfn","post","treat_post",
  "hh_size","n_kids","n_adults","n_workers","n_employed","n_informal",
  "n_indigenous","n_hli","mean_age","mean_educ","share_female",
  "head_age","head_sex","head_etnia","head_indspeaker","head_employed","head_educ",
  "ing_wages_nom","ing_wages","ing_non_wage_income_nom","ing_non_wage_income",
  "ing_gov_transfers_nom","ing_gov_transfers","ing_rentas_nom","ing_rentas",
  "ing_fin_capital_nom","ing_fin_capital","ing_negocio_nom","ing_negocio",
  "ing_ventas_nom","ing_ventas","ing_other_nom","ing_other",
  "ing_lab_nom","ing_lab","ing_mon_nom","ing_mon",
  "ing_cor_nom","ing_cor_real","gas_nm_pa_nom","gas_nm_pa_real",
  "gas_food_nom","gas_food_real","gas_nonfood_nom","gas_nonfood_real",
  "ictpch_nom","ictpch_real",
  "lnw","lnnwi","lngt","lnr","lnfc","lnn","lnv","lno","lni","lnmon"
)
trail <- setdiff(names(hh_panel), ordered_cols)
hh_panel <- as.data.frame(hh_panel)[, c(intersect(ordered_cols, names(hh_panel)), trail)]

write_dta_safe(hh_panel, "../../data/clean/enigh/enigh-hhlevel-year.dta")
message(sprintf("Saved enigh-household.dta — N = %d HH-year obs, vars = %d",
                nrow(hh_panel), ncol(hh_panel)))
