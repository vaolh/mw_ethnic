#################################################
################# Clean Memory ##################
#################################################

rm(list = ls())
options(scipen = 999)

### REPLICATION FILE: enigh-household-month.R
### R VERSION:        4.5+
### AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
### DATE:             2026-05-03

### Builds the household × month ENIGH panel.
### Parity port of enigh-household-month.do.
###
### Output: ../../data/clean/enigh/enigh-household-month.dta

#################################################
################ Load + Helpers #################
#################################################

if (!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, data.table, haven)

source("_helpers.R")

#################################################
########### Aggregate from enigh-month ##########
#################################################

em <- read_dta("../../data/clean/enigh/enigh-month.dta")
em <- as.data.table(em)
message(sprintf("Loaded enigh-month: N = %d", nrow(em)))

income_bases <- c("wages","non_wage_income","gov_transfers","rentas",
                  "fin_capital","negocio","ventas","other","lab","mon")
income_cols  <- c(paste0("ing_", income_bases, "_nom"),
                  paste0("ing_", income_bases))

first_non_na <- function(x) { ok <- which(!is.na(x)); if (length(ok)==0) NA else x[ok[1]] }

em[, `:=`(
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
  "tot_hom","tot_muj","tot_hog","est_socio","deflator","time"
), names(em))

hh_month <- em[, c(
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
    lapply(.SD[, intersect(income_cols, names(em)), with = FALSE],
           function(x) sum(x, na.rm = TRUE)),
    lapply(.SD[, hh_invariant, with = FALSE], first_non_na)
  ), by = .(folioviv, foliohog, year, month)]

hh_month[, hh_size := n_kids + n_adults]

mk_log <- function(x) ifelse(x > 0 & !is.na(x), log(x), NA_real_)
log_map <- list(wages = "lnw", non_wage_income = "lnnwi",
                gov_transfers = "lngt", rentas = "lnr",
                fin_capital = "lnfc", negocio = "lnn",
                ventas = "lnv", other = "lno",
                lab = "lni", mon = "lnmon")
for (b in income_bases) {
  hh_month[[ log_map[[b]] ]] <- mk_log(hh_month[[paste0("ing_", b)]])
}

#################################################
################ Save output ####################
#################################################

ordered_cols <- c(
  "folioviv","foliohog","year","month","time",
  "ubica_geo","state","ent_name","reg_num","reg_name","macro_num","macro_name",
  "tam_loc","factor","upm","est_dis","zlfn","post","treat_post",
  "hh_size","n_kids","n_adults","n_workers","n_employed","n_informal",
  "n_indigenous","n_hli","mean_age","mean_educ","share_female",
  "ing_wages_nom","ing_wages","ing_non_wage_income_nom","ing_non_wage_income",
  "ing_gov_transfers_nom","ing_gov_transfers","ing_rentas_nom","ing_rentas",
  "ing_fin_capital_nom","ing_fin_capital","ing_negocio_nom","ing_negocio",
  "ing_ventas_nom","ing_ventas","ing_other_nom","ing_other",
  "ing_lab_nom","ing_lab","ing_mon_nom","ing_mon",
  "lnw","lnnwi","lngt","lnr","lnfc","lnn","lnv","lno","lni","lnmon",
  "deflator"
)
trail <- setdiff(names(hh_month), ordered_cols)
hh_month <- as.data.frame(hh_month)[, c(intersect(ordered_cols, names(hh_month)), trail)]

write_dta_safe(hh_month, "../../data/clean/enigh/enigh-household-month.dta")
message(sprintf("Saved enigh-household-month.dta — N = %d HH-month obs, vars = %d",
                nrow(hh_month), ncol(hh_month)))
