#################################################
################ Shared Helpers #################
#################################################

### REPLICATION FILE: _helpers.R
### R VERSION:        4.5+
### AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
### DATE:             2026-05-02

### Sourced via `source("_helpers.R")` from every R build script.
### Provides:
###   - load_deflators()      -> data.frame of monthly INPC deflators (base Aug 2024)
###   - zlfn_municipalities() -> character vector of 40 ZLFN 5-digit codes
###   - apply_zlfn(df)        -> add `zlfn` column (1/0) from ubica_geo
###   - apply_state_region(df)-> add state, ent_name, reg_num, reg_name, macro_num, macro_name
###   - clean_ubica_geo(df,y) -> normalize to 5-digit string (2016 9-digit truncated)
###   - build_years_of_study()-> derive years_of_study from nivelaprob/gradoaprob
###   - classify_clave(df)    -> add clave_group from clave (P001-P108)
###   - rename_benefits(df,y) -> rename pres_* to readable benefit names
###   - recode_benefit_indicators(df) -> 0/1 binaries for all benefit*1, *2 columns

suppressPackageStartupMessages({
  library(dplyr)
  library(data.table)
  library(stringr)
  library(haven)
})

INPC_BASE_LABEL <- "August 2024"
DEFLATORS_PATH  <- "../../data/clean/inpc/inpc.csv"

#################################################
################ INPC Deflators #################
#################################################

load_deflators <- function(path = DEFLATORS_PATH) {
  ### Returns a tibble with columns: year, month, time (Date), deflator.
  ### Deflator is INPC[t] / INPC[Aug 2024] (so multiplying nominal by 1/deflator
  ### gives real Aug-2024 pesos).
  dt <- data.table::fread(path)
  data.table::setnames(dt,
    old = c("Año_int", "Mes", "Deflator"),
    new = c("year",    "month", "deflator"))
  dt[, time := as.Date(sprintf("%d-%02d-01", year, month))]
  dt[, c("year", "month", "time", "deflator")]
}

#################################################
############### ZLFN Municipalities #############
#################################################

zlfn_municipalities <- function() {
  c(
    ### Baja California (5 munis)
    "02001","02002","02003","02004","02005",
    ### Coahuila (8 munis)
    "05002","05012","05013","05014","05022","05023","05025","05038",
    ### Chihuahua (8 munis)
    "08005","08015","08028","08035","08037","08042","08052","08053",
    ### Nuevo León (1 muni)
    "19005",
    ### Sonora (11 munis)
    "26002","26004","26017","26019","26039","26043","26048","26055",
    "26059","26060","26070",
    ### Tamaulipas (10 munis)
    "28007","28014","28015","28022","28024","28025","28027",
    "28032","28033","28040"
  )
}

apply_zlfn <- function(df, ugeo_col = "ubica_geo") {
  ### Adds zlfn (integer 1/0) from ubica_geo (string, 5-digit).
  munis <- zlfn_municipalities()
  df[["zlfn"]] <- as.integer(df[[ugeo_col]] %in% munis)
  df
}

#################################################
############### ubica_geo Cleanup ###############
#################################################

clean_ubica_geo <- function(df, year_int) {
  ### Normalizes ubica_geo to a 5-digit string (state + municipality).
  ### 2016 ENIGH stores 9-digit codes (state+muni+locality); truncate to first 5.
  ### Other years already store 5-digit codes.
  df[["ubica_geo"]] <- as.character(df[["ubica_geo"]])
  ### Drop trailing decimal artifacts from numeric→character conversion.
  df[["ubica_geo"]] <- sub("\\..*$", "", df[["ubica_geo"]])
  ### Truncate any code longer than 5 chars to first 5.
  long_idx <- nchar(df[["ubica_geo"]]) > 5
  df[["ubica_geo"]][long_idx] <- substr(df[["ubica_geo"]][long_idx], 1, 5)
  ### Pad shorter codes with leading zeros (defensive).
  short_idx <- nchar(df[["ubica_geo"]]) == 4
  df[["ubica_geo"]][short_idx] <- paste0("0", df[["ubica_geo"]][short_idx])
  df
}

#################################################
################ State / Region #################
#################################################

state_table <- function() {
  data.frame(
    state = 1:32,
    ent_name = c(
      "Aguascalientes","Baja California","Baja California Sur","Campeche",
      "Coahuila","Colima","Chiapas","Chihuahua","Ciudad de México",
      "Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","México",
      "Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla",
      "Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora",
      "Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas"
    ),
    stringsAsFactors = FALSE
  )
}

apply_state_region <- function(df, ugeo_col = "ubica_geo") {
  ### Adds state, ent_name, reg_num, reg_name, macro_num, macro_name.
  df[["state"]] <- as.integer(substr(df[[ugeo_col]], 1, 2))

  st <- state_table()
  df[["ent_name"]] <- st$ent_name[match(df[["state"]], st$state)]

  region_map <- function(s) {
    dplyr::case_when(
      s %in% c(26, 25, 2, 3, 18)             ~ 1L,
      s %in% c(5, 8, 10, 32, 24)             ~ 2L,
      s %in% c(28, 19)                       ~ 3L,
      s %in% c(1, 14, 11, 6, 16)             ~ 4L,
      s %in% c(22, 15, 9, 13, 17, 29, 21)    ~ 5L,
      s %in% c(12, 20, 7)                    ~ 6L,
      s %in% c(27, 30)                       ~ 7L,
      s %in% c(4, 23, 31)                    ~ 8L,
      TRUE                                   ~ NA_integer_
    )
  }
  df[["reg_num"]] <- region_map(df[["state"]])
  df[["reg_name"]] <- dplyr::case_when(
    df[["reg_num"]] == 1 ~ "Northwest",
    df[["reg_num"]] == 2 ~ "North",
    df[["reg_num"]] == 3 ~ "Northeast",
    df[["reg_num"]] == 4 ~ "Center-West",
    df[["reg_num"]] == 5 ~ "Center-East",
    df[["reg_num"]] == 6 ~ "South",
    df[["reg_num"]] == 7 ~ "East",
    df[["reg_num"]] == 8 ~ "Peninsula",
    TRUE ~ NA_character_
  )

  df[["macro_num"]] <- dplyr::case_when(
    df[["reg_num"]] %in% 1:3 ~ 1L,
    df[["reg_num"]] %in% 4:5 ~ 2L,
    df[["reg_num"]] == 6     ~ 3L,
    df[["reg_num"]] %in% 7:8 ~ 4L,
    TRUE                     ~ NA_integer_
  )
  df[["macro_name"]] <- dplyr::case_when(
    df[["macro_num"]] == 1 ~ "Northern",
    df[["macro_num"]] == 2 ~ "Central",
    df[["macro_num"]] == 3 ~ "South",
    df[["macro_num"]] == 4 ~ "Eastern",
    TRUE ~ NA_character_
  )
  df
}

#################################################
############### Years of Study ##################
#################################################

build_years_of_study <- function(niv, grd) {
  ### Derives years_of_study (0-25) from nivelaprob and gradoaprob.
  niv <- suppressWarnings(as.numeric(as.character(niv)))
  grd <- suppressWarnings(as.numeric(as.character(grd)))
  dplyr::case_when(
    niv %in% c(0, 1)        ~ 0,
    niv == 2                ~ pmin(grd, 6, na.rm = FALSE),
    niv == 3                ~ 6  + pmin(grd, 3, na.rm = FALSE),
    niv == 4                ~ 9  + pmin(grd, 3, na.rm = FALSE),
    niv %in% c(5, 6, 7)     ~ 12 + pmin(grd, 5, na.rm = FALSE),
    niv == 8                ~ 17 + pmin(grd, 3, na.rm = FALSE),
    niv == 9                ~ 20 + pmin(grd, 5, na.rm = FALSE),
    TRUE                    ~ NA_real_
  )
}

#################################################
############ Income Classification ##############
#################################################

classify_clave <- function(df) {
  ### Adds clave_group from clave (P001-P108) — must match _helpers.do exactly.
  df[["clave_group"]] <- dplyr::case_when(
    df[["clave"]] %in% c("P001","P002","P011","P018","P019","P067") ~ "wages",
    df[["clave"]] %in% c("P003","P004","P005","P006","P007","P008",
                         "P009","P014","P015","P016") ~ "non_wage_income",
    df[["clave"]] %in% c("P032","P033","P038","P040","P042","P043","P044","P045",
                         "P046","P047","P048","P101","P102","P103","P104","P105",
                         "P106","P107","P108") ~ "gov_transfers",
    df[["clave"]] %in% c("P023","P024","P025") ~ "rentas",
    df[["clave"]] %in% c("P026","P027","P028","P029","P030","P031","P050","P052",
                         "P053","P064","P065","P066") ~ "fin_capital",
    df[["clave"]] %in% c("P068","P069","P070","P071","P072","P073","P074","P075",
                         "P076","P077","P078","P079","P080","P081") ~ "negocio",
    df[["clave"]] %in% c("P054","P055","P056","P059","P060","P061","P062","P063") ~ "ventas",
    df[["clave"]] %in% c("P012","P013","P020","P021","P022","P034","P035","P036",
                         "P037","P039","P041","P049","P051","P057","P058") ~ "other",
    TRUE ~ NA_character_
  )
  df
}

#################################################
################ Benefit Recoding ###############
#################################################

benefit_names <- function() {
  c("incapacidad","aguinaldo","vacaciones","utilidades","credito_vivienda",
    "guarderias","cuidados_parentales","sar_afore","seguro_vida","prestamos",
    "prima_vacacional","becas","comedor","fonacot","despensa","servicios_publicos",
    "pension_invalidez","pension_familia","otras_prestaciones","sin_prestaciones")
}

rename_benefits <- function(df, year_int) {
  ### Renames pres_* benefit indicators to readable names.
  ### 2016: pres_1-6 are medical institution codes; benefits start at pres_7.
  ### 2018+: benefits start at pres_1.
  bn <- benefit_names()
  if (year_int == 2016) {
    drop_cols <- paste0("pres_", 1:6)
    df <- df[, !names(df) %in% drop_cols, drop = FALSE]
    src <- paste0("pres_", 7:26)
  } else {
    src <- paste0("pres_", 1:20)
  }
  present <- intersect(src, names(df))
  ### Rename in the order in which they appear in src.
  for (i in seq_along(src)) {
    if (src[i] %in% names(df)) {
      names(df)[names(df) == src[i]] <- bn[i]
    }
  }
  df
}

recode_benefit_indicators <- function(df) {
  ### Recodes benefit*1, benefit*2 columns from raw response codes to 0/1.
  ### Any positive non-missing value -> 1; zero or missing -> 0.
  bn <- benefit_names()
  for (b in bn) {
    for (j in c("1", "2")) {
      col <- paste0(b, j)
      if (col %in% names(df)) {
        x <- suppressWarnings(as.numeric(as.character(df[[col]])))
        x[is.na(x)] <- 0
        df[[col]] <- as.integer(x > 0)
      }
    }
  }
  df
}

#################################################
########## Stata-friendly write helper ##########
#################################################

write_dta_safe <- function(df, path) {
  ### Strips haven_labelled classes and converts factors to native types
  ### so haven::write_dta produces a Stata-compatible .dta with no surprises.
  for (col in names(df)) {
    if (inherits(df[[col]], "haven_labelled")) {
      df[[col]] <- as.vector(df[[col]])
    }
    if (is.factor(df[[col]])) {
      lvls <- levels(df[[col]])
      num_try <- suppressWarnings(as.numeric(lvls))
      if (all(!is.na(num_try))) {
        df[[col]] <- as.numeric(as.character(df[[col]]))
      } else {
        df[[col]] <- as.character(df[[col]])
      }
    }
  }
  haven::write_dta(df, path)
}
