#################################################
################# Clean Memory ##################
#################################################

rm(list = ls())
options(scipen = 999)

### REPLICATION FILE: enigh-indlevel-month.R
### R VERSION:        4.5+
### AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
### DATE:             2026-05-02

### Builds the individual × month ENIGH panel for years 2016–2024.
### Parity port of enigh-indlevel-month.do — must produce the same variable list,
### same N, and per-variable summary statistics within 1e-6 (validated by
### compare-builds.R).
###
### Output: ../../data/clean/enigh/enigh-indlevel-month.dta

#################################################
################ Load + Helpers #################
#################################################

if (!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, tidyr, data.table, haven, stringr, purrr)

source("_helpers.R")

deflators <- load_deflators()

YEARS <- c(2016, 2018, 2020, 2022, 2024)

#################################################
########### Per-year build function #############
#################################################

build_year_month <- function(year_int) {
  message(sprintf("[%s] Processing ENIGH %d …",
                  format(Sys.time(), "%H:%M:%S"), year_int))

  base <- "../../data/source/enigh"

  ### Poblacion (individual demographics)
  pop <- read_dta(file.path(base, sprintf("poblacion%d.dta", year_int))) |>
    rename_with(tolower) |>
    select(folioviv, foliohog, numren, sexo, edad, parentesco,
           hablaind, comprenind, etnia,
           madre_hog, padre_hog, asis_esc,
           nivelaprob, gradoaprob, hor_1, trabajo_mp, num_trabaj) |>
    mutate(
      sexo       = suppressWarnings(as.numeric(as.character(sexo))),
      edad       = suppressWarnings(as.numeric(as.character(edad))),
      parentesco = suppressWarnings(as.numeric(as.character(parentesco))),
      nivelaprob = suppressWarnings(as.numeric(as.character(nivelaprob))),
      gradoaprob = suppressWarnings(as.numeric(as.character(gradoaprob))),
      hor_1      = suppressWarnings(as.numeric(as.character(hor_1)))
    ) |>
    mutate(
      gender             = dplyr::case_when(sexo == 1 ~ 1L, sexo == 2 ~ 0L,
                                            TRUE ~ NA_integer_),
      etnia              = dplyr::case_when(etnia == "1" ~ 1L, etnia == "2" ~ 0L,
                                            TRUE ~ NA_integer_),
      indspeaker         = dplyr::case_when(hablaind == "1" ~ 1L,
                                            hablaind == "2" ~ 0L,
                                            TRUE ~ NA_integer_),
      indund             = dplyr::case_when(comprenind == "1" ~ 1L,
                                            comprenind == "2" ~ 0L,
                                            TRUE ~ NA_integer_),
      school_attendance  = dplyr::case_when(asis_esc == "1" ~ 1L,
                                            asis_esc == "2" ~ 0L,
                                            TRUE ~ NA_integer_),
      motherhome         = dplyr::case_when(madre_hog == "1" ~ 1L,
                                            madre_hog == "2" ~ 0L,
                                            TRUE ~ NA_integer_),
      fatherhome         = dplyr::case_when(padre_hog == "1" ~ 1L,
                                            padre_hog == "2" ~ 0L,
                                            TRUE ~ NA_integer_),
      employed           = dplyr::case_when(trabajo_mp == "1" ~ 1L,
                                            trabajo_mp == "2" ~ 0L,
                                            TRUE ~ NA_integer_),
      hoursworked        = hor_1,
      years_of_study     = build_years_of_study(nivelaprob, gradoaprob)
    ) |>
    select(-sexo, -hablaind, -comprenind, -asis_esc, -madre_hog, -padre_hog,
           -trabajo_mp, -nivelaprob, -gradoaprob, -hor_1)

  ### Concentradohogar (HH weight, ubica_geo, smg)
  hog <- read_dta(file.path(base, sprintf("concentradohogar%d.dta", year_int))) |>
    rename_with(tolower) |>
    select(folioviv, ubica_geo, factor, smg) |>
    distinct(folioviv, .keep_all = TRUE) |>
    mutate(
      factor = suppressWarnings(as.numeric(as.character(factor))),
      smg    = suppressWarnings(as.numeric(as.character(smg)))
    )
  hog <- clean_ubica_geo(hog, year_int)

  ### Viviendas (housing + survey design)
  viv_cols <- c("folioviv","tipo_viv","mat_pared","mat_techos","mat_pisos",
                "antiguedad","cuart_dorm","num_cuarto","disp_agua","dotac_agua",
                "excusado","disp_elect","combustible","eli_basura",
                "tenencia","renta","estim_pago","pago_viv",
                "tot_resid","tot_hom","tot_muj","tot_hog","tam_loc","est_socio",
                "est_dis","upm")
  viv <- read_dta(file.path(base, sprintf("viviendas%d.dta", year_int))) |>
    rename_with(tolower)
  viv <- viv[, intersect(viv_cols, names(viv)), drop = FALSE]
  viv <- viv |> distinct(folioviv, .keep_all = TRUE)
  ### Convert all selected viviendas vars to numeric where possible.
  for (col in setdiff(names(viv), "folioviv")) {
    x <- suppressWarnings(as.numeric(as.character(viv[[col]])))
    if (sum(is.na(x)) <= sum(is.na(viv[[col]]))) viv[[col]] <- x
  }

  ### Trabajos (jobs and benefits)
  tra <- read_dta(file.path(base, sprintf("trabajos%d.dta", year_int))) |>
    rename_with(tolower)
  tra <- rename_benefits(tra, year_int)
  ### Drop tipo_trab/ocupa if present (inconsistent across years).
  tra[, c("tipo_trab","ocupa")] <- NULL

  bn <- benefit_names()
  by_cols <- c("folioviv","foliohog","numren","id_trabajo",
               "scian","sinco","subor","indep","personal","pago",
               "contrato","tipocontr", bn)
  by_cols <- intersect(by_cols, names(tra))
  ### Sum hours by the worker × job × scian × benefit-pattern key.
  tra_dt <- as.data.table(tra)
  if ("htrab" %in% names(tra_dt)) {
    tra_dt[, htrab := suppressWarnings(as.numeric(as.character(htrab)))]
  } else {
    tra_dt[, htrab := NA_real_]
  }
  tra_dt <- tra_dt[, .(htrab = sum(htrab, na.rm = TRUE)),
                    by = c(by_cols)]
  ### Reshape wide to job 1 / job 2.
  tra_dt[, id_trabajo := as.character(id_trabajo)]
  pivot_cols <- intersect(c("htrab","scian","sinco","subor","indep","personal",
                            "pago","contrato","tipocontr", bn),
                          names(tra_dt))
  tra_wide <- dcast(tra_dt,
    folioviv + foliohog + numren ~ id_trabajo,
    value.var = pivot_cols,
    fun.aggregate = function(x) if (length(x) == 0) NA else x[1])
  ### Standardize column suffix to "1"/"2": dcast names them e.g. htrab_1.
  setnames(tra_wide, names(tra_wide),
           gsub("_(1|2)$", "\\1", names(tra_wide)))
  ### Industry codes from SCIAN.
  tra_wide[, ind1 := suppressWarnings(as.integer(substr(scian1, 1, 2)))]
  tra_wide[, ind2 := suppressWarnings(as.integer(substr(scian2, 1, 2)))]
  setnames(tra_wide, c("htrab1","htrab2"), c("hours1","hours2"),
           skip_absent = TRUE)
  ### Recode benefits to 0/1 and 0/1 indicators for subor, indep, etc.
  tra_wide <- recode_benefit_indicators(as.data.frame(tra_wide))
  for (base_col in c("subor","indep","personal","pago","contrato","tipocontr")) {
    for (j in c("1","2")) {
      col <- paste0(base_col, j)
      if (col %in% names(tra_wide)) {
        x <- suppressWarnings(as.numeric(as.character(tra_wide[[col]])))
        x_recoded <- ifelse(x == 1, 1L,
                     ifelse(x %in% c(2, 3), 0L, NA_integer_))
        tra_wide[[col]] <- x_recoded
      }
    }
  }

  ### Gastospersona (personal expenditure aggregates)
  gp <- read_dta(file.path(base, sprintf("gastospersona%d.dta", year_int))) |>
    rename_with(tolower) |>
    group_by(folioviv, foliohog, numren) |>
    summarise(
      gaspers_tri  = sum(suppressWarnings(as.numeric(gasto_tri)),  na.rm = TRUE),
      gaspers_ntri = sum(suppressWarnings(as.numeric(gas_nm_tri)), na.rm = TRUE),
      .groups = "drop"
    )

  ### Ingresos (income panel)
  ing <- read_dta(file.path(base, sprintf("ingresos%d.dta", year_int))) |>
    rename_with(tolower) |>
    classify_clave() |>
    filter(!is.na(clave_group), clave_group != "")

  ### Pivot to long: one row per (person × clave × slot).
  ing_long <- ing |>
    select(folioviv, foliohog, numren, clave, clave_group,
           starts_with("ing_"), starts_with("mes_")) |>
    select(-any_of(c("ing_tri"))) |>
    pivot_longer(
      cols = matches("^(ing|mes)_[1-6]$"),
      names_to = c(".value","slot"),
      names_pattern = "(ing|mes)_([1-6])"
    ) |>
    mutate(
      mes = trimws(as.character(mes)),
      mes = ifelse(mes == "", NA_character_, mes),
      month = suppressWarnings(as.integer(mes))
    ) |>
    filter(!is.na(month), month >= 1, month <= 12) |>
    mutate(ing = suppressWarnings(as.numeric(ing)))

  ### Aggregate to (person × clave_group × month).
  ing_pm <- ing_long |>
    group_by(folioviv, foliohog, numren, clave_group, month) |>
    summarise(ing_nom = sum(ing, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(
      names_from = clave_group,
      values_from = ing_nom,
      names_prefix = "ing_",
      names_glue = "ing_{clave_group}_nom",
      values_fill = 0
    )

  ### Ensure all 8 group columns exist (fill missing with 0).
  for (grp in c("wages","non_wage_income","gov_transfers","rentas",
                "fin_capital","negocio","ventas","other")) {
    col <- paste0("ing_", grp, "_nom")
    if (!col %in% names(ing_pm)) ing_pm[[col]] <- 0
  }

  ### Merge per-year pieces
  yr <- ing_pm |>
    left_join(pop,      by = c("folioviv","foliohog","numren")) |>
    left_join(as.data.frame(tra_wide),
                       by = c("folioviv","foliohog","numren")) |>
    left_join(gp,       by = c("folioviv","foliohog","numren")) |>
    left_join(viv,      by = "folioviv") |>
    left_join(hog,      by = "folioviv") |>
    mutate(year = year_int)

  yr
}

#################################################
################ Stack all years ################
#################################################

panel <- bind_rows(lapply(YEARS, build_year_month))

#################################################
################ Calendar time ##################
#################################################

panel <- panel |>
  mutate(
    time = as.Date(sprintf("%04d-%02d-01", year, month))
  )

#################################################
################ Merge deflators ################
#################################################

panel <- panel |>
  left_join(deflators |> select(time, deflator), by = "time") |>
  filter(!is.na(deflator))

#################################################
########## Real income & log income #############
#################################################

panel <- panel |>
  mutate(
    ing_lab_nom = ing_wages_nom + ing_non_wage_income_nom,
    ing_mon_nom = ing_wages_nom + ing_non_wage_income_nom +
                  ing_gov_transfers_nom + ing_rentas_nom +
                  ing_fin_capital_nom + ing_negocio_nom +
                  ing_ventas_nom + ing_other_nom
  )

income_bases <- c("wages","non_wage_income","gov_transfers","rentas",
                  "fin_capital","negocio","ventas","other","lab","mon")
for (b in income_bases) {
  panel[[paste0("ing_", b)]] <- panel[[paste0("ing_", b, "_nom")]] / panel$deflator
}

mk_log <- function(x) ifelse(x > 0 & !is.na(x), log(x), NA_real_)
panel <- panel |>
  mutate(
    lnw   = mk_log(ing_wages),
    lnnwi = mk_log(ing_non_wage_income),
    lngt  = mk_log(ing_gov_transfers),
    lnr   = mk_log(ing_rentas),
    lnfc  = mk_log(ing_fin_capital),
    lnn   = mk_log(ing_negocio),
    lnv   = mk_log(ing_ventas),
    lno   = mk_log(ing_other),
    lni   = mk_log(ing_lab),
    lnmon = mk_log(ing_mon)
  )

#################################################
########## Treatment & geography ################
#################################################

panel <- apply_state_region(panel)
panel <- apply_zlfn(panel)

panel <- panel |>
  mutate(
    post       = as.integer(year > 2018),
    treat_post = as.integer(zlfn * post),
    edad_pob   = edad,
    edadsq     = edad * edad,
    female     = 1L - gender
  )

#################################################
############### Sample restriction ##############
#################################################

panel <- panel |>
  filter(!is.na(time), !is.na(edad), edad > 12)

#################################################
################ Order & save ###################
#################################################

ordered_cols <- c(
  "folioviv","foliohog","numren","year","month","time",
  "ubica_geo","state","ent_name","reg_num","reg_name","macro_num","macro_name",
  "tam_loc","factor","upm","est_dis",
  "zlfn","post","treat_post",
  "gender","female","edad","edad_pob","edadsq","years_of_study","hoursworked",
  "employed","school_attendance","motherhome","fatherhome",
  "etnia","indspeaker","indund",
  "ing_wages_nom","ing_wages","ing_non_wage_income_nom","ing_non_wage_income",
  "ing_gov_transfers_nom","ing_gov_transfers","ing_rentas_nom","ing_rentas",
  "ing_fin_capital_nom","ing_fin_capital","ing_negocio_nom","ing_negocio",
  "ing_ventas_nom","ing_ventas","ing_other_nom","ing_other",
  "ing_lab_nom","ing_lab","ing_mon_nom","ing_mon",
  "lnw","lnnwi","lngt","lnr","lnfc","lnn","lnv","lno","lni","lnmon",
  "deflator"
)

trail <- setdiff(names(panel), ordered_cols)
panel <- panel[, c(ordered_cols, trail)]

write_dta_safe(panel, "../../data/clean/enigh/enigh-indlevel-month.dta")
message(sprintf("Saved enigh-month.dta — N = %d, vars = %d",
                nrow(panel), ncol(panel)))
