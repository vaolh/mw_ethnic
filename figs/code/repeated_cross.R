
rm(list = ls())
options(scipen = 999)

if (!require(pacman)) install.packages("pacman") 
p_load("dplyr","ggplot2", "rstudioapi", 
       "dineq", "corrplot", "viridis", "haven", 
       "data.table", "stringr", "zoo", "data.table", 
       "tidyverse", "gdata", "srvyr", "bit64"
)


setwd("/Users/mati/Documents/GitHub/mw_ethnic"); getwd()

# Loop ------------------------------------------------------------------------

years <- c(2016,2018,2020,2022,2024)

cross_list <- list()

for (i in years) {
  message(format(Sys.time(), "%H:%M:%S"), " | Running year ", i)
  year <- years[i]
  
  # Data  Import I --------------------------------------------------------------
  
  # deflators  
  deflators <- fread("data/deflators/output/inpc.csv") %>%
    rename(year = Año_int, month = Mes, deflator = Deflator) %>%
    select(-Fecha) 
  
  deflators_i <- deflators %>%
    filter(year == i | (year == i-1 & month == 12)) %>%
    arrange(desc(year))
  
  {
    dec_im1 <- as.numeric(deflators_i[13,4])
    jan_i <- as.numeric(deflators_i[12,4])
    feb_i <- as.numeric(deflators_i[11,4])
    mar_i <- as.numeric(deflators_i[10,4])
    apr_i <- as.numeric(deflators_i[9,4])
    may_i <- as.numeric(deflators_i[8,4])
    jun_i <- as.numeric(deflators_i[7,4])
    jul_i <- as.numeric(deflators_i[6,4])
    ago_i <- as.numeric(deflators_i[5,4])
    sep_i <- as.numeric(deflators_i[4,4])
    oct_i <- as.numeric(deflators_i[3,4])
    nov_i <- as.numeric(deflators_i[2,4])
    dec_i <- as.numeric(deflators_i[1,4])
    }
  
  if (nrow(deflators_i) != 13) {
    stop(sprintf("deflators_i tiene %d filas (esperaba 13) para el año %d", nrow(deflators_i), i))
  } else {
    message("✓ Deflators rows check passed.")
  }
  
  # We check expected months (prev Dec + 1:12)
  expected_months <- c(12, 1:12) # 13 valores (prev Dec, then 1..12)
  actual_months <- as.integer(deflators_i$month)
  if (!all(expected_months %in% actual_months)) {
    warning("Los meses en deflators_i no son los esperados: ", paste(sort(unique(actual_months)), collapse=", "))
  } else {
    message("✓ Deflators months check passed.")
  }
  
  # trabajos 
  tra_path <- sprintf("data/enigh/input/trabajos%d.dta", i)
  tra_dt <- as.data.table(read_dta(tra_path)) %>% rename_all(tolower)

  if (i %in% c(2016, 2018)) {
    aguinaldo_var <- "pres_8"
    aguinaldo_code <- "08"
  } else {
    aguinaldo_var <- "pres_2"
    aguinaldo_code <- "02"
  }
  
  # ingresos 
  ing_path <- sprintf("data/enigh/input/ingresos%d.dta", i)
  ing_dt <- as.data.table(read_dta(ing_path)) %>% rename_all(tolower)

  # Deflating incomes ---------------------------------------------------------
  
  dup_check <- tra_dt[, .N, by=.(folioviv, foliohog, numren, id_trabajo)][N>1]
  if (nrow(dup_check) > 0) {
    stop("✗ Hay duplicados por (folio, id_trabajo). Revisa tra_dt. Ejemplo:", head(dup_check,5))
  } else {
    message("✓ No duplicates found in tra_dt for (folio, id_trabajo).")
  }
  
  tra_def <- tra_dt %>%
    dplyr::select(folioviv, foliohog, numren, id_trabajo, all_of(aguinaldo_var)) %>%
    rename(pres = all_of(aguinaldo_var)) %>%
    mutate(
      pres = as.character(pres)  
    ) %>%
    as.data.table() %>%
    dcast(folioviv + foliohog + numren ~ id_trabajo, value.var = "pres") %>%
    mutate(
      trab = 1,
      aguinaldo1 = case_when(`1` == aguinaldo_code ~ 1, TRUE ~ 0),
      aguinaldo2 = case_when(`2` == aguinaldo_code ~ 1, TRUE ~ 0)
    ) %>%
    select(folioviv, foliohog, numren, aguinaldo1, aguinaldo2, trab)
  
  # We join this bonus data to the income table
  
  n_before <- nrow(ing_dt)
  ing_dt2 <- left_join(ing_dt, tra_def, by = c("folioviv", "foliohog", "numren"))
  n_after <- nrow(ing_dt2)
  if (n_after != n_before) {
    warning(sprintf("✗ left_join va a cambiar el número de filas: %d -> %d (año %d). Revisa duplicados en tra_def.", n_before, n_after, i))
  }else {
    message("✓ Join rows, for ing-tra join check passed.")
  }
  
  ing_dt <-  full_join(ing_dt, tra_def, by = c("folioviv", "foliohog", "numren")) %>%
    mutate(index=(case_when(clave=="P009" & aguinaldo1!=1 ~ 1,
                            clave=="P016" & aguinaldo2!=1 ~ 1,
                            TRUE ~ 0))) %>%
    filter(index != 1)
  
  ing_dt <-mutate(ing_dt, 
               ing_6=ifelse(is.na(ing_dt$mes_6), ing_6,
                            case_when(mes_6=="02"  ~ ing_6/feb_i,
                                      mes_6=="03"  ~ ing_6/mar_i,
                                      mes_6=="04"  ~ ing_6/apr_i,
                                      mes_6=="05"  ~ ing_6/may_i)),
               ing_5=ifelse(is.na(ing_dt$mes_5), ing_5,
                            case_when(mes_5=="03"  ~ ing_5/mar_i,
                                      mes_5=="04"  ~ ing_5/apr_i,
                                      mes_5=="05"  ~ ing_5/may_i,
                                      mes_5=="06"  ~ ing_5/jun_i)),
               ing_4=ifelse(is.na(ing_dt$mes_4), ing_4,
                            case_when(mes_4=="04"  ~ ing_4/apr_i,
                                      mes_4=="05"  ~ ing_4/may_i,
                                      mes_4=="06"  ~ ing_4/jun_i,
                                      mes_4=="07"  ~ ing_4/jul_i)),
               ing_3=ifelse(is.na(ing_dt$mes_3), ing_3,
                            case_when(mes_3=="05"  ~ ing_3/may_i,
                                      mes_3=="06"  ~ ing_3/jun_i,
                                      mes_3=="07"  ~ ing_3/jul_i,
                                      mes_3=="08"  ~ ing_3/ago_i)),
               ing_2=ifelse(is.na(ing_dt$mes_2), ing_2,
                            case_when(mes_2=="06"  ~ ing_2/jun_i,
                                      mes_2=="07"  ~ ing_2/jul_i,
                                      mes_2=="08"  ~ ing_2/ago_i,
                                      mes_2=="09"  ~ ing_2/sep_i)),
               ing_1=ifelse(is.na(ing_dt$mes_1), ing_1,
                            case_when(mes_1=="07"  ~ ing_1/jul_i,
                                      mes_1=="08"  ~ ing_1/ago_i,
                                      mes_1=="09"  ~ ing_1/sep_i,
                                      mes_1=="10" ~ ing_1/oct_i))) 
  
  index <-c("P008", "P009", "P015", "P016") 
  
  ing_dt <- ing_dt %>%
    mutate(ing_1=ifelse(clave=="P008" | clave=="P015", (ing_1/may_i)/12, ing_1), 
           ing_1=ifelse(clave=="P009" | clave=="P016", (ing_1/dec_im1)/12, ing_1),
           ing_2=ifelse((clave %in%  index) & ing_2 == 0, NA_real_, ing_2),
           ing_3=ifelse((clave %in%  index) & ing_3 == 0, NA_real_, ing_3),
           ing_4=ifelse((clave %in%  index) & ing_4 == 0, NA_real_, ing_4),
           ing_5=ifelse((clave %in%  index) & ing_5 == 0, NA_real_, ing_5),
           ing_6=ifelse((clave %in%  index) & ing_6 == 0, NA_real_, ing_6))
  
  table(tra_dt[[aguinaldo_var]])
  
  # Creating income aggregates -------------------------------------------------
  
  ing_dt <- ing_dt %>% 
    mutate(ing_mens = apply(ing_dt[,c("ing_1","ing_2","ing_3",
                                   "ing_4","ing_5","ing_6")], 1, mean, na.rm=TRUE), 
           # Monetary Income
           ing_mon=case_when((clave>="P001" & clave<="P009") | (clave>="P011" & clave<="P016") |
                               (clave>="P018" & clave<="P048") | (clave>="P067" & clave<="P081") |
                               (clave>="P101" & clave<="P108") ~ ing_mens ),
           # Labor Income
           ing_lab=case_when((clave>="P001" & clave<="P009") | (clave>="P011" & clave<="P016") |
                               (clave>="P018" & clave<="P022") | (clave>="P067" & clave<="P081") ~ ing_mens ), 
           # Rents
           ing_ren=case_when((clave>="P023" & clave<="P031") ~ ing_mens ), 
           # Transfers 
           ing_tra=case_when((clave>="P032" & clave<="P048") | (clave>="P101" & clave<="P108") ~ ing_mens ))
  
  multi <- ing_dt[, .N, by=.(folioviv, foliohog, numren, clave)][N>1]
  if (nrow(multi) > 0) {
    warning("✗ Existen múltiples filas por (folio, numren, clave). Esto se sumará.", nrow(multi))
  }else {
    message("✓ No multiple rows found in ing_dt for (folio, numren, clave).")
  }
  
  # For estimation at the personal level
  vars <- colnames(select(ing_dt, ing_mon, ing_lab, ing_ren, ing_tra))
  ing_dt <- data.table(ing_dt)[, lapply(.SD, sum, na.rm=T), by=list(folioviv, foliohog, numren), .SDcols = vars ]
  
  if (sum(is.na(ing_dt$ing_mon)) > 0) {
    warning("✗ NA's found in ing_mon after aggregation.")
  } else {
    message("✓ No NA's found in income after aggregation.")
  }
  
  # Data  Import II --------------------------------------------------------------
  
  # poblacion
  pop_path <- sprintf("data/enigh/input/poblacion%d.dta", i)
  pop_dt <- as.data.table(read_dta(pop_path)) %>%
    dplyr::select(folioviv, foliohog, numren, etnia, nivelaprob)
  
  # trabajos selection
  tra_dt <- tra_dt %>% 
    filter(id_trabajo == 1) %>%
    mutate(sector = str_sub(scian, 1, 2)) %>%
    dplyr::select(folioviv, foliohog, numren, htrab, 
                  scian, tam_emp, clas_emp, sector, subor, indep) 
  
  # pobreza 
  pob_path <- sprintf("data/enigh/input/poverty/pobreza_%d.dta", i)
  pob_dt <- as.data.table(read_dta(pob_path)) %>% rename_all(tolower) %>%
    filter(pea == 1 | pea == 2) %>%
    mutate(hli = ifelse(is.na(hli), 2, hli),
           informal = ifelse((pea == 1) & (ss_dir == 0), 1, 0),
           ictpc = ictpc/ago_i,
           ict = ict/ago_i,
           nomon = nomon/ago_i,
           reg_esp = reg_esp/ago_i,
           pago_esp = pago_esp/ago_i,
           ) %>%
    dplyr::select(-ing_mon, -ing_lab, -ing_ren, -ing_tra)
  
  if (i %in% c(2020, 2022, 2024)) {
    pob_dt <- pob_dt %>% dplyr::select(-discap)
  } 
  
  # Cross sectional merge ------------------------------------------------------
  
  #income_vars <- c("ing_lab", "ing_mon", "ing_ren", "ing_tra", "ictpc", "ict")
  
  ZNLF <- c("02001", "02002", "02003", "02004", "02005", "02006", "02007", #Baja California
            "05002", "05012", "05013", "05014", "05022", "05023", "05025", "05038", #Coahuila
            "08005", "08015", "08028", "08035", "08037", "08042", "08052", "08053", #Chihuahua
            "19005", #Nuevo León
            "26002", "26004", "26017", "26019", "26039", "26043", "26048", "26055", 
            "26059", "26060", "26070", #Sonora 
            "28007", "28014", "28015", "28022", "28024", "28025", "28027", 
            "28032", "28033", "28043") #Tamaulipas 
  
  cross <- pob_dt %>%
    left_join(pop_dt, by = c("folioviv", "foliohog", "numren")) %>%
    left_join(tra_dt, by = c("folioviv", "foliohog", "numren")) %>%
    left_join(ing_dt, by = c("folioviv", "foliohog", "numren")) %>%
    mutate(year = i,
           ZNLF = ifelse(ubica_geo %in% ZNLF, 1, 0),
           ent_name = case_when(ent == 1  ~ "Aguascalientes",
                                ent == 2  ~ "Baja California",
                                ent == 3  ~ "Baja California Sur",
                                ent == 4  ~ "Campeche",
                                ent == 5  ~ "Coahuila",
                                ent == 6  ~ "Colima",
                                ent == 7  ~ "Chiapas",
                                ent == 8  ~ "Chihuahua",
                                ent == 9  ~ "Ciudad de México",
                                ent == 10 ~ "Durango",
                                ent == 11 ~ "Guanajuato",
                                ent == 12 ~ "Guerrero",
                                ent == 13 ~ "Hidalgo",
                                ent == 14 ~ "Jalisco",
                                ent == 15 ~ "México",
                                ent == 16 ~ "Michoacán",
                                ent == 17 ~ "Morelos",
                                ent == 18 ~ "Nayarit",
                                ent == 19 ~ "Nuevo León",
                                ent == 20 ~ "Oaxaca",
                                ent == 21 ~ "Puebla",
                                ent == 22 ~ "Querétaro",
                                ent == 23 ~ "Quintana Roo",
                                ent == 24 ~ "San Luis Potosí",
                                ent == 25 ~ "Sinaloa",
                                ent == 26 ~ "Sonora",
                                ent == 27 ~ "Tabasco",
                                ent == 28 ~ "Tamaulipas",
                                ent == 29 ~ "Tlaxcala",
                                ent == 30 ~ "Veracruz",
                                ent == 31 ~ "Yucatán",
                                ent == 32 ~ "Zacatecas"),
    reg_num = case_when(
    (ent == 26) | (ent == 25) | (ent == 02) | (ent == 03) | (ent == 18) ~ 1,
    (ent == 05) | (ent == 08) | (ent == 10) | (ent == 32) | (ent == 24) ~ 2,
    (ent == 28) | (ent == 19) ~ 3,
    (ent == 01) | (ent == 14) | (ent == 11) | (ent == 06) | (ent == 16) ~ 4,
    (ent == 22) | (ent == 15) | (ent == 09) | (ent == 13) | (ent == 17) | (ent == 29) | (ent == 21) ~ 5,
    (ent == 12) | (ent == 20) | (ent == 07) ~ 6,
    (ent == 27) | (ent == 30) ~ 7,
    (ent == 04) | (ent == 23) | (ent == 31) ~ 8),
    reg_name = case_when(
      reg_num == 1 ~ "Northwest",
      reg_num == 2 ~ "North",
      reg_num == 3 ~ "Northeast",
      reg_num == 4 ~ "Center-West",
      reg_num == 5 ~ "Center-East",
      reg_num == 6 ~ "South",
      reg_num == 7 ~ "East",
      reg_num == 8 ~ "Peninsula"),
    macro_num = case_when( 
      (ent == 26) | (ent == 25) | (ent == 02) | (ent == 03) | (ent == 18) | 
        (ent == 05) | (ent == 08) | (ent == 10) | (ent == 32) | (ent == 24) |
        (ent == 28) | (ent == 19) ~ 1,
      (ent == 01) | (ent == 14) | (ent == 11) | (ent == 06) | (ent == 16) |
        (ent == 22) | (ent == 15) | (ent == 09) | (ent == 13) | (ent == 17) | 
        (ent == 29) | (ent == 21) ~ 2,
      (ent == 12) | (ent == 20) | (ent == 07) ~ 3,
      (ent == 27) | (ent == 30) | (ent == 04) | (ent == 23) | (ent == 31) ~ 4),
    macro_name = case_when(
      macro_num == 1 ~ "Northern",
      macro_num == 2 ~ "Central",
      macro_num == 3 ~ "South",
      macro_num == 4 ~ "Eastern")) %>%
    filter(!if_any(all_of(vars), is.na))
    
    
  if (sum(is.na(cross$reg_num)) > 0 | sum(is.na(cross$macro_num)) > 0){
    warning("✗ Regional and/or Macroregional construction has NA's: ", n_cross, " >", n_pob)
  } else {
    message("✓ No NA's in the construction of regions.")
  }
  
  n_pob <- nrow(pob_dt)
  n_cross <- nrow(cross) # después del join
  if (n_cross > n_pob) {
    warning("✗ cross tiene más filas que pob_dt después de los joins: ", n_cross, " >", n_pob)
  } else {
    message("✓ Cross sectional left_join join check passed.")
  }
  
  # National deciles and centiles
  
  cross <- cross %>%
    mutate(deciles_ictpc = ntiles.wtd(x=ictpc, n=10, weights=factor),
           deciles_inglab = ntiles.wtd(x=ing_lab, n=10, weights=factor),
           centiles_ictpc = ntiles.wtd(x=ictpc, n=100, weights=factor),
           centiles_inglab = ntiles.wtd(x=ing_lab, n=100, weights=factor))
  
  if (any(
    is.na(cross$deciles_ictpc),
    is.na(cross$deciles_inglab),
    is.na(cross$centiles_ictpc),
    is.na(cross$centiles_inglab)
  )) {
    warning("✗ NA's found in decile/centile assignment")
  } else {
    message("✓ No NA's found in decile/centile assignment")
  }
  
  if (any(min(cross$deciles_ictpc) == 1 & max(cross$deciles_ictpc) == 10,
          min(cross$deciles_inglab) == 1 & max(cross$deciles_inglab) == 10,
          min(cross$centiles_ictpc) == 1 & max(cross$centiles_ictpc) == 100,
          min(cross$centiles_inglab) == 1 & max(cross$centiles_inglab) == 100
  )) {
    message("✓ Correct decile/centile ranges found")
  } else {
    message("✗ Incorrect decile/centile ranges found")
  }
  
  cross_list[[paste0("cross_section", i)]] <- cross
    
}

remove(list = ls()[!ls() %in% c("cross_list")])

cross_16 <- cross_list$cross_section2016
cross_18 <- cross_list$cross_section2018
cross_20 <- cross_list$cross_section2020
cross_22 <- cross_list$cross_section2022
cross_24 <- cross_list$cross_section2024


cross_final <- rbindlist(list(cross_16, cross_18, cross_20, cross_22, cross_24), 
                         use.names = TRUE, ignore.attr = T)


fwrite(cross_final, "data/enigh/output/cross_sectional_data.dta")

