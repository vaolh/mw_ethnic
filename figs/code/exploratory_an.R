
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

years <- c(2016, 2018, 2020, 2022, 2024)

for (i in years) {
  message(format(Sys.time(), "%H:%M:%S"), " | Running year ", i)
  
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
  
  # trabajos 
  tra_path <- sprintf("data/enigh/input/trabajos%d.dta", i)
  tra_dt <- as.data.table(read_dta(tra_path)) %>% rename_all(tolower)
  tra_dt[tra_dt == ""] <- NA
  
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
  ing_dt[ing_dt == ""] <- NA
  
  # Deflating incomes ---------------------------------------------------------
  
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
  
  ing_dt <-  left_join(ing_dt, tra_def, by = c("folioviv", "foliohog", "numren")) %>%
    mutate(index=(case_when(clave=="P009" & aguinaldo1!=1 ~ 1,
                            clave=="P016" & aguinaldo2!=1 ~ 1,
                            TRUE ~ 0)))
  
  ing_dt <-mutate(ing_dt, 
               ing_6=ifelse(is.na(mes_6), ing_6,
                            case_when(mes_6=="02"  ~ ing_6/feb_i,
                                      mes_6=="03"  ~ ing_6/mar_i,
                                      mes_6=="04"  ~ ing_6/apr_i,
                                      mes_6=="05"  ~ ing_6/may_i)),
               ing_5=ifelse(is.na(mes_5), ing_5,
                            case_when(mes_5=="03"  ~ ing_5/mar_i,
                                      mes_5=="04"  ~ ing_5/apr_i,
                                      mes_5=="05"  ~ ing_5/may_i,
                                      mes_5=="06"  ~ ing_5/jun_i)),
               ing_4=ifelse(is.na(mes_4), ing_4,
                            case_when(mes_4=="04"  ~ ing_4/apr_i,
                                      mes_4=="05"  ~ ing_4/may_i,
                                      mes_4=="06"  ~ ing_4/jun_i,
                                      mes_4=="07"  ~ ing_4/jul_i)),
               ing_3=ifelse(is.na(mes_3), ing_3,
                            case_when(mes_3=="05"  ~ ing_3/may_i,
                                      mes_3=="06"  ~ ing_3/jun_i,
                                      mes_3=="07"  ~ ing_3/jul_i,
                                      mes_3=="08"  ~ ing_3/ago_i)),
               ing_2=ifelse(is.na(mes_2), ing_2,
                            case_when(mes_2=="06"  ~ ing_2/jun_i,
                                      mes_2=="07"  ~ ing_2/jul_i,
                                      mes_2=="08"  ~ ing_2/ago_i,
                                      mes_2=="09"  ~ ing_2/sep_i)),
               ing_1=ifelse(is.na(mes_1), ing_1,
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
  
  # For estimation at the household level, we aggregate the income variables by folioviv and foliohog
  vars <- colnames(select(ing_dt, ing_mon, ing_lab, ing_ren, ing_tra))
  ing_dt <- data.table(ing_dt)[, lapply(.SD, sum, na.rm=T), by=list(folioviv, foliohog, numren), .SDcols = vars ]
  
  # Data  Import II --------------------------------------------------------------
  
  # poblacion
  pop_path <- sprintf("data/enigh/input/poblacion%d.dta", i)
  pop_dt <- as.data.table(read_dta(pop_path)) %>%
    dplyr::select(folioviv, foliohog, numren, etnia, nivelaprob, gradoaprob) %>%
    mutate(
      años_estudio = NA_real_,
      años_estudio = case_when(
        nivelaprob %in% c("0", "1") ~ 0,
        nivelaprob == "2" & gradoaprob == "1" ~ 1,
        nivelaprob == "2" & gradoaprob == "2" ~ 2,
        nivelaprob == "2" & gradoaprob == "3" ~ 3,
        nivelaprob == "2" & gradoaprob == "4" ~ 4,
        nivelaprob == "2" & gradoaprob == "5" ~ 5,
        nivelaprob == "2" & gradoaprob == "6" ~ 6,
        nivelaprob == "3" & gradoaprob == "1" ~ 7,
        nivelaprob == "3" & gradoaprob == "2" ~ 8,
        nivelaprob == "3" & gradoaprob == "3" ~ 9,
        nivelaprob == "4" & gradoaprob == "1" ~ 10,
        nivelaprob == "4" & gradoaprob == "2" ~ 11,
        nivelaprob == "4" & gradoaprob == "3" ~ 12,
        nivelaprob == "5" & gradoaprob == "1" ~ 13,
        nivelaprob == "5" & gradoaprob == "2" ~ 14,
        nivelaprob == "5" & gradoaprob == "3" ~ 15,
        nivelaprob == "5" & gradoaprob == "4" ~ 16,
        nivelaprob == "5" & gradoaprob == "5" ~ 17,
        nivelaprob == "6" & gradoaprob == "1" ~ 13,
        nivelaprob == "6" & gradoaprob == "2" ~ 14,
        nivelaprob == "6" & gradoaprob == "3" ~ 15,
        nivelaprob == "6" & gradoaprob == "4" ~ 16,
        nivelaprob == "6" & gradoaprob == "5" ~ 17,
        nivelaprob == "7" & gradoaprob == "1" ~ 13,
        nivelaprob == "7" & gradoaprob == "2" ~ 14,
        nivelaprob == "7" & gradoaprob == "3" ~ 15,
        nivelaprob == "7" & gradoaprob == "4" ~ 16,
        nivelaprob == "7" & gradoaprob == "5" ~ 17,
        nivelaprob == "8" & gradoaprob == "1" ~ 18,
        nivelaprob == "8" & gradoaprob == "2" ~ 19,
        nivelaprob == "8" & gradoaprob == "3" ~ 20,
        nivelaprob == "9" & gradoaprob == "1" ~ 21,
        nivelaprob == "9" & gradoaprob == "2" ~ 22,
        nivelaprob == "9" & gradoaprob == "3" ~ 23,
        nivelaprob == "9" & gradoaprob == "4" ~ 24,
        nivelaprob == "9" & gradoaprob == "5" ~ 25,
        TRUE ~ NA_real_
      )
    )
  
  # trabajos selection
  tra_dt <- tra_dt %>% 
    filter(id_trabajo == 1) %>%
    mutate(sector = str_sub(scian, 1, 2)) %>%
    dplyr::select(folioviv, foliohog, numren, htrab, 
                  scian, tam_emp, clas_emp, sector) 
  
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
  
  # agem
  
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
           ZNLF = ifelse(ubica_geo %in% ZNLF, 1, 0))
    
}




