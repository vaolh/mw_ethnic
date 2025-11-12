
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

years <- c(2016)

for (i in years) {
  
  ## Deflators, as in CONEVAL methodology -------------------------------------
  
  deflators <- fread("data/deflators/output/inpc.csv") %>%
    rename(year = Año_int, month = Mes, deflator = Deflator) %>%
    select(-Fecha) 
  
  deflators_i <- deflators %>%
    filter(year == i | (year == i-1 & month == 12))
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
  
  # Data  Import --------------------------------------------------------------
  
  # poblacion
  #pop_path <- sprintf("data/enigh/input/poblacion%d.dta", i)
  #pop_dt <- as.data.table(read_dta(pop_path))
  
  # concentradohogar 
  #hog_path <- sprintf("data/enigh/input/concentradohogar%d.dta", i)
  #hog_dt <- as.data.table(read_dta(hog_path))
  
  # trabajos 
  tra_path <- sprintf("data/enigh/input/trabajos%d.dta", i)
  tra_dt <- as.data.table(read_dta(tra_path)) %>% rename_all(tolower)
  
  if (i == 2016) {
    aguinaldo_var <- "pres_8"
    aguinaldo_code <- "08"
  } else {
    aguinaldo_var <- "pres_2"
    aguinaldo_code <- "02"
  }
  
  # ingresos 
  ing_path <- sprintf("data/enigh/input/ingresos%d.dta", i)
  ing_dt <- as.data.table(read_dta(ing_path))
  
  # pobreza 
  #pob_path <- sprintf("data/enigh/input/poverty/pobreza_%d.csv", i)
  #pob_dt <- as.data.table(read.csv(pob_path))
  
  # Deflating incomes ---------------------------------------------------------
  
  tra_def <- tra_dt %>%
    select(folioviv, foliohog, numren, id_trabajo, all_of(aguinaldo_var)) %>%
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
  
  ing_dt <-  full_join(ing_dt, tra_def, by = c("folioviv", "foliohog", "numren")) %>%
    mutate(index=(case_when(clave=="P009" & aguinaldo1!=1 ~ 1,
                            clave=="P016" & aguinaldo2!=1 ~ 1,
                            TRUE ~ 0))) %>%
    filter(index != 1)
  
  ing_dt <-mutate(ing_dt, 
               ing_6=ifelse(is.na(ing_dt$mes_6), ing_6,
                            case_when(mes_6==2  ~ ing_6/feb_i,
                                      mes_6==3  ~ ing_6/mar_i,
                                      mes_6==4  ~ ing_6/apr_i,
                                      mes_6==5  ~ ing_6/may_i)),
               ing_5=ifelse(is.na(ing_dt$mes_5), ing_5,
                            case_when(mes_5==3  ~ ing_5/mar_i,
                                      mes_5==4  ~ ing_5/apr_i,
                                      mes_5==5  ~ ing_5/may_i,
                                      mes_5==6  ~ ing_5/jun_i)),
               ing_4=ifelse(is.na(ing_dt$mes_4), ing_4,
                            case_when(mes_4==4  ~ ing_4/apr_i,
                                      mes_4==5  ~ ing_4/may_i,
                                      mes_4==6  ~ ing_4/jun_i,
                                      mes_4==7  ~ ing_4/jul_i)),
               ing_3=ifelse(is.na(ing_dt$mes_3), ing_3,
                            case_when(mes_3==5  ~ ing_3/may_i,
                                      mes_3==6  ~ ing_3/jun_i,
                                      mes_3==7  ~ ing_3/jul_i,
                                      mes_3==8  ~ ing_3/ago_i)),
               ing_2=ifelse(is.na(ing_dt$mes_2), ing_2,
                            case_when(mes_2==6  ~ ing_2/jun_i,
                                      mes_2==7  ~ ing_2/jul_i,
                                      mes_2==8  ~ ing_2/ago_i,
                                      mes_2==9  ~ ing_2/sep_i)),
               ing_1=ifelse(is.na(ing_dt$mes_1), ing_1,
                            case_when(mes_1==7  ~ ing_1/jul_i,
                                      mes_1==8  ~ ing_1/ago_i,
                                      mes_1==9  ~ ing_1/sep_i,
                                      mes_1==10 ~ ing_1/oct_i))) 
  
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
  
  # Victor's incomes classification lists 
  
  { 
  wages <-  c("P001", "P002", "P011", "P018", "P019", "P067")
  non_wage_income< - c("P003", "P004", "P005", "P006", "P007", "P009",
                       "P008", "P014", "P015", "P016")
  financial_capital <-  c("P026", "P027", "P028", "P029", "P030", "P031", "P050",
                          "P065", "P066", "P052", "P053", "P064")
  business_income <-  c("P068", "P069", "P070", "P071", "P072", "P073", "P074",
                        "P078", "P079", "P080", "P081", "P075", "P076", "P077") 
  sales <-c("P054", "P055", "P056", "P059", "P060", "P061", "P062", "P063")
  }
  
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
  #vars <- colnames(select(ing_dt, ing_mon, ing_lab, ing_ren, ing_tra))
  #ing_dt <- data.table(ing_dt)[, lapply(.SD, sum, na.rm=T), by=list(folioviv, foliohog), .SDcols = vars ]
  
}


