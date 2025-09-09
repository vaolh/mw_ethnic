rm(list = ls())

if (!require(pacman)) install.packages("pacman")
p_load(dplyr, readxl)

inpc_all <- read_xlsx("ca57_2018a.xlsx", col_names = FALSE) %>%
  dplyr::select(2) %>%
  dplyr::rename(INPC = 1)

inpc_all <- inpc_all[c(14:680),]

inpc_all$INPC <- as.numeric(inpc_all$INPC)
inpc_all <- inpc_all %>% filter(!is.na(INPC))

inpc_all$Fecha <- seq(as.Date("1970-01-01"), by = "month", length.out = nrow(inpc_all))
inpc_all$Año_int <- as.integer(format(inpc_all$Fecha, "%Y"))
inpc_all$Mes     <- as.integer(format(inpc_all$Fecha, "%m"))

inpc <- inpc_all %>%
  filter((Año_int >= 2015) & (Año_int != 2025))

inpc <- inpc %>%
  mutate(Deflator = INPC / INPC[which(inpc$Año_int == 2024 & inpc$Mes == 8)])







