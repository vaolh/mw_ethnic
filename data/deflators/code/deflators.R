if (!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, jsonlite, lubridate)

json_file <- "../input/inpc.json"
data <- fromJSON(json_file)

inpc_obs <- data[["Series"]][["OBSERVATIONS"]]
inpc_df <- do.call(rbind, lapply(inpc_obs, as.data.frame))

inpc_df$Fecha <- as.Date(paste0(inpc_df$TIME_PERIOD, "/01"), format="%Y/%m/%d")
inpc_df$Año_int <- as.integer(format(inpc_df$Fecha, "%Y"))
inpc_df$Mes     <- as.integer(format(inpc_df$Fecha, "%m"))

inpc_df <- inpc_df %>%
  dplyr::select(
    INPC = OBS_VALUE,
    Fecha,
    Año_int,
    Mes
  ) %>%
  mutate(INPC = as.numeric(INPC)) 

inpc <- inpc_df %>%
  filter((Año_int >= 2015) & (Año_int != 2025))

inpc <- inpc %>%
  mutate(Deflator = INPC / INPC[which(inpc$Año_int == 2024 & inpc$Mes == 8)])

if (!dir.exists("../output")) dir.create("../output", recursive = TRUE)

write.csv(inpc, "../output/inpc.csv", row.names = FALSE)
