################################################ 
# Project: Malaria + Clima + EVI in Colombia
# Title: Malaria in Choco data processing
# Last Mod: 18/Nov/2025
# Author: Daniela A. Puentes Herrera
################################################

# 0. Load libraries 
library(readxl)
library(tidyverse)
library(lubridate)
library(purrr)
library(sf)
library(zoo)

################################################
# Load input data
################################################
# Malaria data from SIVIGILA (2008-01 to 2023-12)
malaria <- read.csv(file="malaria_total_choco.csv") 

# Spatial municipalities in Choco from DANE (2018)
shape_mun_choco <-  st_read("/Users/mac/Desktop/PROJECTS/Proyecto-Cuantitativa/malaria/choco_mun.shp")

# Climate dataset
colombia_data <- read.csv( "/Users/mac/Desktop/PROJECTS/Dengue_expansions-main/DENGUE_COLOMBIA_2008_2023_DATA_SET.csv"
) %>% select("COD_MUN", "DATE2", "URB_ELV_MEAN", "MUN_ELV_MAX"  , "MUN_ELV_MEAN", 
             "MUN_T_MEAN_MAX", "MUN_T_MEAN_MIN", "MUN_T_MEAN_MEAN",  "PREC_CUM_MONTH")

################################################
# Harmonize municipality codes
################################################
malaria$COD_MUN_O <- as.character(malaria$COD_MUN_O)
malaria$COD_MUN_O<- str_pad(malaria$COD_MUN_O, width = 3, pad = "0")
malaria$COD_DPTO_O <- as.character(malaria$COD_DPTO_O)

# Standardize COD_MUN
malaria$COD_MUN <- paste0(malaria$COD_DPTO_O, malaria$COD_MUN_O)
malaria$COD_MUN <- as.character(malaria$COD_MUN)

# Keep only municipalities present in the shape file
malaria <- malaria[malaria$COD_MUN %in% shape_mun_choco$COD_MU, ]

################################################
# Aggregate malaria to monthly level
################################################
malaria_mensual <- malaria %>% group_by(FEC_ANO, FEC_MES, COD_MUN, MALARIA) %>% summarize( 
  CASES = n(), .groups = "drop") %>% filter(FEC_ANO != 2023)

rm(malaria) #Clean space
malaria_mensual$DATE2 <- paste(malaria_mensual$FEC_ANO, as.character(malaria_mensual$FEC_MES), sep = "-")

# Ensure consistent types
colombia_data$COD_MUN      <- as.character(colombia_data$COD_MUN)
malaria_mensual$COD_MUN    <- as.character(malaria_mensual$COD_MUN)

################################################
# Create full malaria dataset  
# (15 years × 12 months × 30 municipalities × 2 species)
################################################
years <- 2008:2022                      # 15 years
months <- sprintf("%02d", 1:12)         # 12 months
municipalities <- unique(malaria_mensual$COD_MUN) # Vector of municipalities

# Malaria species
species <- c("MALARIA FALCIPARUM", "MALARIA VIVAX")

# Create the full grid 
data_full <- expand_grid(
  FEC_ANO = years,
  FEC_MES = months,
  COD_MUN = municipalities,
  MALARIA = species
) %>%
  mutate(DATE2 = paste0(FEC_ANO, "-", FEC_MES)
  ) %>% arrange(COD_MUN)


################################################
# Merge full dim dataset with real malaria observations
################################################

# Standardize types
malaria_mensual <- malaria_mensual %>%
  mutate(
    FEC_ANO = as.integer(FEC_ANO),
    FEC_MES = sprintf("%02d", as.integer(FEC_MES)),
    COD_MUN = as.character(COD_MUN),
    MALARIA = as.character(MALARIA) )

data_full <- data_full %>%
  mutate(
    FEC_ANO = as.integer(FEC_ANO),
    FEC_MES = sprintf("%02d", as.integer(FEC_MES)),
    COD_MUN = as.character(COD_MUN),
    MALARIA = as.character(MALARIA)
  )

# Merge + fill missing cases with zero
malaria_choco <- data_full %>%
  left_join(
    malaria_mensual %>% select(FEC_ANO, FEC_MES, COD_MUN, MALARIA, CASES),
    by = c("FEC_ANO", "FEC_MES", "COD_MUN", "MALARIA")
  ) %>%
  mutate(
    CASES = ifelse(is.na(CASES), 0, CASES),  # Rellena faltantes con 0
    DATE2 = paste0(FEC_ANO, "-", FEC_MES)
  ) %>% arrange(COD_MUN)

rm(data_full, malaria_mensual)

################################################
#  Malaria + Climate Section
################################################

choco_climate <- colombia_data %>% filter(COD_MUN %in% municipalities) %>% arrange(COD_MUN)
rm(colombia_data) # Clean space

choco_data <- malaria_choco %>% left_join(
    choco_climate,
    by = c("COD_MUN", "DATE2")
  ) %>% arrange(COD_MUN, DATE2, MALARIA)

rm(choco_climate, malaria_choco) # Clean space


################################################
# choco_data + EVI
################################################

ruta <- "/Users/mac/Desktop/PROJECTS/Proyecto-Cuantitativa/malaria/GEE_Exports/V2"

################################################
# Process each EVI CSV
################################################
archivos <- list.files(
  path = ruta,
  pattern = "\\.csv$",
  full.names = TRUE
)

# Function to process each EVI CSV
procesar_archivo <- function(archivo) {
  df <- read.csv(archivo, stringsAsFactors = FALSE)
  
  df %>%
    mutate(
      YYYYMM   = as.character(YYYYMM),
      COD_MUN  = as.character(COD_MUN),
      mean_evi = as.numeric(mean_evi)
    )
}

################################################
# Merge all EVI data
################################################
data_total <- map_dfr(archivos, procesar_archivo) %>%
  mutate(DATE2 = YYYYMM)

# Create monthly calendar (2008–2023 = 180 months)
calendar <- tibble(
  YYYYMM = format(
    seq(as.Date("2008-01-01"), as.Date("2022-12-01"), by = "month"),
    "%Y-%m"
  )
)
# Create full EVI × malaria-type dataset
malaria_types <- c("MALARIA VIVAX", "MALARIA FALCIPARUM")

data1 <- expand.grid(
  COD_MUN = municipalities,
  DATE2 = calendar$YYYYMM,
  MALARIA = malaria_types
) %>%
  as_tibble() %>%
  arrange(COD_MUN, DATE2, MALARIA)
rm(calendar)

# Merge full calendar with real EVI values
evi <- data1 %>%
  left_join(
    data_total %>% select(COD_MUN, DATE2, mean_evi),
    by = c("COD_MUN", "DATE2")
  ) %>%
  mutate(
    mean_evi = ifelse(is.na(mean_evi), "No data", mean_evi)
  )

#  Choco_data + EVI
choco_data_V2 <- choco_data %>% left_join(
  evi, by = c("COD_MUN", "DATE2", "MALARIA")) %>% arrange(COD_MUN, DATE2, MALARIA)

rm(evi, choco_data, data1, data_total)

# Convertir columnas antes
choco_data_V2 <- choco_data_V2 %>%
  mutate(
    FEC_MES = as.character(FEC_MES),
    COD_MUN = as.character(COD_MUN),
    mean_evi = as.numeric(as.character(mean_evi))
  )

# Luego resumir
historicos_interpolados <- choco_data_V2 %>%
  arrange(COD_MUN, FEC_MES) %>%
  group_by(COD_MUN) %>%
  mutate(
    mean_evi_interp = na.approx(mean_evi, na.rm = FALSE)
  ) %>%
  ungroup()


################################################
# Incorporate spatial data
################################################
choco_data_shp <- choco_data_V2 %>%
  left_join(shape_mun_choco, by = "COD_MUN")

################################################
# Export data 
################################################
write.csv(historicos_interpolados, "/Users/mac/Desktop/PROJECTS/Proyecto-Cuantitativa/malaria/choco_data_V2.csv", row.names = FALSE)



