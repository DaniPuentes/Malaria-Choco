################################################ 
# Project: Malaria + Clima + Forest in Colombia
# Title: Malaria in Colombia data processing
# Last Mod: 19/Nov/2025
# Author: Daniela A. Puentes Herrera
################################################

# 0. Cargar librerias 
library(readxl)
library(tidyverse)
library(lubridate)
library(purrr)

# 1. Definir la ruta
ruta <- "/Users/mac/Desktop/PROJECTS/Proyecto-Cuantitativa/malaria/GEE_Exports"

# 2. Listar todos los archivos csv
archivos <- list.files(
  path = ruta, 
  pattern = "\\.csv$",  # acepta csv
  full.names = TRUE
)

# 3. Función para procesar cada archivo
procesar_archivo <- function(archivo) {
  
  df <- read.csv(archivo, stringsAsFactors = FALSE)
  
  df <- df %>%
    mutate(
      YYYYMM   = as.character(YYYYMM),
      COD_MUN  = as.character(COD_MUN),
      mean_evi = as.numeric(mean_evi)
    )
  
  return(df)
}

# 4. Aplicar la función a todos los archivos y unirlos
data_total <- map_dfr(archivos, procesar_archivo) %>% mutate(DATE2 = YYYYMM)

# 1. Crear calendario mensual de 180 meses
calendar <- tibble(
  YYYYMM = format(seq(as.Date("2008-01-01"), as.Date("2023-12-01"), by = "month"), "%Y-%m")
)


tener envuenta los dos tipos de MALARIA VIVAX y MALARIA FALCIPARUM 
data1 <- expand.grid(
  COD_MUN = municipalities,
  DATE2 = calendar$YYYYMM
) %>%
  as_tibble() %>%
  mutate(tes = 0) %>% arrange(COD_MUN)


data_final <- data1 %>%
  left_join(
    data_total %>% select(COD_MUN, DATE2, mean_evi),
    by = c("COD_MUN", "DATE2"),
  ) 

data_final %>% na.omit() %>%ggplot(aes(x=DATE2, y=mean_evi)) + geom_point() + theme_classic()

# 6. Exportar datos
write.csv(data_total, "/Users/mac/Desktop/PROJECTS/Proyecto-Cuantitativa/malaria/malaria_total_choco.csv", row.names = FALSE)




