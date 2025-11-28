################################################ 
# Project: Malaria + Clima + Forest in Colombia
# Title: Malaria in Colombia data processing
# Last Mod: 19/Nov/2025
# Author: Daniela A. Puentes Herrera, Maria A. Parra & Laura Serna
################################################

# 0. Cargar librerias 
library(tidyverse) #Manejo de dataframes
library(lubridate) #Fechas
library(corrplot) #Correlaciones
library(car) #Correlaciones

# 1. Cargar datos depurados de malaria en municipios del Choco
ruta <- "datos/choco_data_V2.csv"
datos <- read.csv(ruta)
glimpse(datos)

# 2. Filtrar por especie 
falciparum <- datos %>% filter(MALARIA == "MALARIA FALCIPARUM") %>% na.omit()
vivax       <- datos %>% filter(MALARIA == "MALARIA VIVAX") %>% na.omit()

data_falciparum <- split(falciparum, falciparum$COD_MUN)
data_vivax <- split(vivax, vivax$COD_MUN)




# # 2. Variables seleccionadas
# selected_vars <- c("CASES", "MUN_T_MEAN_MAX", "MUN_T_MEAN_MIN", 
#                    "MUN_T_MEAN_MEAN", "PREC_CUM_MONTH", "mean_evi_interp")
# 
# climatic_vars <- c("MUN_T_MEAN_MAX", "MUN_T_MEAN_MIN", 
#                    "MUN_T_MEAN_MEAN", "PREC_CUM_MONTH", "mean_evi_interp")
# 

plot_muni_series <- function(data, X, Y){
  
  data %>% 
    ggplot(aes_string(x = X, y = Y)) +
    geom_line() +
    geom_point() +
    labs(title = paste("Municipality:", muni_id),
         x = time_col,
         y = value_col) +
    theme_minimal()
}

plot_muni_series(data_falciparum$`27001`, data_falciparum$`27001`$DATE2, data_falciparum$`27001`$CASES)

