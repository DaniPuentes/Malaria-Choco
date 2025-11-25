################################################ 
# Project: Malaria and land land in Colombia
# Title: Malaria in Colombia data processing
# Last Mod: 23/Oct/2025
# Author: Daniela A. Puentes Herrera
################################################

# 0. Cargar librerias 
library(readxl)
library(tidyverse)
library(lubridate)
library(purrr)

# 1. Definir la ruta
ruta <- "/Users/mac/Desktop/PROJECTS/Proyecto-Cuantitativa/malaria"

# 2. Listar todos los archivos .xls y .xlsx
archivos <- list.files(
  path = ruta, 
  pattern = "\\.xls[x]?$",  # acepta .xls y .xlsx
  full.names = TRUE
)

# 3. Función para procesar cada archivo
procesar_archivo <- function(archivo) {
  read_excel(archivo) %>%
    mutate(
      MALARIA = case_when(
        COD_EVE == 470 ~ "MALARIA FALCIPARUM",
        COD_EVE == 490 ~ "MALARIA VIVAX"
      ),
      FEC_NOT = ymd(FEC_NOT), #Descomponer la fecha en Año, Mes y Día 
      FEC_ANO = year(FEC_NOT),
      FEC_MES = month(FEC_NOT),
      FEC_DIA = day(FEC_NOT)
    ) %>%
    filter(Pais_ocurrencia == "COLOMBIA", COD_PAIS_O == "170", COD_DPTO_O == "27") %>%
    select(any_of(c(#Variables seleccionadas para el análisis
      "FEC_NOT", "MALARIA", "FEC_ANO", "FEC_MES", "FEC_DIA",
      "EDAD", "SEXO", "COD_MUN_O","COD_DPTO_O","AREA", "OCUPACION",
      "COD_MUN_R", "COD_DPTO_N", "COD_MUN_N",
      "Departamento_ocurrencia", "Municipio_ocurrencia"
    ))) %>% 
    select(MALARIA, contains("FEC"), everything()) %>%
    filter(
      !grepl("\\*", Departamento_ocurrencia),
      !grepl("\\*", Municipio_ocurrencia)
    ) %>%
    na.omit()
}

# 4. Aplicar la función a todos los archivos y unirlos
data_total <- map_dfr(archivos, procesar_archivo)

# 5. Revisar resultado
glimpse(data_total)

# 6. Exportar datos
write.csv(data_total, "/Users/mac/Desktop/PROJECTS/Proyecto-Cuantitativa/malaria/malaria_total_choco.csv", row.names = FALSE)