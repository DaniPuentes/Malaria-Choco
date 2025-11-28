################################################ 
# Project: Malaria + Clima + Forest in Colombia
# Title: Malaria in Colombia data processing
# Last Mod: 19/Nov/2025
# Author: Daniela A. Puentes Herrera, Maria A. Parra & Laura Serna
################################################


# Load libraries
packages <- c("readr","Hmisc","zoo","lubridate","dplyr","ggplot2","forecast","tseries","lmtest","ggfortify", "reshape2")
lapply(packages, library, character.only = TRUE)
rm(packages)


# Cargar datos depurados de malaria en municipios del Choco
ruta <- "datos/choco_data_agrupada.csv"
datos <- read.csv(ruta)
datos <- datos %>% arrange(DATE2) #Ordenar los datos
glimpse(datos)
rm(ruta)

# Series de tiempo
cases_ts <- ts(datos$CASES_total, start = c(2008,1), frequency = 12)
temp_ts  <- ts(datos$MUN_T_MEAN_MEAN_M, start = c(2008,1), frequency = 12)
prec_ts  <- ts(datos$PREC_CUM_MONTH_M,  start = c(2008,1), frequency = 12)
evi_ts   <- ts(datos$MEAN_EVI_M,       start = c(2008,1), frequency = 12)


# Correlaciones simples

## Renombrar los datos 
df_cor <- datos %>% select(Cases = CASES_total, Mean_Temperature = MUN_T_MEAN_MEAN_M,
    Accumulative_Precipitation = PREC_CUM_MONTH_M, Mean_EVI = MEAN_EVI_M)

## Matriz de correlacion 
mydata.cor = cor(df_cor, method = c("pearson"))
mydata.rcorr = rcorr(as.matrix(df_cor)) # Matriz de correlación con p-values
mydata.coeff = mydata.rcorr$r
mydata.p =  mydata.rcorr$P


## Plotear 
corrplot(mydata.cor)


# Correlacion cruzada 

# Temperatura → Casos
ccf(temp_ts, cases_ts, main="CCF: Temperature vs Malaria Cases")

# Precipitación → Casos
ccf(prec_ts, cases_ts, main="CCF: Precipitation vs Malaria Cases")

# EVI → Casos
ccf(evi_ts, cases_ts, main="CCF: EVI vs Malaria Cases")





