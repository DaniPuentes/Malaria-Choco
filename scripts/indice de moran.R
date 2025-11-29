# ==============================================================================
# 1. INSTALAR Y CARGAR LIBRERÍAS
# ==============================================================================

library(sf)         # Para leer y manejar el Shapefile
library(dplyr)      # Para manipulación de datos
library(spdep)      # Para estadística espacial (Moran I)
library(tmap)       # Para visualización de mapas
library(RColorBrewer) # Paletas de colores

# ==============================================================================
# 2. CARGAR DATOS
# ==============================================================================

# A) Cargar los datos epidemiológicos 
 
datos_malaria <- read.csv("/Users/lauramsernalopez/Downloads/Malaria/choco_data_municipios.csv")

# B) Cargar Shapefile 

mapa_choco <- st_read("/Users/lauramsernalopez/Downloads/Malaria/choco_mun.shp") 

# ==============================================================================
# 3. PREPARACIÓN Y LIMPIEZA DE DATOS
# ==============================================================================

# Paso 3.1: Agrupar los datos temporales del CSV
data_agrupada <- datos_malaria %>%
  group_by(COD_MUN) %>%
  summarise(
    Total_Casos = sum(CASES_total, na.rm = TRUE),
    Prom_Precipitacion = mean(PREC_CUM_MONTH_M, na.rm = TRUE),
    Prom_EVI = mean(MEAN_EVI_M, na.rm = TRUE)
  ) %>%
  mutate(COD_MUN = as.character(COD_MUN)) # Convertir código a texto para el cruce

# Paso 3.2: Revisar la columna de código en el Shapefile
head(mapa_choco)

columna_codigo_shp <- "COD_MUN"

# Aseguramos que el código del mapa también sea texto para que cruce bien
mapa_choco$COD_MUN_JOIN <- as.character(mapa_choco[[columna_codigo_shp]])

# ==============================================================================
# 4. UNIR DATOS AL MAPA (JOIN)
# ==============================================================================

Map_sf <- left_join(mapa_choco, data_agrupada, by = c("COD_MUN_JOIN" = "COD_MUN"))

Map_sf$Total_Casos[is.na(Map_sf$Total_Casos)] <- 0

# ==============================================================================
# 5. MATRIZ DE PESOS ESPACIALES
# ==============================================================================

# Crear lista de vecinos (Criterio 'Queen': comparte borde o vértice)
neighbors <- poly2nb(Map_sf)

# Convertir a lista de pesos (Estandarizado por filas 'W')
weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

# ==============================================================================
# 6. INDICE DE MORAN GLOBAL (Para todo el municipio)
# ==============================================================================

moran_global <- moran.test(Map_sf$Total_Casos, weights, zero.policy = TRUE)

print("--- RESULTADOS MORAN GLOBAL ---")
print(moran_global)

# Interpretación:
# Si p-value < 0.05 y Moran I > 0: Hay agrupamiento significativo en el Chocó.

# ==============================================================================
# 7. INDICE DE MORAN LOCAL (LISA - Cluster Map)
# ==============================================================================

# Calcular Moran Local para cada municipio
local_moran <- localmoran(Map_sf$Total_Casos, weights, zero.policy = TRUE)

# Guardar resultados en el objeto del mapa
Map_sf$Ii <- local_moran[,1]  # Estadístico local
Map_sf$E_Ii<- local_moran[,2] # Esperanza
Map_sf$Var_Ii<- local_moran[,3] # Varianza
Map_sf$Z_Ii<- local_moran[,4] # Z-score
Map_sf$Pr <- local_moran[,5]  # P-value

# --- CREACIÓN DE CATEGORÍAS LISA (Alto-Alto, Bajo-Bajo, etc.) ---
# Centrar la variable de interés y el índice local
m.r <- Map_sf$Total_Casos - mean(Map_sf$Total_Casos)
m.lp <- Map_sf$Ii - mean(Map_sf$Ii)
significance_level <- 0.05

# Crear vector de cuadrantes
quadrant <- vector(mode="numeric", length=nrow(Map_sf))

# Definir cuadrantes
quadrant[m.r > 0 & m.lp > 0] <- 4 # High-High (Hotspot)
quadrant[m.r < 0 & m.lp < 0] <- 1 # Low-Low (Coldspot)
quadrant[m.r < 0 & m.lp > 0] <- 2 # Low-High (Outlier espacial)
quadrant[m.r > 0 & m.lp < 0] <- 3 # High-Low (Outlier espacial)

# Filtrar por significancia estadística (p-value)
quadrant[Map_sf$Pr > significance_level] <- 0 

# Asignar etiquetas legibles
Map_sf$LISA_Cluster <- factor(quadrant, levels=c(0, 1, 2, 3, 4),
                              labels=c("No Significativo", 
                                       "Bajo-Bajo (Coldspot)", 
                                       "Bajo-Alto", 
                                       "Alto-Bajo", 
                                       "Alto-Alto (Hotspot)"))

# ==============================================================================
# 8. VISUALIZACIÓN (MAPAS)
# ==============================================================================
tmap_mode("plot") # Cambia a "view" si quieres mapas interactivos en el navegador

# A. Mapa de Incidencia (Total de Casos)
mapa_casos <- tm_shape(Map_sf) +
  tm_polygons(col = "Total_Casos", 
              style = "jenks", 
              palette = "YlOrRd", 
              title = "Total Casos Malaria") +
  tm_layout(main.title = "Distribución de Casos", legend.outside = TRUE)

# B. Mapa de Clusters LISA (El más importante)
# Colores estándar: Rojo (HH), Azul (LL), Celeste (LH), Rosa (HL), Gris (No sig)
colores_lisa <- c("#eeeeee", "#4575b4", "#abd9e9", "#fdae61", "#d73027")

mapa_lisa <- tm_shape(Map_sf) +
  tm_polygons(col = "LISA_Cluster", 
              palette = colores_lisa,
              title = "Clusters LISA",
              border.col = "black", 
              lwd = 0.5) +
  tm_layout(main.title = "Análisis Moran Local (p<0.05)", legend.outside = TRUE)

# C. Visualizar ambos mapas juntos
tmap_arrange(mapa_casos, mapa_lisa)

