# --- LIBRARIES ---
library(terra)
library(sf)
library(dplyr)
library(exactextractr)
library(stringr)

# --- INPUT DATA ---
# Define paths
path_rasters <- "/Users/mac/Desktop/PROJECTS/Proyecto-Cuantitativa/bosque-no-bosque/"    # carpeta con bosque_YYYY.tif
path_shp <- "/Users/mac/Desktop/MGN2018_00_COLOMBIA/ADMINISTRATIVO/MGN_MPIO_POLITICO.shp"

# Load shapefile
municipios <- st_read(path_shp)

# --- 1. FILTRAR MUNICIPIOS DE INTERÉS ---
 municipios_sel <- municipios %>%
   filter(MPIO_CNMBR %in% c("QUIBDÓ", "ALTO BAUDÓ", "BAJO BAUDÓ", "BAGADÓ",
                            "PUEBLO RICO", "MAGÜÍ", "ROBERTO PAYÁN"))


st_write(
  municipios_sel,
  "/Users/mac/Desktop/PROJECTS/Proyecto-Cuantitativa/municipios_sel.shp",
  delete_layer = TRUE
)

# --- 2. LISTAR TODOS LOS RASTERS ---
rasters_paths <- list.files(path_rasters, pattern = "\\.img$", full.names = TRUE)

# Extraer año desde el nombre (asume algo como 'bosque_2000.tif')
years <- str_extract(basename(rasters_paths), "\\d{4}")

# --- 3. PROCESAR CADA AÑO ---
resultados <- list()

for (i in seq_along(rasters_paths)) {
  cat("Procesando:", rasters_paths[i], "\n")
  r <- rast(rasters_paths[i])
  r <- project(r, crs(municipios_sel))
  r_crop <- crop(r, municipios_sel)
  r_mask <- mask(r_crop, municipios_sel)
  
  # función segura
  safe_extract <- function(x, c) {
    if (all(is.na(x))) {
      return(data.frame(bosque = 0, nobosque = 0))
    } else {
      return(data.frame(
        bosque = sum(c[x == 1], na.rm = TRUE),
        nobosque = sum(c[x == 0], na.rm = TRUE)
      ))
    }
  }
  
  area_list <- exact_extract(r_mask, municipios_sel, fun = safe_extract)
  
  df <- data.frame(
    municipio = municipios_sel$MPIO_CNMBR,
    bosque_m2 = sapply(area_list, function(x) x$bosque),
    nobosque_m2 = sapply(area_list, function(x) x$nobosque),
    year = as.numeric(years[i])
  )
  
  resultados[[i]] <- df
}

# --- 4. UNIR Y CALCULAR PORCENTAJES ---
resumen <- bind_rows(resultados) %>%
  mutate(
    total_m2 = bosque_m2 + nobosque_m2,
    pct_bosque = 100 * bosque_m2 / total_m2,
    pct_nobosque = 100 * nobosque_m2 / total_m2
  ) %>%
  arrange(municipio, year)

# --- 5. RESULTADO FINAL ---
print(resumen)

# --- (Opcional) GUARDAR RESULTADOS ---
write.csv(resumen, "area_bosque_por_municipio.csv", row.names = FALSE)