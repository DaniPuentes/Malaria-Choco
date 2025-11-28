"""
Project: Malaria-Chocó
Author: Daniela A Puentes Herrera 
Date: 28 November 2025

Description:
------------
This script provides two main functions to manage Python dependencies
for the Malaria-Chocó analysis environment.

1. load_or_install(packages):
   Attempts to import each requested package. If a package is missing,
   the function installs it using pip and then imports it again.

2. setup_environment():
   Defines all required libraries, installs missing ones, and returns
   a dictionary containing the imported modules for convenient access.

Notes:
------
- Built-in packages (glob, math, re) are not installed.
- Matplotlib is configured to use the "Agg" backend for environments
  without display systems (e.g., servers or clusters).
"""

import importlib, subprocess, sys

def load_or_install(pkgs):
    for p in pkgs:
        try: importlib.import_module(p)
        except ImportError:
            subprocess.check_call([sys.executable, "-m", "pip", "install", p])

def setup_environment():
    pkgs = ["glob2","imageio","matplotlib","numpy","pandas","rasterio"]
    load_or_install(pkgs)

    import glob, math, re, tempfile
    from pathlib import Path
    import imageio, matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    import numpy as np, pandas as pd, rasterio
    from matplotlib.colors import BoundaryNorm, ListedColormap

    return locals()

env = setup_environment()
import geopandas as gpd
import os 


# 1. Cargar datos 
ruta = "/Users/mac/Desktop/PROJECTS/Proyecto-Cuantitativa/Malaria-Choco-Proj/datos/choco_data_V2.csv"
datos = env["pd"].read_csv(ruta)

print(datos.info())
print(datos.head())

# 2. Filter by species (Falciparum) and remove rows with missing values
falciparum = datos[datos["MALARIA"] == "MALARIA FALCIPARUM"]

falciparum["DATE2"] = env["pd"].to_datetime(falciparum["DATE2"], format="%Y-%m")

print(falciparum.info())
print(falciparum.head())


# Columns to interpolate
cols = ["MUN_ELV_MAX", "MUN_ELV_MEAN", "MUN_T_MEAN_MAX", "MUN_T_MEAN_MIN", "MUN_T_MEAN_MEAN", "PREC_CUM_MONTH", "mean_evi", "mean_evi_interp"]

# Group by municipality, sort by date, interpolate linearly
falciparum = falciparum.sort_values(["COD_MUN", "DATE2"])

# Interpolate per municipality preserving original index
falciparum[cols] = (
    falciparum.groupby("COD_MUN")[cols]
              .transform(lambda g: g.interpolate("linear"))
)

print(falciparum.info())
print(falciparum.head())


# Ahora falciparum tiene los valores interpolados linealmente para las columnas especificadas.
# Puedes guardar el DataFrame resultante si es necesario
output_ruta = "/Users/mac/Desktop/PROJECTS/Proyecto-Cuantitativa/Malaria-Choco-Proj/datos/choco_data_falciparum_interpolated.csv"
falciparum.to_csv(output_ruta, index=False)


# Limpiamos la terminal 
os.system('cls' if os.name == 'nt' else 'clear')


# Verificamos que los datos se hayan guardado correctamente
datos = env["pd"].read_csv("/Users/mac/Desktop/PROJECTS/Proyecto-Cuantitativa/Malaria-Choco-Proj/datos/choco_data_falciparum_interpolated.csv")

print(datos.info())
print(datos.head())

# Cargamos la data espacial de los municipios
shapefile_path = "/Users/mac/Desktop/PROJECTS/Proyecto-Cuantitativa/Malaria-Choco-Proj/Choco-shape/choco_mun.shp"
municipios = gpd.read_file(shapefile_path)


# Ajustar tipos de datos para la fusión
municipios["COD_MUN"] = municipios["COD_MUN"].astype(int)
datos["COD_MUN"] = datos["COD_MUN"].astype(int)

# Unir los datos tabulares con los datos espaciales
gdf = municipios.merge(datos, on="COD_MUN", how="left")


print(gdf.info())
print(gdf.head())


# Vamos a graficar una fecha en especifico 
sel = gdf[gdf["DATE2"] == "2018-07-01"]



def plot_var(gdf_sel, var, cmap="viridis"):
    fig, ax = plt.subplots(figsize=(9, 7))
    gdf_sel.plot(
        column=var,
        cmap=cmap,
        edgecolor="black",
        linewidth=0.3,
        legend=True,
        ax=ax
    )
    ax.set_title(f"{var} — {gdf_sel['DATE2'].iloc[0]}")
    ax.axis("off")
    plt.show()


plot_var(sel, "CASES", cmap="Reds")
plot_var(sel, "mean_evi_interp", cmap="Greens")
plot_var(sel, "PREC_CUM_MONTH", cmap="Blues")
plot_var(sel, "MUN_T_MEAN_MEAN", cmap="coolwarm")   
plot_var(sel, "MUN_ELV_MEAN", cmap="terrain")