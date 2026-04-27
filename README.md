# Visualización de Accidentes Viales - Shiny

## Descripción

Esta aplicación web interactiva fue desarrollada con Shiny en R para analizar un dataset de accidentes viales. Permite explorar barrios con mayor accidentalidad, evolución temporal, personas heridas, vehículos involucrados, proporciones y patrones por mes y año.

## Dataset

- **Fuente:** Datos Abiertos Colombia
- **Tema:** Accidentes viales
- **Archivo utilizado:** `Accidentes_Viales_20260426.csv`
- **Descripción:** El dataset contiene registros de accidentes viales con variables como fecha de ocurrencia, barrio, dirección, vehículos involucrados y personas heridas.

## Visualizaciones implementadas

1. Gráfico de barras horizontal para comparar barrios con más accidentes.
2. Gráfico de línea para observar la evolución mensual.
3. Histograma para analizar la distribución de heridos.
4. Gráfico de dispersión para relacionar vehículos involucrados y heridos.
5. Gráfico de dona para comparar accidentes con y sin heridos.
6. Gráfico de caja para analizar la distribución de heridos por barrio.
7. Mapa de calor para observar accidentes por mes y año.

## Tecnologías utilizadas

- R
- Shiny
- Plotly
- Dplyr
- Lubridate
- DT
- Bslib
- GitHub
- shinyapps.io

# Despligue

https://jhon9036.shinyapps.io/visualizacion-shiny/

# Autor

Jhon Alexander Vargas

## Instalación y ejecución local

```r
install.packages(c("shiny", "plotly", "dplyr", "lubridate", "DT", "bslib"))
shiny::runApp()

