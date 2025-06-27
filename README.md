# R-Dashboard-VideoClub
RetroVision Dashboard es una aplicación interactiva desarrollada en R Shiny que simula un sistema de análisis para una red ficticia de videoclubes distribuidos en la ciudad de La Plata, inspirado en la estética de los años 2000.

Permite explorar el comportamiento de usuarios por género cinematográfico, identificar la sucursal más visitada, y visualizar cada local sobre un mapa interactivo. Su diseño retro y oscuro busca evocar la nostalgia de la era del VHS y DVD, combinando herramientas modernas de análisis visual con una interfaz temática y amigable.

Tecnologías utilizadas
*Shiny + shinydashboard (estructura de la app)

*dplyr, plotly, ggplot2 (análisis y visualización)

*leaflet (mapa interactivo de sucursales)

*CSS personalizado (tema oscuro estilo años 2000)

Funcionalidades
*Filtros dinámicos por género, barrio y visitas

*Indicadores clave: total de visitas, sucursal top, atención 24hs

*Gráficos interactivos por género y local

*Mapa con ubicación y datos de cada sucursal

Cómo ejecutar en R

# Instalar dependencias si es necesario
install.packages(c("shiny", "shinydashboard", "dplyr", "plotly", "leaflet", "ggplot2"))

# Correr la app
shiny::runApp("app.R")
