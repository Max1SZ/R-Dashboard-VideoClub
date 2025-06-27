# ---- 1. Paquetes(para instalar) ----
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(dplyr)


# ---- 2. Dataset ficticio de ejemplo. En cualquier caso se puede hacer un csv ----
set.seed(123)
generos     <- c("Acción", "Comedia", "Drama", "Terror", "Fantasía", "Ciencia Ficción")
sucursales  <- c("Centro", "Tolosa", "Los Hornos", "City Bell", "San Carlos", "Gonnet")

peliculas <- data.frame(
  id       = 1:300,
  titulo   = paste("Película", 1:300),
  genero   = sample(generos,    300, replace = TRUE),
  sucursal = sample(sucursales, 300, replace = TRUE),
  rating   = sample(1:5,        300, replace = TRUE),
  visitas  = sample(50:500,     300, replace = TRUE),   # <-- visitas simuladas
  lat      = -34.921 + runif(300, -0.02, 0.02),          # coords La Plata
  lng      = -57.954 + runif(300, -0.02, 0.02)
)

# ---- 3. UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Videoclub RetroVision"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen", tabName = "resumen", icon = icon("dashboard")),
      menuItem("Mapa",    tabName = "mapa",    icon = icon("map"))
    ),
    sliderInput("rating",   "Rating mínimo:", min = 1, max = 5, value = 3),
    selectInput("genero",   "Género:",      choices = c("Todos", generos)),
    selectInput("sucursal", "Sucursal:",    choices = c("Todas", sucursales))
  ),
# ---- CSS para detalles sin necesidad de librerias ----
  dashboardBody(
    tags$head(
      tags$style(HTML("
  /* Fondo general oscuro */
  body, .content-wrapper, .right-side {
    background-color: #121212 !important;
    color: #FFFFFF;
  }

  /* Header azul Blockbuster */
  .main-header .logo, .main-header .navbar {
    background-color: #0033A0 !important;
    color: #FFD700 !important;
  }

  /* Sidebar oscuro con textos claros */
  .main-sidebar {
    background-color: #1e1e1e !important;
  }
  .sidebar-menu > li > a {
    color: #FFD700 !important;
  }

  /* Títulos de boxes */
  .box-title {
    color: #FFD700 !important;
    font-weight: bold;
    font-family: 'Verdana', sans-serif;
    font-size: 18px;
  }

  /* Texto dentro de box */
  .box-body {
    color: #FFFFFF !important;
  }

  /* Controles de inputs */
  .control-label {
    color: #FF4C4C !important;
    font-weight: bold;
  }

  /* Value boxes personalizados */
  .small-box.bg-blue {
    background-color: #0033A0 !important;
    color: #FFD700 !important;
  }
  .small-box.bg-green {
    background-color: #228B22 !important;
    color: #FFFFFF !important;
  }
  .small-box.bg-red {
    background-color: #FF4C4C !important;
    color: #FFFFFF !important;
  }

  /* Leyendas del mapa */
  .leaflet-control {
    background-color: #1e1e1e !important;
    color: #FFFFFF !important;
  }

  /* Títulos generales */
  h1, h2, h3, h4, h5, h6 {
    color: #FFD700 !important;
    font-family: 'Impact', 'Arial Black', sans-serif;
  }
    "))
    ),
    tabItems(
      # --- TAB RESUMEN --------------------------
      tabItem(tabName = "resumen",
              fluidRow(
                valueBoxOutput("total_peliculas"),
                valueBoxOutput("promedio_rating"),
                valueBoxOutput("sucursal_top")
              ),
              fluidRow(
                # Nuevo gráfico de barras de visitas por género
                box(plotlyOutput("plot_visitas_genero"), width = 6),
                # Torta de visitas por género para comparar
                box(plotlyOutput("pie_visitas_genero"),  width = 6)
              )
      ),
      
      # --- TAB MAPA -----------------------------
      tabItem(tabName = "mapa",
              fluidRow(
                box(leafletOutput("mapa_sucursales", height = 600), width = 12)
              )
      )
    )
  )
)

# ---- 4. SERVER ----
server <- function(input, output) {
  
  # --- Filtro reactivo ----------------------------------
  datos_filtrados <- reactive({
    df <- peliculas %>% filter(rating >= input$rating)
    if (input$genero   != "Todos")  df <- df %>% filter(genero   == input$genero)
    if (input$sucursal != "Todas")  df <- df %>% filter(sucursal == input$sucursal)
    df
  })
  
  # --- Value‑boxes --------------------------------------
  output$total_peliculas <- renderValueBox({
    valueBox(nrow(datos_filtrados()), "Películas", icon = icon("film"), color = "blue")
  })
  
  output$promedio_rating <- renderValueBox({
    prom <- round(mean(datos_filtrados()$rating), 2)
    valueBox(prom, "Rating promedio", icon = icon("star"), color = "green")
  })
  
  output$sucursal_top <- renderValueBox({
    top <- datos_filtrados() %>%
      group_by(sucursal) %>% summarise(visitas = sum(visitas)) %>%
      slice_max(visitas, n = 1)
    valueBox(top$sucursal, "Sucursal más visitada", icon = icon("store"), color = "orange")
  })
  
  # --- GRÁFICOS -----------------------------------------
  # 1) Barras de visitas por género
  output$plot_visitas_genero <- renderPlotly({
    datos_filtrados() %>%
      group_by(genero) %>% summarise(visitas = sum(visitas)) %>%
      arrange(desc(visitas)) %>%
      plot_ly(x = ~genero, y = ~visitas, type = "bar", color = ~genero) %>%
      layout(title = "Visitas por género",
             xaxis = list(title = "Género"),
             yaxis = list(title = "Cantidad de visitas"))
  })
  
  # 2) Pie de visitas por género
  output$pie_visitas_genero <- renderPlotly({
    datos_filtrados() %>%
      group_by(genero) %>% summarise(visitas = sum(visitas)) %>%
      plot_ly(labels = ~genero, values = ~visitas, type = "pie") %>%
      layout(title = "Participación de visitas por género")
  })
  
  # --- MAPA ---------------------------------------------
  output$mapa_sucursales <- renderLeaflet({
    df  <- datos_filtrados()
    pal <- colorFactor(rainbow(length(unique(df$sucursal))), domain = df$sucursal)
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~log(visitas + 10),
        color = ~pal(sucursal), stroke = FALSE, fillOpacity = 0.8,
        popup = ~paste0("<b>", titulo, "</b><br>Género: ", genero,
                        "<br>Sucursal: ", sucursal,
                        "<br>Rating: ", rating,
                        "<br>Visitas: ", visitas)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~sucursal, title = "Sucursales", opacity = 1)
  })
}

# ---- 5. RUN APP ----
shinyApp(ui, server)
