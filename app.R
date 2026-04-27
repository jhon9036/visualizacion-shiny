library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(DT)
library(bslib)

# =========================
# CONFIGURACION
# =========================

ruta <- "data/Accidentes_Viales_20260426.csv"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

paleta_barrio <- c(
  "#2563EB", "#14B8A6", "#F59E0B", "#EF4444",
  "#8B5CF6", "#22C55E", "#06B6D4", "#F97316",
  "#64748B", "#DB2777", "#84CC16", "#0EA5E9"
)

colores_gravedad <- c("Sin heridos" = "#10B981", "Con heridos" = "#EF4444")
color_fondo <- "#0F172A"
color_panel <- "#111827"
color_texto <- "#E5E7EB"
color_texto_suave <- "#94A3B8"
color_grid <- "rgba(148, 163, 184, 0.18)"
color_linea <- "#38BDF8"
color_acento <- "#F59E0B"

escala_barras <- list(
  c(0, "#1D4ED8"),
  c(0.55, "#06B6D4"),
  c(1, "#F59E0B")
)

escala_calor <- list(
  c(0, "#1E293B"),
  c(0.35, "#2563EB"),
  c(0.7, "#F59E0B"),
  c(1, "#EF4444")
)

# =========================
# FUNCIONES DE LIMPIEZA
# =========================

normalizar_columna <- function(nombre) {
  nombre <- trimws(tolower(nombre))
  nombre <- iconv(nombre, from = "", to = "ASCII//TRANSLIT")
  nombre <- gsub("[^a-z0-9]+", "_", nombre)
  nombre <- gsub("^_|_$", "", nombre)
  nombre[nombre == "" | is.na(nombre)] <- "columna"
  nombre
}

buscar_columna <- function(columnas, palabra) {
  resultado <- columnas[grepl(palabra, columnas, ignore.case = TRUE)]
  if (length(resultado) > 0) resultado[1] else NA_character_
}

limpiar_texto <- function(x, valor_por_defecto = "NO REGISTRA", mayusculas = FALSE) {
  x <- trimws(as.character(x))
  x_mayus <- toupper(x)

  vacios <- is.na(x) | x == "" | x_mayus %in% c("NA", "N/A", "NULL", "NONE", "NAN")
  vacios[is.na(vacios)] <- TRUE

  x[vacios] <- valor_por_defecto
  if (mayusculas) x <- toupper(x)
  x
}

convertir_numero <- function(x) {
  x <- gsub(",", ".", as.character(x), fixed = TRUE)
  valor <- suppressWarnings(as.numeric(x))
  valor[is.na(valor)] <- 0
  valor
}

normalizar_meses_texto <- function(x) {
  reemplazos <- c(
    "enero" = "Jan", "ene" = "Jan", "jan" = "Jan",
    "febrero" = "Feb", "feb" = "Feb",
    "marzo" = "Mar", "mar" = "Mar",
    "abril" = "Apr", "abr" = "Apr", "apr" = "Apr",
    "mayo" = "May", "may" = "May",
    "junio" = "Jun", "jun" = "Jun",
    "julio" = "Jul", "jul" = "Jul",
    "agosto" = "Aug", "ago" = "Aug", "aug" = "Aug",
    "septiembre" = "Sep", "setiembre" = "Sep", "sep" = "Sep", "set" = "Sep",
    "octubre" = "Oct", "oct" = "Oct",
    "noviembre" = "Nov", "nov" = "Nov",
    "diciembre" = "Dec", "dic" = "Dec", "dec" = "Dec"
  )

  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  for (patron in names(reemplazos)) {
    x <- gsub(
      paste0("\\b", patron, "\\b"),
      reemplazos[[patron]],
      x,
      ignore.case = TRUE
    )
  }
  x
}

convertir_fecha <- function(x) {
  if (inherits(x, "POSIXt")) return(x)
  if (inherits(x, "Date")) return(as.POSIXct(x))

  texto <- trimws(as.character(x))
  texto[texto == ""] <- NA_character_
  texto <- normalizar_meses_texto(texto)

  ordenes <- c(
    "ymd HMS", "ymd HM", "ymd IMS p", "ymd IM p", "ymd",
    "dmy HMS", "dmy HM", "dmy IMS p", "dmy IM p", "dmy",
    "mdy HMS", "mdy HM", "mdy IMS p", "mdy IM p", "mdy",
    "y b d HMS", "y b d HM", "y b d IMS p", "y b d IM p", "y b d",
    "d b y HMS", "d b y HM", "d b y IMS p", "d b y IM p", "d b y"
  )

  fecha <- suppressWarnings(
    parse_date_time(texto, orders = ordenes, tz = "America/Bogota", locale = "C")
  )

  numeros <- suppressWarnings(as.numeric(texto))
  es_excel <- is.na(fecha) & !is.na(numeros) & numeros > 20000 & numeros < 80000
  if (any(es_excel)) {
    fecha[es_excel] <- as.POSIXct(
      as.Date(numeros[es_excel], origin = "1899-12-30"),
      tz = "America/Bogota"
    )
  }

  fecha
}

renombrar_columnas <- function(df) {
  names(df) <- make.unique(normalizar_columna(names(df)), sep = "_")

  mapa <- list(
    fecha = "fecha",
    barrio = "barrio",
    vehiculos = "vehicul",
    heridos = "herid",
    fallecidos = "fallecid",
    direccion = "direccion"
  )

  for (destino in names(mapa)) {
    origen <- buscar_columna(names(df), mapa[[destino]])
    if (!is.na(origen)) {
      names(df)[names(df) == origen] <- destino
    }
  }

  df
}

cargar_datos <- function(ruta_csv) {
  if (!file.exists(ruta_csv)) {
    stop("No se encontro el archivo: ", ruta_csv, call. = FALSE)
  }

  df <- tryCatch(
    read.csv(
      ruta_csv,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "UTF-8-BOM"
    ),
    error = function(e) {
      read.csv(
        ruta_csv,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        fileEncoding = "Latin1"
      )
    }
  )

  df <- renombrar_columnas(df)

  if (!"fecha" %in% names(df)) df$fecha <- NA
  if (!"barrio" %in% names(df)) df$barrio <- "NO REGISTRA"
  if (!"vehiculos" %in% names(df)) df$vehiculos <- 0
  if (!"heridos" %in% names(df)) df$heridos <- 0
  if (!"fallecidos" %in% names(df)) df$fallecidos <- 0
  if (!"direccion" %in% names(df)) df$direccion <- "NO REGISTRA"

  df$fecha <- convertir_fecha(df$fecha)
  df$anio <- year(df$fecha)
  df$mes <- month(df$fecha)

  df$barrio <- limpiar_texto(df$barrio, mayusculas = TRUE)
  df$direccion <- limpiar_texto(df$direccion)
  df$vehiculos <- convertir_numero(df$vehiculos)
  df$heridos <- convertir_numero(df$heridos)
  df$fallecidos <- convertir_numero(df$fallecidos)
  df$gravedad <- ifelse(df$heridos > 0, "Con heridos", "Sin heridos")

  df
}

# =========================
# FUNCIONES DE APOYO VISUAL
# =========================

formatear_numero <- function(x) {
  format(round(x, 0), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

contar_barrios <- function(df, top_n) {
  df %>%
    filter(!is.na(barrio), barrio != "") %>%
    count(barrio, sort = TRUE, name = "accidentes") %>%
    slice_head(n = top_n)
}

crear_matriz_calor <- function(df) {
  tabla <- df %>%
    filter(!is.na(anio), !is.na(mes)) %>%
    count(anio, mes, name = "accidentes")

  if (nrow(tabla) == 0) return(NULL)

  anios <- sort(unique(tabla$anio))
  meses <- 1:12
  matriz <- matrix(
    0,
    nrow = length(anios),
    ncol = length(meses),
    dimnames = list(as.character(anios), sprintf("%02d", meses))
  )

  for (i in seq_len(nrow(tabla))) {
    matriz[as.character(tabla$anio[i]), sprintf("%02d", tabla$mes[i])] <- tabla$accidentes[i]
  }

  matriz
}

tarjeta_indicador <- function(titulo, valor, detalle = NULL) {
  card(
    class = "metric-card",
    card_body(
      div(class = "metric-title", titulo),
      div(class = "metric-value", formatear_numero(valor)),
      if (!is.null(detalle)) div(class = "metric-detail", detalle)
    )
  )
}

validar_datos <- function(df, mensaje = "No hay datos con los filtros seleccionados.") {
  validate(need(nrow(df) > 0, mensaje))
}

eje_profesional <- function(titulo, tickangle = 0, showgrid = TRUE) {
  list(
    title = list(text = titulo, font = list(color = color_texto_suave, size = 12)),
    tickfont = list(color = color_texto_suave, size = 11),
    gridcolor = if (showgrid) color_grid else "rgba(0,0,0,0)",
    zerolinecolor = "rgba(148, 163, 184, 0.25)",
    linecolor = "rgba(148, 163, 184, 0.35)",
    ticks = "outside",
    tickcolor = "rgba(148, 163, 184, 0.45)",
    tickangle = tickangle
  )
}

aplicar_tema_plotly <- function(fig, titulo, x_title = "", y_title = "", showlegend = TRUE,
                                margin = list(l = 72, r = 28, t = 78, b = 58),
                                x_tickangle = 0, y_tickangle = 0) {
  fig %>%
    layout(
      title = list(
        text = paste0("<b>", titulo, "</b>"),
        x = 0,
        xanchor = "left",
        font = list(color = "#F8FAFC", size = 18)
      ),
      paper_bgcolor = color_fondo,
      plot_bgcolor = color_panel,
      font = list(family = "Segoe UI, Inter, Arial, sans-serif", color = color_texto),
      xaxis = eje_profesional(x_title, tickangle = x_tickangle),
      yaxis = eje_profesional(y_title, tickangle = y_tickangle),
      hoverlabel = list(
        bgcolor = "#020617",
        bordercolor = "rgba(148, 163, 184, 0.35)",
        font = list(color = "#F8FAFC", size = 12)
      ),
      legend = list(
        orientation = "h",
        x = 0,
        y = -0.18,
        xanchor = "left",
        font = list(color = color_texto_suave, size = 11),
        bgcolor = "rgba(0,0,0,0)"
      ),
      showlegend = showlegend,
      margin = margin
    ) %>%
    config(
      displaylogo = FALSE,
      responsive = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d")
    )
}

# =========================
# DATOS BASE
# =========================

datos <- cargar_datos(ruta)

barrios_iniciales <- sort(unique(datos$barrio[!is.na(datos$barrio)]))
max_barrios_inicial <- max(1, length(barrios_iniciales))
hay_fechas <- any(!is.na(datos$fecha))
fecha_min <- if (hay_fechas) min(as.Date(datos$fecha), na.rm = TRUE) else Sys.Date()
fecha_max <- if (hay_fechas) max(as.Date(datos$fecha), na.rm = TRUE) else Sys.Date()

# =========================
# INTERFAZ
# =========================

ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "darkly",
    primary = "#2563EB",
    bg = color_fondo,
    fg = color_texto
  ),

  tags$head(
    tags$style(
      HTML(
        "
        body { background-color: #111827; }
        .app-intro { color: #d1d5db; margin-bottom: 18px; }
        .metric-card {
          border: 1px solid rgba(255,255,255,0.08);
          background: #172033;
          min-height: 120px;
        }
        .metric-title {
          color: #aeb7c7;
          font-size: 0.9rem;
          text-transform: uppercase;
          letter-spacing: 0;
        }
        .metric-value {
          color: #ffffff;
          font-size: 2rem;
          font-weight: 700;
          line-height: 1.15;
          margin-top: 8px;
        }
        .metric-detail {
          color: #8fb3ff;
          font-size: 0.85rem;
          margin-top: 6px;
        }
        .filter-note, .empty-state {
          color: #cbd5e1;
          background: rgba(255,255,255,0.06);
          border-radius: 8px;
          padding: 10px 12px;
          margin-top: 12px;
        }
        .download-row {
          display: flex;
          justify-content: flex-end;
          margin-bottom: 12px;
        }
        .tab-content {
          background: #0F172A;
          border: 1px solid rgba(148, 163, 184, 0.18);
          border-top: 0;
          border-radius: 0 0 10px 10px;
          padding: 16px;
        }
        .nav-tabs {
          border-bottom-color: rgba(148, 163, 184, 0.22);
        }
        .nav-tabs .nav-link {
          color: #CBD5E1;
          border-radius: 8px 8px 0 0;
        }
        .nav-tabs .nav-link.active {
          color: #F8FAFC;
          background-color: #1E293B;
          border-color: rgba(148, 163, 184, 0.24);
          border-bottom-color: #1E293B;
        }
        .plotly.html-widget {
          border-radius: 10px;
          overflow: hidden;
        }
        "
      )
    )
  ),

  titlePanel("Visualizacion de Accidentes Viales - Shiny"),

  p(
    class = "app-intro",
    "Aplicacion interactiva para analizar accidentes viales, identificar barrios criticos y observar patrones en el tiempo."
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filtros"),

      dateRangeInput(
        "rango_fechas",
        "Selecciona rango de fechas:",
        start = fecha_min,
        end = fecha_max,
        min = fecha_min,
        max = fecha_max
      ),

      sliderInput(
        "top_n",
        "Cantidad de barrios a mostrar:",
        min = 1,
        max = max_barrios_inicial,
        value = min(10, max_barrios_inicial),
        step = 1
      ),

      selectizeInput(
        "barrios",
        "Filtrar por barrios especificos:",
        choices = barrios_iniciales,
        selected = character(0),
        multiple = TRUE,
        options = list(
          placeholder = "Dejalo vacio para usar todos",
          plugins = list("remove_button")
        )
      ),

      uiOutput("resumen_filtros")
    ),

    mainPanel(
      width = 9,
      h3("Indicadores principales"),
      uiOutput("indicadores"),

      br(),

      tabsetPanel(
        tabPanel("Barras", plotlyOutput("grafico_barras", height = "520px")),
        tabPanel("Linea", plotlyOutput("grafico_linea", height = "520px")),
        tabPanel("Histograma", plotlyOutput("grafico_histograma", height = "520px")),
        tabPanel("Dispersion", plotlyOutput("grafico_dispersion", height = "560px")),
        tabPanel("Dona", plotlyOutput("grafico_dona", height = "520px")),
        tabPanel("Caja", plotlyOutput("grafico_caja", height = "560px")),
        tabPanel("Mapa de calor", plotlyOutput("grafico_calor", height = "520px")),
        tabPanel(
          "Datos",
          div(class = "download-row", downloadButton("descargar_csv", "Descargar CSV filtrado")),
          DTOutput("tabla_datos")
        ),
        tabPanel("Hallazgos", uiOutput("hallazgos"))
      )
    )
  )
)

# =========================
# SERVIDOR
# =========================

server <- function(input, output, session) {

  datos_por_fecha <- reactive({
    df <- datos

    if (hay_fechas && !is.null(input$rango_fechas) && length(input$rango_fechas) == 2) {
      df <- df %>%
        filter(
          !is.na(fecha),
          as.Date(fecha) >= input$rango_fechas[1],
          as.Date(fecha) <= input$rango_fechas[2]
        )
    }

    df
  })

  observe({
    df <- datos_por_fecha()
    barrios_disponibles <- sort(unique(df$barrio[!is.na(df$barrio)]))
    max_barrios <- max(1, length(barrios_disponibles))

    seleccion_actual <- input$barrios %||% character(0)
    seleccion_actual <- intersect(seleccion_actual, barrios_disponibles)

    valor_top <- input$top_n %||% min(10, max_barrios)
    valor_top <- min(max(1, valor_top), max_barrios)

    updateSelectizeInput(
      session,
      "barrios",
      choices = barrios_disponibles,
      selected = seleccion_actual,
      server = TRUE
    )

    updateSliderInput(
      session,
      "top_n",
      min = 1,
      max = max_barrios,
      value = valor_top
    )
  })

  datos_filtrados <- reactive({
    df <- datos_por_fecha()
    barrios <- input$barrios %||% character(0)

    if (length(barrios) > 0) {
      df <- df %>% filter(barrio %in% barrios)
    }

    df
  })

  top_n_real <- reactive({
    df <- datos_filtrados()
    barrios_disponibles <- length(unique(df$barrio[!is.na(df$barrio)]))
    if (nrow(df) == 0 || barrios_disponibles == 0) return(0)
    min(input$top_n %||% barrios_disponibles, barrios_disponibles)
  })

  barrios_top_actual <- reactive({
    df <- datos_filtrados()
    if (nrow(df) == 0 || top_n_real() == 0) return(character(0))
    contar_barrios(df, top_n_real()) %>% pull(barrio)
  })

  datos_analisis <- reactive({
    df <- datos_filtrados()
    barrios_top <- barrios_top_actual()

    if (nrow(df) == 0) return(df)
    if (length(barrios_top) == 0) return(df[0, , drop = FALSE])

    df %>% filter(barrio %in% barrios_top)
  })

  output$resumen_filtros <- renderUI({
    df_fecha <- datos_por_fecha()
    df_filtrado <- datos_filtrados()
    df <- datos_analisis()
    top_real <- top_n_real()

    div(
      class = "filter-note",
      tags$strong("Resumen:"),
      tags$br(),
      paste0("Registros por filtros: ", formatear_numero(nrow(df_filtrado))),
      tags$br(),
      paste0("Registros en Top ", formatear_numero(top_real), ": ", formatear_numero(nrow(df))),
      tags$br(),
      paste0("Barrios mostrados: ", formatear_numero(length(unique(df$barrio[!is.na(df$barrio)])))),
      tags$br(),
      paste0("Barrios tras fecha: ", formatear_numero(length(unique(df_fecha$barrio[!is.na(df_fecha$barrio)]))))
    )
  })

  output$indicadores <- renderUI({
    df <- datos_analisis()

    fluidRow(
      column(3, tarjeta_indicador("Accidentes", nrow(df), "Registros del Top actual")),
      column(3, tarjeta_indicador("Barrios", length(unique(df$barrio[!is.na(df$barrio)])), "Barrios mostrados")),
      column(3, tarjeta_indicador("Heridos", sum(df$heridos, na.rm = TRUE), "Personas heridas")),
      column(3, tarjeta_indicador("Vehiculos", sum(df$vehiculos, na.rm = TRUE), "Vehiculos involucrados"))
    )
  })

  output$grafico_barras <- renderPlotly({
    df <- datos_filtrados()
    validar_datos(df)
    validate(need(top_n_real() > 0, "No hay barrios disponibles para mostrar."))

    df_top <- contar_barrios(df, top_n_real()) %>% arrange(accidentes)
    df_top$barrio <- factor(df_top$barrio, levels = df_top$barrio)

    plot_ly(
      df_top,
      x = ~accidentes,
      y = ~barrio,
      type = "bar",
      orientation = "h",
      text = ~formatear_numero(accidentes),
      textposition = "outside",
      cliponaxis = FALSE,
      marker = list(
        color = df_top$accidentes,
        colorscale = escala_barras,
        showscale = TRUE,
        line = list(color = "rgba(255,255,255,0.18)", width = 1),
        colorbar = list(
          title = list(text = "Accidentes", font = list(color = color_texto_suave, size = 11)),
          tickfont = list(color = color_texto_suave, size = 10),
          outlinewidth = 0
        )
      ),
      hovertemplate = "Barrio: %{y}<br>Accidentes: %{x}<extra></extra>"
    ) %>%
      aplicar_tema_plotly(
        titulo = paste0("Top ", nrow(df_top), " barrios con mas accidentes"),
        x_title = "Cantidad de accidentes",
        y_title = "Barrio",
        showlegend = FALSE,
        margin = list(l = 128, r = 90, t = 78, b = 54)
      )
  })

  output$grafico_linea <- renderPlotly({
    df <- datos_analisis() %>%
      filter(!is.na(fecha)) %>%
      mutate(mes_fecha = floor_date(fecha, "month")) %>%
      count(mes_fecha, name = "accidentes")

    validate(need(nrow(df) > 0, "No hay fechas validas para graficar."))

    plot_ly(
      df,
      x = ~mes_fecha,
      y = ~accidentes,
      type = "scatter",
      mode = "lines+markers",
      fill = "tozeroy",
      fillcolor = "rgba(56, 189, 248, 0.10)",
      line = list(color = color_linea, width = 3.5, shape = "spline"),
      marker = list(
        color = color_acento,
        size = 9,
        line = list(color = color_panel, width = 2)
      ),
      hovertemplate = "Mes: %{x|%Y-%m}<br>Accidentes: %{y}<extra></extra>"
    ) %>%
      aplicar_tema_plotly(
        titulo = paste0("Evolucion mensual de accidentes viales - Top ", top_n_real(), " barrios"),
        x_title = "Fecha",
        y_title = "Cantidad de accidentes",
        showlegend = FALSE
      )
  })

  output$grafico_histograma <- renderPlotly({
    df <- datos_analisis()
    validar_datos(df)

    plot_ly(
      df,
      x = ~heridos,
      color = ~gravedad,
      colors = colores_gravedad,
      type = "histogram",
      opacity = 0.86,
      marker = list(line = list(color = "rgba(255,255,255,0.16)", width = 1)),
      hovertemplate = "Heridos: %{x}<br>Accidentes: %{y}<extra></extra>"
    ) %>%
      aplicar_tema_plotly(
        titulo = paste0("Distribucion de heridos por accidente - Top ", top_n_real(), " barrios"),
        x_title = "Numero de heridos",
        y_title = "Frecuencia",
        margin = list(l = 74, r = 28, t = 78, b = 92)
      ) %>%
      layout(
        barmode = "overlay",
        bargap = 0.08
      )
  })

  output$grafico_dispersion <- renderPlotly({
    df <- datos_analisis()
    validar_datos(df)

    campo_color <- if (length(unique(df$barrio[!is.na(df$barrio)])) <= 12) "barrio" else "gravedad"
    colores <- if (campo_color == "barrio") paleta_barrio else colores_gravedad
    titulo_color <- if (campo_color == "barrio") "Barrio" else "Gravedad"

    plot_ly(
      df,
      x = ~vehiculos,
      y = ~heridos,
      color = as.formula(paste0("~", campo_color)),
      colors = colores,
      size = ~pmax(heridos, 1),
      sizes = c(7, 24),
      type = "scatter",
      mode = "markers",
      text = ~paste(
        "Barrio:", barrio,
        "<br>Direccion:", direccion,
        "<br>Fecha:", ifelse(is.na(fecha), "NO REGISTRA", as.character(as.Date(fecha))),
        "<br>Heridos:", heridos,
        "<br>Vehiculos:", vehiculos
      ),
      hoverinfo = "text",
      marker = list(
        opacity = 0.78,
        line = list(width = 0.8, color = "rgba(255,255,255,0.58)")
      )
    ) %>%
      aplicar_tema_plotly(
        titulo = paste0("Relacion entre vehiculos y heridos - Top ", top_n_real(), " barrios"),
        x_title = "Vehiculos involucrados",
        y_title = "Personas heridas",
        margin = list(l = 74, r = 28, t = 78, b = 112)
      ) %>%
      layout(
        legend = list(
          title = list(text = titulo_color, font = list(color = color_texto_suave, size = 11)),
          orientation = "h",
          x = 0,
          y = -0.24,
          font = list(color = color_texto_suave, size = 10),
          bgcolor = "rgba(0,0,0,0)"
        )
      )
  })

  output$grafico_dona <- renderPlotly({
    df <- datos_analisis()
    validar_datos(df)

    resumen <- df %>%
      count(gravedad, name = "accidentes") %>%
      arrange(match(gravedad, c("Sin heridos", "Con heridos")))

    plot_ly(
      resumen,
      labels = ~gravedad,
      values = ~accidentes,
      type = "pie",
      hole = 0.45,
      sort = FALSE,
      direction = "clockwise",
      marker = list(
        colors = unname(colores_gravedad[resumen$gravedad]),
        line = list(color = color_fondo, width = 3)
      ),
      textinfo = "label+percent",
      textfont = list(color = "#F8FAFC", size = 13),
      hovertemplate = "%{label}<br>Accidentes: %{value}<br>%{percent}<extra></extra>"
    ) %>%
      aplicar_tema_plotly(
        titulo = paste0("Proporcion de accidentes con y sin heridos - Top ", top_n_real(), " barrios"),
        showlegend = TRUE,
        margin = list(l = 28, r = 28, t = 78, b = 86)
      ) %>%
      layout(
        annotations = list(
          list(
            text = paste0("<b>", formatear_numero(sum(resumen$accidentes)), "</b><br>accidentes"),
            x = 0.5,
            y = 0.5,
            showarrow = FALSE,
            font = list(color = color_texto, size = 15)
          )
        )
      )
  })

  output$grafico_caja <- renderPlotly({
    df <- datos_analisis()
    validar_datos(df)
    validate(need(top_n_real() > 0, "No hay barrios disponibles para mostrar."))

    barrios_top <- barrios_top_actual()
    df_box <- df
    df_box$barrio <- factor(df_box$barrio, levels = barrios_top)

    plot_ly(
      df_box,
      x = ~barrio,
      y = ~heridos,
      color = ~barrio,
      colors = paleta_barrio,
      type = "box",
      boxpoints = "suspectedoutliers",
      boxmean = TRUE,
      jitter = 0.32,
      pointpos = 0,
      marker = list(
        size = 5,
        opacity = 0.62,
        line = list(color = "rgba(255,255,255,0.45)", width = 0.5)
      ),
      line = list(width = 1.5)
    ) %>%
      aplicar_tema_plotly(
        titulo = paste0("Distribucion de heridos en los ", length(barrios_top), " barrios principales"),
        x_title = "Barrio",
        y_title = "Numero de heridos",
        showlegend = FALSE,
        margin = list(l = 74, r = 28, t = 78, b = 150),
        x_tickangle = -35
      )
  })

  output$grafico_calor <- renderPlotly({
    df <- datos_analisis()
    validar_datos(df)

    matriz <- crear_matriz_calor(df)
    validate(need(!is.null(matriz), "No hay fechas validas para crear el mapa de calor."))

    plot_ly(
      x = colnames(matriz),
      y = rownames(matriz),
      z = matriz,
      type = "heatmap",
      colorscale = escala_calor,
      zsmooth = FALSE,
      colorbar = list(
        title = list(text = "Accidentes", font = list(color = color_texto_suave, size = 11)),
        tickfont = list(color = color_texto_suave, size = 10),
        outlinewidth = 0
      ),
      hovertemplate = "Mes: %{x}<br>Anio: %{y}<br>Accidentes: %{z}<extra></extra>"
    ) %>%
      aplicar_tema_plotly(
        titulo = paste0("Mapa de calor de accidentes por anio y mes - Top ", top_n_real(), " barrios"),
        x_title = "Mes",
        y_title = "Anio",
        showlegend = FALSE,
        margin = list(l = 72, r = 92, t = 78, b = 58)
      )
  })

  output$tabla_datos <- renderDT({
    df <- datos_analisis()

    if (nrow(df) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos con los filtros seleccionados."), rownames = FALSE))
    }

    datatable(
      df,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })

  output$descargar_csv <- downloadHandler(
    filename = function() {
      paste0("accidentes_viales_filtrados_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(datos_analisis(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$hallazgos <- renderUI({
    df <- datos_analisis()

    if (nrow(df) == 0) {
      return(div(class = "empty-state", "No hay datos con los filtros seleccionados. Ajusta el rango de fechas o los barrios."))
    }

    barrio_mayor <- contar_barrios(df, 1)
    texto_barrio <- if (nrow(barrio_mayor) > 0) barrio_mayor$barrio[1] else "No disponible"
    cantidad_barrio <- if (nrow(barrio_mayor) > 0) barrio_mayor$accidentes[1] else 0

    HTML(paste0(
      "<h4>Hallazgos principales</h4>",
      "<ol>",
      "<li>Se analizaron <b>", formatear_numero(nrow(df)), "</b> registros de accidentes viales segun los filtros seleccionados.</li>",
      "<li>El barrio con mayor numero de accidentes es <b>", texto_barrio, "</b>, con <b>", formatear_numero(cantidad_barrio), "</b> registros.</li>",
      "<li>Se registran <b>", formatear_numero(sum(df$heridos, na.rm = TRUE)), "</b> personas heridas.</li>",
      "<li>Se reportan <b>", formatear_numero(sum(df$vehiculos, na.rm = TRUE)), "</b> vehiculos involucrados.</li>",
      "<li>El Top aplicado en las graficas de barrios es <b>", formatear_numero(top_n_real()), "</b>.</li>",
      "<li>La linea temporal y el mapa de calor ayudan a ubicar periodos con mayor concentracion de accidentes.</li>",
      "</ol>"
    ))
  })
}

shinyApp(ui = ui, server = server)

