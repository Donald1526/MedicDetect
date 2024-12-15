
library(shiny)
library(isotree)
library(ggplot2)
library(janitor)
library(DT)
library(plotly)
library(rmarkdown)
library(pagedown)


# Aumentar el límite de tamaño del archivo
options(shiny.maxRequestSize = 30 * 1024^2)  # Hasta 30 MB

# Interfaz de Usuario
ui <- navbarPage(
  title = div(
    img(src = "logo.webp", height = "30px", style = "margin-right: 0px;"), # Agrega el logo al título
    "MedDetect"
  ),
  
  # Ventana 1: Cargar Archivo
  tabPanel(
    "Cargar Archivo",
    fluidPage(
      titlePanel("Carga de Archivo CSV"),
      sidebarLayout(
        sidebarPanel(
          fileInput("file", "Sube tu archivo CSV", accept = c(".csv")),
          radioButtons("sep", "Selecciona el delimitador:",
                       choices = c("Coma (,)" = ",", "Punto y coma (;)" = ";", "Tabulación (\\t)" = "\t"),
                       selected = ";"),
          textOutput("file_info"),
          uiOutput("warning_message")
        ),
        mainPanel(
          h4("Vista previa de los datos"),
          DT::dataTableOutput("preview")
        )
      )
    )
  ),
  
  # Ventana 2: Seleccionar columnas, umbral y entrenar modelo
  tabPanel(
    "Entrenar Modelo",
    fluidPage(
      titlePanel("Selecciona las Columnas, Ajusta el Umbral y Entrena el Modelo"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("column_selector"),  # Selector de columnas dinámico
          sliderInput("threshold", "Selecciona el Umbral de Anomalía:",
                      min = 0, max = 1, value = 0.6, step = 0.05),  # Umbral ajustable
          actionButton("train", "Entrenar Modelo")
        ),
        mainPanel(
          h4("Información del Modelo"),
          verbatimTextOutput("model_info")
        )
      )
    )
  ),
  
  # Ventana 3: Visualización
  tabPanel(
    "Visualización",
    fluidPage(
      titlePanel("Visualización de Anomalías"),
      sidebarLayout(
        sidebarPanel(
          h4("Seleccione las Variables para el Gráfico"),
          uiOutput("variable_selector"),
          p("Visualización interactiva de las anomalías detectadas.")
        ),
        mainPanel(
          plotlyOutput("outlier_plot")
        )
      )
    )
  ),
  
  # Ventana 4: Formulario para nuevos datos
  tabPanel(
    "Formulario de Predicción",
    fluidPage(
      titlePanel("Formulario para Detección de Anomalías"),
      sidebarLayout(
        sidebarPanel(
          h4("Ingrese los datos"),
          uiOutput("dynamic_form"),  # Formulario dinámico
          actionButton("check_anomaly", "Verificar Anomalía")
        ),
        mainPanel(
          h4("Resultado"),
          textOutput("anomaly_result")
        )
      )
    )
  ),
  
  # Ventana 5: Datos Anómalos y Medias
  tabPanel(
    "Lista de Datos Anómalos",
    fluidPage(
      titlePanel("Lista de Datos Anómalos y Cálculo de Medias"),
      sidebarLayout(
        sidebarPanel(
          h4("Información"),
          p("Esta tabla lista todos los datos que fueron detectados como anómalos, junto con todas sus columnas."),
          p("Además, permite descargar los datos anómalos en un archivo PDF."),
          downloadButton("download_pdf", "Descargar Datos Anómalos (PDF)")
        ),
        mainPanel(
          DT::dataTableOutput("anomalies_table"),
          h4("Medias Calculadas"),
          verbatimTextOutput("mean_calculations")
        )
      )
    )
  )
)

# Lógica del Servidor
server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
  model <- reactiveVal(NULL)
  selected_columns <- reactiveVal(NULL)
  dataset_with_scores <- reactiveVal(NULL)
  warning_message <- reactiveVal(NULL)
  
  # Ventana 1: Cargar archivo CSV
  observeEvent(input$file, {
    req(input$file, input$sep)
    data <- tryCatch({
      read.csv(input$file$datapath, sep = input$sep, fileEncoding = "UTF-8", header = TRUE)
    }, warning = function(w) {
      warning_message("Advertencia: Algunos datos podrían no haberse leído correctamente.")
      NULL
    }, error = function(e) {
      warning_message("Error: No se pudo leer el archivo. Verifica el delimitador o el formato del archivo.")
      NULL
    })
    if (!is.null(data)) {
      warning_message(NULL)
      data <- clean_names(data)
      dataset(data)
    } else {
      dataset(NULL)
    }
  })
  
  # Mostrar advertencias si las hay
  output$warning_message <- renderUI({
    if (!is.null(warning_message())) {
      div(style = "color: red;", warning_message())
    }
  })
  
  # Mostrar información del archivo cargado
  output$file_info <- renderText({
    req(input$file)
    paste("Archivo cargado:", input$file$name)
  })
  
  # Mostrar vista previa interactiva del dataset
  output$preview <- DT::renderDataTable({
    req(dataset())
    DT::datatable(
      dataset(),
      options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 15),
        scrollX = TRUE
      )
    )
  })
  
  # Ventana 2: Selector dinámico de columnas
  output$column_selector <- renderUI({
    req(dataset())
    checkboxGroupInput("columns", "Selecciona las columnas para entrenar",
                       choices = colnames(dataset()))
  })
  
  # Entrenar modelo
  observeEvent(input$train, {
    req(dataset(), input$columns)
    data_for_training <- dataset()[, input$columns, drop = FALSE]
    model(isolation.forest(data_for_training, ntrees = 300))
    selected_columns(input$columns)
    
    # Calcular puntuaciones de anomalías
    anomaly_scores <- predict(model(), data_for_training, type = "score")
    data_with_scores <- dataset()
    data_with_scores$Anomaly_Score <- anomaly_scores
    threshold <- input$threshold
    data_with_scores$Anomaly <- ifelse(anomaly_scores > threshold, "Sí", "No")
    dataset_with_scores(data_with_scores)
  })
  
  # Mostrar información del modelo
  output$model_info <- renderPrint({
    req(model())
    paste("Modelo entrenado con las columnas:", paste(selected_columns(), collapse = ", "),
          "\nUmbral de Anomalía:", input$threshold)
  })
  
  # Ventana 3: Selección de variables y visualización
  output$variable_selector <- renderUI({
    req(dataset_with_scores())
    selectInput("variables", "Selecciona las Variables (Máx. 3)", 
                choices = names(dataset_with_scores()), 
                multiple = TRUE, 
                selected = head(names(dataset_with_scores()), 3))
  })
  
  output$outlier_plot <- renderPlotly({
    req(input$variables, length(input$variables) > 1)
    data_with_scores <- dataset_with_scores()
    variables <- input$variables
    
    if (length(variables) == 3) {
      plot_ly(
        data = data_with_scores,
        x = ~get(variables[1]),
        y = ~get(variables[2]),
        z = ~get(variables[3]),
        color = ~Anomaly,
        colors = c("blue", "red"),
        type = "scatter3d",
        mode = "markers"
      )
    } else if (length(variables) == 2) {
      gg <- ggplot(data_with_scores, aes_string(x = variables[1], y = variables[2], color = "Anomaly")) +
        geom_point(size = 3) +
        scale_color_manual(values = c("No" = "blue", "Sí" = "red")) +
        labs(title = "Visualización 2D de Anomalías") +
        theme_minimal()
      ggplotly(gg)
    } else {
      plotly_empty() %>% layout(title = "Seleccione al menos 2 variables para graficar.")
    }
  })
  # Ventana 4: Formulario dinámico para predicción
  output$dynamic_form <- renderUI({
    req(selected_columns())
    lapply(selected_columns(), function(col) {
      numericInput(
        inputId = paste0("input_", col), 
        label = col, 
        value = NULL, 
        step = 0.01
      )
    })
  })
  
  observeEvent(input$check_anomaly, {
    req(selected_columns(), model())
    new_data <- as.data.frame(t(sapply(selected_columns(), function(col) {
      input[[paste0("input_", col)]]
    })))
    colnames(new_data) <- selected_columns()
    
    anomaly_score <- predict(model(), new_data, type = "score")
    result <- ifelse(anomaly_score > input$threshold, "Es una anomalía", "Es normal")
    output$anomaly_result <- renderText({ result })
  })
  
  # Ventana 5: Descargar datos anómalos en PDF
  output$anomalies_table <- DT::renderDataTable({
    req(dataset_with_scores())
    anomalies <- dataset_with_scores()[dataset_with_scores()$Anomaly == "Sí", ]
    DT::datatable(anomalies, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("reporte_anomalias", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(dataset_with_scores())
      anomalies <- dataset_with_scores()[dataset_with_scores()$Anomaly == "Sí", ]
      
      # Verificar si hay datos anómalos
      print("Contenido de anomalies:")
      print(anomalies)
      
      if (nrow(anomalies) == 0) {
        stop("No hay datos anómalos para generar el informe.")
      }
      
      # Renderizar el R Markdown
      temp_html <- tempfile(fileext = ".html")
      rmarkdown::render(
        "anomalies_report.Rmd",
        output_file = temp_html,
        params = list(data = anomalies),
        envir = new.env(parent = globalenv())
      )
      
      # Convertir HTML a PDF
      pagedown::chrome_print(input = temp_html, output = file)
    }
  )
  output$mean_calculations <- renderPrint({
    req(dataset_with_scores())
    data_with_scores <- dataset_with_scores()
    global_means <- colMeans(data_with_scores[, selected_columns(), drop = FALSE])
    without_anomalies <- data_with_scores[data_with_scores$Anomaly == "No", ]
    means_without_anomalies <- colMeans(without_anomalies[, selected_columns(), drop = FALSE])
    
    list(
      "Medias Globales" = global_means,
      "Medias Sin Anomalías" = means_without_anomalies
    )
  })
}

# Ejecutar aplicación
shinyApp(ui, server)
