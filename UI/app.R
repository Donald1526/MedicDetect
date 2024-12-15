
library(shiny)
library(isotree)
library(ggplot2)
library(janitor)
library(DT)
library(plotly)
library(rmarkdown)
library(pagedown)


options(shiny.maxRequestSize = 30 * 1024^2) 
# Interfaz de Usuario
ui <- navbarPage(
  title = div(
    img(src = "logo.jpg", height = "30px", style = "margin-right: 0px;"), 
    "MedDetect"
  ),
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('logo.jpg');
        background-size: 30%;  /* Ajustar imagen al tamaño de la ventana */
        background-attachment: fixed;
        background-position: center;
        background-repeat: no-repeat;
        opacity: 1.0; /* Opacidad del fondo */
      }
      .navbar {
        background-color: rgba(255, 255, 255, 0.8) !important; /* Translucidez del navbar */
      }
      .well {
        background-color: rgba(255, 255, 255, 0.8); /* Translucidez de paneles laterales */
        border-radius: 10px;
      }
      .tab-pane {
        background-color: rgba(255, 255, 255, 0.8); /* Translucidez del contenido de pestañas */
        border-radius: 10px;
      }
    "))
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
          p("En esta tabla se encuentran los pacientes cuyos datos son anomalos."),
          p("Descarga sus datos."),
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
  
  output$warning_message <- renderUI({
    if (!is.null(warning_message())) {
      div(style = "color: red;", warning_message())
    }
  })
  
  output$file_info <- renderText({
    req(input$file)
    paste("Archivo cargado:", input$file$name)
  })
  
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
  
  observeEvent(input$train, {
    req(dataset(), input$columns)
    data_for_training <- dataset()[, input$columns, drop = FALSE]
    model(isolation.forest(data_for_training, ntrees = 300))
    selected_columns(input$columns)
    
    anomaly_scores <- predict(model(), data_for_training, type = "score")
    data_with_scores <- dataset()
    data_with_scores$Anomaly_Score <- anomaly_scores
    threshold <- input$threshold
    data_with_scores$Anomaly <- ifelse(anomaly_scores > threshold, "Sí", "No")
    dataset_with_scores(data_with_scores)
  })
  
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
    req(selected_columns())  # Requiere columnas seleccionadas
    lapply(selected_columns(), function(col) {
      numericInput(
        inputId = paste0("input_", col), 
        label = paste("Ingrese valor para:", col), 
        value = NULL, 
        step = 0.01
      )
    })
  })
  
  observeEvent(input$check_anomaly, {
    req(selected_columns(), model())
    
    user_input <- sapply(selected_columns(), function(col) {
      input[[paste0("input_", col)]]
    })
    
    new_data <- as.data.frame(t(user_input))
    colnames(new_data) <- selected_columns()
    
    print("Valores ingresados en el formulario:")
    print(new_data)
    
    anomaly_score <- predict(model(), new_data, type = "score")
    threshold <- input$threshold  # Umbral configurado por el usuario
    
    print(paste("Puntaje de Anomalía:", anomaly_score))
    print(paste("Umbral:", threshold))
    
    result <- ifelse(anomaly_score > threshold, "¡Es una anomalía!", "Es normal")
    
    output$anomaly_result <- renderText({
      paste("Resultado:", result, "\nPuntaje de Anomalía:", round(anomaly_score, 4))
    })
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
      
      print("Contenido de anomalies:")
      print(anomalies)
      
      if (nrow(anomalies) == 0) {
        stop("No hay datos anómalos para generar el informe.")
      }
      
      temp_html <- tempfile(fileext = ".html")
      rmarkdown::render(
        "anomalies_report.Rmd",
        output_file = temp_html,
        params = list(data = anomalies),
        envir = new.env(parent = globalenv())
      )
      
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
