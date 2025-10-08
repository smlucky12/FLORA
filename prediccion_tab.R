# ============================
# MODULO DE PREDICCION NDVI - COMPLETO
# ============================

library(shiny)
library(dplyr)
library(ggplot2)
library(forecast)
library(tidyr)
library(ggfortify)
library(lubridate)
library(DT)
library(lmtest)

# Función para obtener el string del modelo ARIMA
arima_string <- function(fit) {
  if (is.null(fit)) return("Not available")
  order <- fit$arma
  return(paste0("ARIMA(", order[1], ",", order[6], ",", order[2], ")",
                ifelse(order[3] > 0, paste0("(", order[3], ",", order[7], ",", order[4], ")"), "")))
}

# --- UI ---
prediccion_tab_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("NDVI PREDICTION",
           sidebarLayout(
             sidebarPanel(
               selectInput(ns("rect_id"), "SELECT AREA (RECT_ID):", choices = NULL),
               numericInput(ns("horizonte"), "MONTHS TO PREDICT:", value = 6, min = 1, max = 24),
               actionButton(ns("calcular_prediccion"), "ANALYZE AND PREDICT"),
               width = 3,
               actionButton(ns("btn_tutorial_grafico1"), "View Graph Tutorial 1", 
                            icon = icon("chart-line"), 
                            class = "btn-info"),
               actionButton(ns("btn_tutorial_grafico2"), "View Graph Tutorial 2", 
                            icon = icon("chart-line"), 
                            class = "btn-info"),
               actionButton(ns("btn_tutorial_grafico3"), "View Graph Tutorial 3", 
                            icon = icon("chart-line"), 
                            class = "btn-info"),
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("VISUALIZATION",
                          plotOutput(ns("grafico_ajuste"), height = 300),
                          plotOutput(ns("grafico_stl"), height = 300),
                          plotOutput(ns("grafico_forecast"), height = 300)
                 ),
                 tabPanel("PREDICTION TABLE",
                          DTOutput(ns("tabla_predicciones")),
                          downloadButton(ns("descargar_predicciones"), "Download Predictions")
                 )
               )
             )
           )
  )
}

# --- SERVER ---
prediccion_tab_server <- function(id, ndvi_result) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive to store predictions
    predicciones_arima <- reactiveVal(NULL)
    predicciones_lineal <- reactiveVal(NULL)
    predicciones_cuadratico <- reactiveVal(NULL)
    
    # --- UPDATE RECT_ID LIST ---
    observe({
      data <- ndvi_result()
      req(data)
      
      if (!is.null(data) && nrow(data) > 0) {
        rect_ids <- sort(unique(data$rect_id))
        updateSelectInput(session, "rect_id", 
                          choices = rect_ids,
                          selected = rect_ids[1])
      }
    })
    
    # --- PREPARE DATA FOR TIME SERIES ---
    datos_serie <- reactive({
      data <- ndvi_result()
      req(data, input$rect_id)
      
      # Filter by selected rect_id and sort by date
      series <- data %>%
        filter(rect_id == as.numeric(input$rect_id)) %>%
        arrange(year, month, day)
      
      # Create complete date
      series <- series %>%
        mutate(
          date = make_date(year, month, day),
          t = row_number()  # Sequential time variable
        ) %>%
        filter(!is.na(date), !is.na(ndvi))
      
      return(series)
    })
    
    # --- CALCULATE PREDICTION ---
    observeEvent(input$calcular_prediccion, {
      req(datos_serie())
      
      series <- datos_serie()
      
      # Validations
      if (nrow(series) == 0) {
        showNotification("No valid data for selected area.", type = "error")
        return()
      }
      
      if (nrow(series) < 12) {
        showNotification("At least 12 observations are needed for predictions.", 
                         type = "warning")
      }
      
      # --- CREATE MONTHLY DUMMIES ---
      series$month_factor <- factor(month(series$date))
      dummies <- model.matrix(~ month_factor - 1, data = series)
      series <- cbind(series, dummies)
      
      # --- LINEAR AND QUADRATIC FITS ---
      series$t2 <- series$t^2
      
      # Linear model with monthly dummies
      modelo_lin <- tryCatch({
        lm(ndvi ~ t + ., data = series[, c("ndvi", "t", colnames(dummies))])
      }, error = function(e) {
        showNotification("Error fitting linear model", type = "warning")
        NULL
      })
      
      # Quadratic model with monthly dummies
      modelo_cuad <- tryCatch({
        lm(ndvi ~ t + t2 + ., data = series[, c("ndvi", "t", "t2", colnames(dummies))])
      }, error = function(e) {
        showNotification("Error fitting quadratic model", type = "warning")
        NULL
      })
      
      # --- ARIMA MODEL ---
      ts_data <- ts(series$ndvi, frequency = 12)
      fit_arima <- NULL
      
      if (nrow(series) >= 12) {
        fit_arima <- tryCatch({
          auto.arima(ts_data)
        }, error = function(e) {
          showNotification("Error fitting ARIMA model", type = "warning")
          NULL
        })
        
        if (!is.null(fit_arima)) {
          forecast_arima <- forecast(fit_arima, h = input$horizonte)
          
          # Create ARIMA predictions table
          predicciones_arima_df <- data.frame(
            periodo = 1:input$horizonte,
            prediccion = as.numeric(forecast_arima$mean),
            limite_inferior = as.numeric(forecast_arima$lower[, 2]),
            limite_superior = as.numeric(forecast_arima$upper[, 2]),
            modelo = "ARIMA"
          )
          predicciones_arima(predicciones_arima_df)
        }
      }
      
      # --- LINEAR MODEL PREDICTIONS ---
      if (!is.null(modelo_lin)) {
        # Create future data for linear prediction
        futuro_lin <- data.frame(
          t = (max(series$t) + 1):(max(series$t) + input$horizonte)
        )
        
        # Add monthly dummies
        ultimo_mes <- tail(series$month_factor, 1)
        for (mes_col in colnames(dummies)) {
          futuro_lin[[mes_col]] <- ifelse(mes_col == paste0("month_factor", ultimo_mes), 1, 0)
        }
        
        predicciones_lin <- predict(modelo_lin, newdata = futuro_lin, interval = "prediction")
        predicciones_lineal(data.frame(
          periodo = 1:input$horizonte,
          t = futuro_lin$t,
          prediccion = predicciones_lin[, "fit"],
          limite_inferior = predicciones_lin[, "lwr"],
          limite_superior = predicciones_lin[, "upr"],
          modelo = "Linear"
        ))
      }
      
      if (!is.null(modelo_cuad)) {
        # Create future data for quadratic prediction
        futuro_cuad <- data.frame(
          t = (max(series$t) + 1):(max(series$t) + input$horizonte),
          t2 = ((max(series$t) + 1):(max(series$t) + input$horizonte))^2
        )
        
        # Add monthly dummies
        for (mes_col in colnames(dummies)) {
          futuro_cuad[[mes_col]] <- ifelse(mes_col == paste0("month_factor", ultimo_mes), 1, 0)
        }
        
        predicciones_cuad <- predict(modelo_cuad, newdata = futuro_cuad, interval = "prediction")
        predicciones_cuadratico(data.frame(
          periodo = 1:input$horizonte,
          t = futuro_cuad$t,
          prediccion = predicciones_cuad[, "fit"],
          limite_inferior = predicciones_cuad[, "lwr"],
          limite_superior = predicciones_cuad[, "upr"],
          modelo = "Quadratic"
        ))
      }
      
      # --- GRAPH: TREND COMPARISON ---
      output$grafico_ajuste <- renderPlot({
        p <- ggplot(series, aes(x = t, y = ndvi)) +
          geom_point(color = "darkgreen", alpha = 0.7) +
          geom_line(color = "darkgreen", alpha = 0.5) +
          labs(title = paste("NDVI TREND - Area", input$rect_id),
               x = "TIME (OBSERVATION ORDER)",
               y = "AVERAGE NDVI") +
          theme_minimal(base_size = 12)
        
        # Add trend lines if models were fitted correctly
        if (!is.null(modelo_lin)) {
          p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, 
                               color = "blue", linetype = "dashed", linewidth = 1)
        }
        if (!is.null(modelo_cuad)) {
          p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, 
                               color = "red", linewidth = 1)
        }
        
        if (!is.null(modelo_lin) && !is.null(modelo_cuad)) {
          p <- p + labs(subtitle = "Linear (blue) vs Quadratic (red)")
        } else if (!is.null(modelo_lin)) {
          p <- p + labs(subtitle = "Linear trend (blue)")
        } else if (!is.null(modelo_cuad)) {
          p <- p + labs(subtitle = "Quadratic trend (red)")
        }
        
        p
      })
      
      # --- GRAPH: STL DECOMPOSITION ---
      output$grafico_stl <- renderPlot({
        # Create time series
        ts_data <- ts(series$ndvi, frequency = 12)
        
        # Verify enough data for STL
        if (length(ts_data) >= 24) {
          stl_fit <- stl(ts_data, s.window = "periodic")
          autoplot(stl_fit) + 
            ggtitle(paste("STL DECOMPOSITION OF NDVI - Area", input$rect_id))
        } else {
          # Alternative graph if insufficient data
          plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE,
               xlim = c(0, 1), ylim = c(0, 1))
          text(0.5, 0.5, 
               paste("At least 24 observations needed\nfor STL decomposition\n",
                     "Current observations:", length(ts_data)), 
               cex = 1.2, col = "red")
        }
      })
      
      # --- GRAPH: ARIMA PREDICTION ---
      output$grafico_forecast <- renderPlot({
        ts_data <- ts(series$ndvi, frequency = 12)
        
        # Fit ARIMA model
        fit <- tryCatch({
          auto.arima(ts_data)
        }, error = function(e) {
          NULL
        })
        
        if (!is.null(fit) && nrow(series) >= 12) {
          forecast_data <- forecast(fit, h = input$horizonte)
          
          autoplot(forecast_data) +
            ggtitle(paste("NDVI PREDICTION (ARIMA) - Area", input$rect_id, "-", 
                          input$horizonte, "MONTHS")) +
            ylab("AVERAGE NDVI") +
            xlab("TIME") +
            theme_minimal(base_size = 12)
        } else {
          # Alternative graph if ARIMA fails
          plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE,
               xlim = c(0, 1), ylim = c(0, 1))
          text(0.5, 0.5, 
               paste("Could not fit ARIMA model\n",
                     "Available observations:", nrow(series), "\n",
                     "Minimum recommended: 12"), 
               cex = 1.2, col = "red")
        }
      })
      
      # --- PREDICTIONS TABLE ---
      output$tabla_predicciones <- renderDT({
        # Verify at least one model has predictions
        has_data <- FALSE
        if (!is.null(predicciones_arima()) && nrow(predicciones_arima()) > 0) has_data <- TRUE
        if (!has_data) {
          return(datatable(data.frame(Message = "No predictions available. Run analysis first."), 
                           options = list(dom = 't'), rownames = FALSE))
        }
        
        # Combine all predictions
        all_predictions <- list()
        
        if (!is.null(predicciones_arima()) && nrow(predicciones_arima()) > 0) {
          all_predictions[["ARIMA"]] <- predicciones_arima()
        }
        
        # Combine into single dataframe
        df_final <- bind_rows(all_predictions, .id = "modelo") %>%
          mutate(across(where(is.numeric), ~ round(., 4))) %>%
          select(periodo, prediccion, limite_inferior, limite_superior)
        
        datatable(df_final, 
                  options = list(
                    pageLength = 10,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel')
                  ),
                  rownames = FALSE,
                  caption = paste("NDVI Predictions - Area", input$rect_id)) %>%
          formatStyle('prediccion', 
                      backgroundColor = styleInterval(
                        c(0.3, 0.6),
                        c("#ef989c", "#fbcc68", "#60c9a3")
                      ),
                      color = "black",
                      fontWeight = "bold")
      })
      
      # --- DOWNLOAD PREDICTIONS ---
      output$descargar_predicciones <- downloadHandler(
        filename = function() {
          paste0("ndvi_predictions_area_", input$rect_id, "_", Sys.Date(), ".csv")
        },
        content = function(file) {
          # Combine all predictions
          all_predictions <- list()
          
          if (!is.null(predicciones_arima()) && nrow(predicciones_arima()) > 0) {
            all_predictions[["ARIMA"]] <- predicciones_arima()
          }
          if (!is.null(predicciones_lineal()) && nrow(predicciones_lineal()) > 0) {
            all_predictions[["Linear"]] <- predicciones_lineal()
          }
          if (!is.null(predicciones_cuadratico()) && nrow(predicciones_cuadratico()) > 0) {
            all_predictions[["Quadratic"]] <- predicciones_cuadratico()
          }
          
          # If no predictions, create empty dataframe
          if (length(all_predictions) == 0) {
            df_final <- data.frame(Message = "No predictions available")
          } else {
            df_final <- bind_rows(all_predictions, .id = "modelo") %>%
              mutate(across(where(is.numeric), ~ round(., 4))) %>%
              select(modelo, periodo, prediccion, limite_inferior, limite_superior)
          }
          
          write.csv(df_final, file, row.names = FALSE)
        }
      )
      
      showNotification("Prediction analysis completed", type = "message")
      
    }) # END OBSERVEEVENT
    
    observeEvent(input$btn_tutorial_grafico1, {
      showModal(modalDialog(
        title = "Tutorial - Trend Graph",
        HTML("<p>This graph shows the evolution of average NDVI</p>
              <ul>
              <li><b>Y Axis (vertical): NDVI average value (from 0 to 1).</li>
              <li><b>X Axis (horizontal): Time</li>
              </ul>
              <p><b>Interpretation:</b></p>
              <ul>
              <li>If both lines (blue and red) go up: vegetation improving over time.</li>
              <li>If they go down: vegetation deteriorating.</li>
              </ul>"),
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "l"
      ))
    })
    
    observeEvent(input$btn_tutorial_grafico2, {
      showModal(modalDialog(
        title = "Tutorial - STL Decomposition",
        HTML("<p>This graph helps us separate vegetation information into simpler parts to better understand what's happening.</p>
              <ul>
              <li><b>1. Original data: The actual vegetation information we measured, same as in the first graph.</li>
              <li><b>2. Remainder (residual): Small things we can't explain with the other parts, like noise or unexpected changes.</li>
              <li><b>3. Seasonality: Shows changes that repeat regularly, for example, how vegetation changes in each season.</li>
              <li><b>4. Trend: Tells us if vegetation is generally improving or worsening over time.</li>
              </ul>
              <p><b>Interpretation:</b></p>
              <ul>
              <li>Helps us clearly see seasonal changes to know when vegetation grows or decreases each year.</li>
              <li>Allows identifying unexpected changes that could be due to rare events like fires or droughts.</li>
              </ul>"),
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "l"
      ))
    })
    
    observeEvent(input$btn_tutorial_grafico3, {
      showModal(modalDialog(
        title = "Tutorial - ARIMA Prediction",
        HTML("<p>This graph shows how vegetation has changed in the past and what's expected to happen in the next n months</p>
              <ul>
              <li><b>The black line shows actual vegetation values (NDVI) we measured in the past.</li>
              <li><b>The blue line shows the prediction of what vegetation will be like in the coming months.</li>
              <li><b>The gray area around the blue line indicates a confidence range, meaning the space where vegetation is likely to move, showing prediction uncertainty.</li>
              </ul>
              <p><b>Interpretation:</b></p>
              <ul>
              <li>This graph helps with better planning by showing an estimate of how vegetation will change, allowing preparation for possible future improvements or problems.</li>
              </ul>"),
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "l"
      ))
    })
  })
}


