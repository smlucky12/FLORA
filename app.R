# ==============================
# ============================================================
#  FLORA - Aplicación Shiny para análisis y predicción de NDVI
# ============================================================

# ==============================
# Librerías
# ==============================
library(shinyWidgets)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(DT)
library(jsonlite)
library(terra)
library(sf)
library(doParallel)
library(shinybusy)
library(dplyr)
library(tidyr)
library(bslib)
library(rintrojs)

options(encoding = "UTF-8")

# ==============================
# Límites del área
# ==============================
lat_min <- -10
lat_max <- 0
lng_min <- -81.23413
lng_max <- -70

# ==============================
# Importar módulo de predicción
# ==============================
source("prediccion_tab.R")  # Asegúrate de tener este archivo
source("ndvi_visual_tab.R")
source("grafico.R")
# ==============================
# Interfaz de Usuario (UI)
# ==============================
ui <- fluidPage(
  
  tags$meta(charset = "UTF-8"),
  theme = bs_theme(version = 4, bootswatch = "minty"),
  introjsUI(),
  add_busy_spinner(spin = "fading-circle"),
  
  # ---- Title ----
  titlePanel(
    div(
      style = "text-align: center; font-size: 36px; font-weight: bold; color: #2E8B57;",
      "FLORA"
    )
  ),
  
  # Main tabset
  tabsetPanel(
    id = "main_tabs",
    tabPanel(
      title = "MAP AND NDVI",
      value = "mapa_ndvi",
      # Tutorial button INSIDE this tab
      fluidRow(
        column(
          12,
          align = "center",
          actionButton(
            "btn_tutorial",
            "View Tutorial",
            style = "margin-bottom:10px; background-color:#2E8B57; color:white;"
          )
        )
      ),
      
      # Map content and controls
      sidebarLayout(
        sidebarPanel(
          div(id = "coords_box",
              `data-step` = 2,
              `data-intro` = "Display of rectangle ID and coordinates.",
              DTOutput("tabla_coords")
          ),
          div(id = "calc_box",
              `data-step` = 3,
              `data-intro` = "After selecting areas, click 'Calculate NDVI'.",
              actionButton("calcular_ndvi", "Calculate NDVI")
          ),
          div(id = "ndvi_box",
              `data-step` = 4,
              `data-intro` = "NDVI results appear here (ID, year, month, day, NDVI).",
              fluidRow(
                column(10, DTOutput("tabla_ndvi")),
                column(2, br(), actionLink("help_ndvi_table", label = "❓", title = "NDVI color meaning"))
              )
          ),
          div(id = "download_box",
              `data-step` = 5,
              `data-intro` = "Download a CSV with rectangle, date and NDVI.",
              downloadButton("guardar_csv", "Save CSV table")
          ),
          width = 4
        ),
        mainPanel(
          div(id = "map_box",
              `data-step` = 1,
              `data-intro` = "Draw rectangles on the map to define sampling areas. You can also change the base layer (Standard / Satellite / Terrain).",
              leafletOutput("mapa", height = 600)
          ),
          width = 8
        )
      )
    ),
    
    # Other tabs (without tutorial button)
    prediccion_tab_ui("prediccion1"),
    ndvi_visual_tab_ui("ndvi_visual1"),
    grafico_tab_ui("grafico1")
  )
)
# ==============================
# Servidor (SERVER)
# ==============================
server <- function(input, output, session) {
  
  # -----------------------------
  # Welcome Modal
  # -----------------------------
  
  # Welcome modal (optional)
  observe({
    showModal(
      modalDialog(
        title = div(span("Welcome to FLORA", style = "font-weight:bold; font-size:24px; color:#2E8B57;")),
        HTML('
          <div style="font-family: Lucida Handwriting, sans-serif; line-height:1.6;">
            <p style="font-size:16px; text-align:center;">
              A tool created to <b>explore, discover and understand</b> vegetation health in a simple and interactive way.
            </p>
            <ul style="font-size:16px; list-style-type: \\002714; margin-left:20px;">
              <li>Easily select your areas of interest on the map.</li>
              <li>Calculate and analyze the vegetation index (NDVI).</li>
              <li>Explore clear results in dynamic tables and charts.</li>
              <li>Discover trends and nature predictions over time.</li>
            </ul>
            <p style="font-size:15px; text-align:center; color:#555;">
              <i>With <b>FLORA</b>, environmental data comes to life so you can discover what the land has to tell you.</i>
            </p>
          </div>
        '),
        easyClose = TRUE,
        footer = modalButton("Get Started")
      )
    )
  })
  
  # -----------------------------
  # REACTIVE VARIABLES
  # -----------------------------
  rect_data <- reactiveVal(
    data.frame(
      rect_id = integer(0),
      lng_min = numeric(0),
      lng_max = numeric(0),
      lat_min = numeric(0),
      lat_max = numeric(0)
    )
  )
  
  ndvi_result <- reactiveVal(NULL)
  
  # -----------------------------
  # INTERACTIVE MAP
  # -----------------------------
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap, group = "Standard Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain (Topographic)") %>%
      setView(lng = -75, lat = -5, zoom = 6) %>%
      setMaxBounds(lng1 = lng_min, lat1 = lat_min, lng2 = lng_max, lat2 = lat_max) %>%
      addDrawToolbar(
        targetGroup = "selection",
        rectangleOptions = drawRectangleOptions(showArea = TRUE, repeatMode = FALSE),
        polygonOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      ) %>%
      addLayersControl(
        baseGroups = c("Standard Map", "Satellite", "Terrain (Topographic)"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # -----------------------------
  # Capture drawn rectangles
  # -----------------------------
  observeEvent(input$mapa_draw_new_feature, {
    feature <- input$mapa_draw_new_feature
    feature <- jsonlite::fromJSON(jsonlite::toJSON(feature))
    
    if (feature$geometry$type == "Polygon") {
      coords <- feature$geometry$coordinates[1, , , 1]
      lngs <- coords[, 1]
      lats <- coords[, 2]
      
      rect_id <- ifelse(nrow(rect_data()) == 0, 1, max(rect_data()$rect_id) + 1)
      rect_df <- data.frame(
        rect_id = rect_id,
        lng_min = min(lngs),
        lng_max = max(lngs),
        lat_min = min(lats),
        lat_max = max(lats)
      )
      
      rect_data(rbind(rect_data(), rect_df))
    }
  })
  
  # -----------------------------
  # Coordinates and NDVI tables
  # -----------------------------
  output$tabla_coords <- renderDT({
    datatable(rect_data(), options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$tabla_ndvi <- renderDT({
    req(ndvi_result())
    
    df <- ndvi_result()
    
    datatable(df, options = list(pageLength = 10), rownames = FALSE) %>%
      formatStyle(
        "ndvi",
        backgroundColor = styleInterval(
          c(0.3, 0.6),
          c("#ef989c", "#fbcc68", "#60c9a3")
        ),
        color = "black",
        fontWeight = "bold"
      )
  })
  
  # Help button ❓
  observeEvent(input$help_ndvi_table, {
    showModal(
      modalDialog(
        title = "Color Meaning (NDVI)",
        HTML("<ul>
               <li><b>Red:</b> Poor or damaged vegetation.</li>
               <li><b>Yellow:</b> Average vegetation, requires attention.</li>
               <li><b>Green:</b> Healthy vegetation.</li>
             </ul>"),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  # -----------------------------
  # Tutorial logic
  # -----------------------------
  observeEvent(input$btn_tutorial, {
    # Only execute if we are in the "MAP AND NDVI" tab
    if (input$main_tabs == "mapa_ndvi") {
      showModal(modalDialog(
        title = "Welcome to FLORA Tutorial",
        HTML("<p><b>FLORA</b> helps you see vegetation health in a simple way.</p>"),
        footer = tagList(
          modalButton("Close"),
          actionButton("start_tour", "Start Tour")
        ),
        easyClose = TRUE
      ))
    } else {
      showNotification("The tutorial is only available in the 'MAP AND NDVI' tab.", type = "warning")
    }
  })
  
  observeEvent(input$start_tour, {
    removeModal()
    introjs(session, options = list(
      steps = data.frame(
        element = c("#map_box", "#coords_box", "#calc_box", "#ndvi_box", "#download_box"),
        intro = c(
          "Draw rectangles on the map to define sampling areas.",
          "Display of rectangle ID and coordinates (lng/lat).",
          "After selecting areas, click 'Calculate NDVI'.",
          "NDVI results appear here.",
          "Download a CSV with the results."
        ),
        position = rep("right", 5),
        stringsAsFactors = FALSE
      )
    ))
  })
  
  # -----------------------------
  # Automatic tour when NDVI is available
  # -----------------------------
  observeEvent(ndvi_result(), {
    df <- ndvi_result()
    if (!is.null(df) && nrow(df) > 0) {
      later::later(function() {
        introjs(session, options = list(
          steps = data.frame(
            element = c("#ndvi_box", "#download_box"),
            intro = c(
              "NDVI results have been calculated successfully.",
              "You can download them as CSV."
            ),
            position = c("right", "right"),
            stringsAsFactors = FALSE
          )
        ))
      }, delay = 0.5)
    }
  })
  
  # -----------------------------
  # Download CSV
  # -----------------------------
  output$guardar_csv <- downloadHandler(
    filename = function() {
      paste0("ndvi_rectangles_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(ndvi_result(), file, row.names = FALSE)
    }
  )
  
  # -----------------------------
  # Button: Calculate NDVI
  # -----------------------------
  observeEvent(input$calcular_ndvi, {
    req(nrow(rect_data()) > 0)
    
    withProgress(message = "Calculating NDVI from TIF files...", value = 0, {
      
      tif_folder <- "TIF"
      tif_files <- list.files(tif_folder, pattern = "\\.tif$", full.names = TRUE)
      
      if (length(tif_files) == 0) {
        showNotification("No TIF files found in the specified folder.", type = "error")
        return()
      }
      
      rect_sf_list <- lapply(1:nrow(rect_data()), function(i) {
        r <- rect_data()[i, ]
        st_polygon(list(matrix(c(
          r$lng_min, r$lat_min,
          r$lng_max, r$lat_min,
          r$lng_max, r$lat_max,
          r$lng_min, r$lat_max,
          r$lng_min, r$lat_min
        ), ncol = 2, byrow = TRUE)))
      })
      
      rect_sf <- st_sf(
        rect_id = rect_data()$rect_id,
        geometry = st_sfc(rect_sf_list),
        crs = 4326
      )
      
      results <- list()
      
      for (tif_file in tif_files) {
        base_name <- tools::file_path_sans_ext(basename(tif_file))
        ndvi_raster <- rast(tif_file)
        
        for (i in 1:nrow(rect_sf)) {
          poly <- rect_sf[i, ]
          area_proj <- st_transform(poly, crs(ndvi_raster))
          
          ndvi_crop <- crop(ndvi_raster, vect(area_proj))
          ndvi_mask <- mask(ndvi_crop, vect(area_proj))
          
          ndvi_values <- values(ndvi_mask) * 0.00000001
          valid_values <- ndvi_values[!is.na(ndvi_values) & ndvi_values >= -2 & ndvi_values <= 1]
          average_ndvi <- round(mean(valid_values), 2)
          
          julian_date <- sub(".*\\.A(\\d{7})\\..*", "\\1", base_name)
          
          if (!is.na(julian_date) && nchar(julian_date) == 7) {
            year <- as.numeric(substr(julian_date, 1, 4))
            doy <- as.numeric(substr(julian_date, 5, 7))
            date <- as.Date(doy - 1, origin = paste0(year, "-01-01"))
          } else {
            year <- NA
            date <- NA
          }
          
          results[[length(results) + 1]] <- data.frame(
            rect_id = rect_sf$rect_id[i],
            year = year,
            month = if (!is.na(date)) as.numeric(format(date, "%m")) else NA,
            day = if (!is.na(date)) as.numeric(format(date, "%d")) else NA,
            ndvi = average_ndvi
          )
        }
      }
      
      final_df <- do.call(rbind, results)
      ndvi_result(final_df)
      
      showNotification("NDVI calculation completed successfully", type = "message")
    })
  })
  
  # -----------------------------
  # Prediction module
  # -----------------------------
  prediccion_tab_server("prediccion1", ndvi_result)
  ndvi_visual_tab_server("ndvi_visual1", ndvi_result, rect_data)
  grafico_tab_server("grafico1", ndvi_result)
}

  
# ==============================
# Ejecutar Shiny App
# ==============================

shinyApp(ui, server)
