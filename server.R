
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(raster)
library(rgdal)
library(ggvis)
library(geosphere)
library(countrycode)
library(DT)
library(dplyr)

Default_Country <- "AFG"
Working_Directory <- "~/Projects/koala"
Zoom_Level <- 6

shinyServer(function(input, output, session) {
  
  setwd(Working_Directory)
  
  v <- reactiveValues(status = "",
                      country = Default_Country)

  update_country_selection <- function() {
    countries <- countries@data %>%
      dplyr::select(iso_a3, name) %>%
      dplyr::arrange(name)
    
    updateSelectInput(session, 
                      "country", 
                      choices = setNames(as.character(countries$iso_a3), countries$name),
                      selected = Default_Country)
  }
  
  output$status_bar <- renderText(v$status)

  progress <- shiny::Progress$new(session)
  on.exit(progress$close())
  
  progress$set(message = "Loading Natural Earth dataset")
  countries <- readOGR("./datasets/natural_earth", "ne_50m_admin_0_countries")  
  
  update_country_selection()
  
  progress$set(message = "Loading Population dataset")
  population_data <- raster("./datasets/gpw/gldens00/glds00ag/w001001.adf")
  
  progress$set(message = "Loading GeoEPR dataset")
  population_groups <- readOGR("./datasets/geoEPR/2014", "GeoEPR-2014")
  
  progress$set(message = "Loading Night Lights dataset")
  nightlight_data <- raster("./datasets/noaa/F152000.v4/F152000.v4b_web.stable_lights.avg_vis.tif")
  
  population_palette <- function() { "OrRd" }
  nightlight_palette <- function() { "Blues" }
  
  spatial_data <- function(country) { subset(countries, iso_a3 == country) }
  
  country_name <- reactive({
    if (is.null(input$country) || input$country == "")
      return("")
    
    sprintf("%s (%s)", spatial_data(input$country)$name, input$country)
  })  
  
  extent_obj <- reactive({ 
    if (is.null(input$country) || input$country == "")
      return()
    
    extent(spatial_data(input$country)) 
  })

  masked_obj <- function(source_data) {
    cropped_obj <- crop(source_data, extent_obj())
    mask(x = cropped_obj, mask = spatial_data(input$country)) 
  }

  population_obj <- reactive({ 
    if (is.null(input$country) || input$country == "")
      return()
    
    population_masked <- masked_obj(population_data)
    
    if (input$grid_scale == 1)
      return(population_masked)
    
    aggregate(population_masked, input$grid_scale)
  })
  
  nightlight_obj <- reactive({ 
    if (is.null(input$country) || input$country == "")
      return()
    
    resample(masked_obj(nightlight_data), population_obj())
  })
  
  groups_obj <- reactive({
    if (is.null(input$country) || input$country == "")
      return()
    
    cow_code <- countrycode(input$country, "iso3c", "cown")
    population_groups[population_groups$gwid == cow_code,]
  })
  
  output$map <- renderLeaflet({
    if (is.null(input$country) || input$country == "")
      return()
    
    ext <- extent_obj()
    pol <- rbind(c(ext@xmax, ext@ymax), c(ext@xmax, ext@ymin), c(ext@xmin, ext@ymin), c(ext@xmin, ext@ymax))
    
    center <- centroid(pol)
    
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v4/altaf-ali.98591aaf/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoiYWx0YWYtYWxpIiwiYSI6ImNpZ2xoZGI2NTAwOXV2eG00MHlrd3BnNjIifQ.qOEihbpCyME9-vG4YxLGqw",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      addPolygons(data = countries, layerId = ~iso_a3, weight = 0, fillColor = "transparent") %>%
      setView(lng = center[1], lat =  center[2], zoom = Zoom_Level) %>%
      addLayersControl(
        baseGroups = c("Population Density", "Nightlight"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  observeEvent(input$map_shape_mouseover, {
    event <- input$map_shape_mouseover
    v$status <- sprintf("%s: %2.2f, %2.2f", event$id, event$lat, event$lng) 
  })
  
  observeEvent(input$map_shape_click, { 
    event <- input$map_shape_click    
    
    if (!input$map_click || is.null(event))
      return()
    
    v$country <- event$id
    
    updateSelectInput(session, "country", selected = v$country)
  })
  
  observeEvent(input$country, {
    if (is.null(input$country) || input$country == "")
      return()
    
    withProgress(message = "Updating map layers", {
      leafletProxy("map", session = session) %>%
        addRasterImage(population_obj(), colors = population_palette(), opacity = 0.8, group = "Population Density") %>%
        addRasterImage(nightlight_obj(), colors = nightlight_palette(), opacity = 0.8, group = "Nightlight") %>%
        hideGroup("Nightlight")
    })
  })
  
  build_raster_table <- function(obj) {
    ext <- extent_obj()
    val <- values(obj)

    raster_table <- data.frame(class = class(population_obj), 
                               dimensions = sprintf("%s, %s, %s (nrow, ncol, ncell)", nrow(obj), ncol(obj), ncell(obj)),
                               resolution = sprintf("%s, %s (x, y)", res(obj)[1], res(obj)[2]),
                               extent = sprintf("%s, %s, %s, %s (xmin, xmax, ymin, ymax)", ext@xmin, ext@xmax, ext@ymin, ext@ymax),
                               coord.ref = projection(obj),
                               names = names(obj)[1],
                               values = sprintf("%s, %s (min, max)", min(val, na.rm = TRUE), max(val, na.rm = TRUE))
    )
    t(raster_table)
  }
  
  output$raster <- renderTable({
    if (is.null(input$country) || input$country == "")
      return()
    
    if (is.null(population_obj()) || is.null(nightlight_obj()))
      return()
    
    raster_table <- cbind(build_raster_table(population_obj()), build_raster_table(nightlight_obj()))
    colnames(raster_table) <- c("Population", "Night Light")
    raster_table
  })
  
  output$groups <- DT::renderDataTable({
    DT::datatable(groups_obj()@data, 
                  selection = 'single', 
                  options = list(pageLength = 3, 
                                 scrollY = "300px",
                                 scrollCollapse = TRUE,
                                 paging = FALSE))
  })

  grid_table <- reactive({
    if (is.null(input$country) || input$country == "")
      return(data.frame(population_density = double(), nightlight = double(), group = factor()))
    
    population_obj <- population_obj()
    population_obj_values <- values(population_obj)
    population_obj <- setValues(population_obj, seq_along(population_obj))
   
    grids <- rasterToPolygons(population_obj)
    grids@data$grid_id <- seq_along(population_obj)
    grids@data$population_density <- population_obj_values
    grids@data$nightlight <- values(nightlight_obj())
   
    groups <- spTransform(groups_obj(), crs(grids))
    overlays <- sp::over(groups, grids, returnList = TRUE)

    groups_list <- lapply(seq_along(overlays), function(i) {
      data.frame(overlays[[i]], group_id = groups@data$groupid[i])
    })
    
    dplyr::bind_rows(groups_list) %>%
      dplyr::mutate(group = factor(group_id, levels = groups@data$groupid, labels = groups@data$group)) %>%
      dplyr::select(grid_id, group, group_id, population_density, nightlight)
  })
  
  output$grids <- DT::renderDataTable({
    DT::datatable(grid_table(), 
                  selection = 'single', 
                  options = list(pageLength = 3, 
                                 scrollY = "300px",
                                 scrollCollapse = TRUE,
                                 paging = FALSE))
  })
  
  reactive({
    grid_table %>%
      ggvis(x = ~population_density, y = ~nightlight) %>%
      add_axis("x", orient = "top", ticks = 0, title = country_name(),
               properties = axis_props(
                 axis = list(stroke = "white"),
                 labels = list(fontSize = 0))) %>%
      add_axis("x", title = "Population Density") %>%
      scale_numeric("x", nice = TRUE)  %>%
      add_axis("y", title = "Nightlight") %>%
      scale_numeric("y", nice = TRUE)  %>%
      layer_points(stroke := "black", size = 0.2, fill = ~group) 
  }) %>%
  bind_shiny("plot")
  
  output$download <- downloadHandler(
    filename = function() {
      paste(input$country, "-", Sys.Date(), '.csv', sep='')
    },
    content = function(connection) {
      write.csv(grid_table(), connection, row.names = FALSE)
    }
  )  
})
