
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(shinysky)
library(leaflet)
library(ggvis)
library(DT)

dashboard_header <- dashboardHeader(title = "GRIDS")

dashboard_sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebar_menu",
    menuItem("Home", tabName = "menuitem_main", icon = icon("dashboard")),
    hr(),
    selectInput("country", label = "Country", choices = NULL),
    checkboxInput("map_click", label = "Clickable Map", value = FALSE),
    sliderInput("grid_scale", "Grid Scale", min = 1, max = 10, value = 1),
    sliderInput("year", "Year", min = 1992, max = 2013, value = 1992, animate = TRUE),
    HTML("<footer>"),
    box(
      width = 12,
      textOutput("status_bar")
    ),
    HTML("</footer>")
  )
)

dashboard_body <- dashboardBody(
  tags$style(type = 'text/css', 
    "footer { position:absolute; bottom:0; width: calc(100% - 230px); }"
  ),
  
  tabItems(
    tabItem(tabName = "menuitem_main",
      fluidRow(
        box(status = "success",
            busyIndicator(text = "Updating ...", wait = 300),
            leafletOutput("map")
        ),
        box(status = "success", 
            ggvisOutput("plot")
        )
      ),
      fluidRow(
        tabBox(
          id = "info_panel",
          width = 12,
          tabPanel("Raster", tableOutput("raster")),
          tabPanel("Groups", DT::dataTableOutput("groups")),
          tabPanel("Grids", DT::dataTableOutput("grids")),
          tabPanel("Summary", tableOutput("summary")),
          tabPanel("Download", 
            h5("Download grid dataset for the selected country with population density and nightlight data"),
            downloadButton('download', 'Download'))
        )
      )
    )
  )
)

dashboardPage(dashboard_header, dashboard_sidebar, dashboard_body)
