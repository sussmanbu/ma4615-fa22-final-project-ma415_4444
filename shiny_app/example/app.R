#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# 
#

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(leaflet)
library(sf)
library(tidyverse)
library(htmltools)
library(plotly)



# Build UI
ui <- fluidPage(
  tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"), 
  # Application title
  span(style = "font-weight: 600; font-size: 25px; width: 100%;
         color: #022DB7;", "State Income, College Completion Rate, and Poverty Rate"),
  # Line Breaks
  br(),br(),
  # Map
  fluidRow(
    column(8, leafletOutput("map")
    ),
    column(4, 
           span(style = "font-weight: 600; font-size:18px","Select "), span(style="font-weight: 600; font-size:18px; color:green", "State"), 
           span(style = "font-weight: 600; font-size:18px"," from the map:"),
           br(),br(),
           htmlOutput("name"),
           br(),br(),
           htmlOutput("explanation"),
           br(),
           span(),
           hr()
    )),
  br(),br(),
  hr(),
  fluidRow(
    column(5, plotlyOutput("income_plot", width = "120%", height = "400px")
    ))
)


# Define server logic required
server <- function(input, output, session) {
  
  ####### GLOBAL VARIABLE #########
  click_count <- 0 ###COUNTING CLICKs
  sn <- " "
  mode <- ""
  ###############
  ####INPUT DATASET#####
  df <- read_csv("states.csv") %>%
    rename(NAME=STATE)
 
  state <- st_read("cb_2019_us_state_20m/cb_2019_us_state_20m.shp")
  all_state <- state %>% inner_join(df,by='NAME')
  
  
  

  ##MAP ELEMENTS###
  bins <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
  pal <- colorBin("YlOrRd", domain = state$MHI, bins = bins)
  labels <- sprintf(
    "<strong>%s</strong>",
    state$NAME
  ) %>% 
    lapply(htmltools::HTML)
  
  
  ## 
  
  ########MAP###########
  output$map <- renderLeaflet({
    leaflet(state) %>%
      setView(-96, 37.8, 4) %>%
      addTiles() %>%
      addPolygons(layerId = ~NAME,
                  weight=2,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"))}) 
  observeEvent(input$df, {
    labels <- sprintf(
      "<strong>%s</strong><br/>%g ",
      all_state$NAME,all_state$MHI,
    ) %>% 
      lapply(htmltools::HTML)
    

    
    leafletProxy("map", data = all_state) %>%
      clearShapes() %>%
      clearTiles() %>%
      clearControls() %>%
      addTiles() %>%
      addPolygons(layerId = ~NAME,
                  fillColor = ~pal(proportion),
                  opacity=0.5,
                  weight=2,
                  fillOpacity=1,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto")) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, 
                title = NULL, position = "bottomright")
  })
  
  observeEvent(input$df, {
    labels <- sprintf(
      "<strong>%s</strong><br/>%g percent",
      all_state$NAME,all_state$CCR
    ) %>% 
      lapply(htmltools::HTML)
    
    
    leafletProxy("map", data = all_state) %>%
      clearShapes() %>%
      clearTiles() %>%
      clearControls() %>%
      addTiles() %>%
      addPolygons(layerId = ~NAME,
                  fillColor = ~pal(proportion),
                  opacity=0.5,
                  weight=2,
                  fillOpacity=1,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto")) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, 
                title = NULL, position = "bottomright")
  })
  
 
  
  ########################################  
  output$explanation <- renderText({
    click <- input$map_shape_click$id
    selection <- all_state %>% filter(NAME == click)
    name <- selection$NAME
    percentage <- selection$CCR %>% round(digits=2)
    explain <- "College Completion Rate after 25 is: "
    a<- "% of the population in "
    
    explain2 <- ". The Medium Income is: "
    income <- selection$MHI
    sign <- "$."
    explain3 <- "And the Poverty Rate is: "
    pr <- selection$POVERTY
    paste(explain,percentage,a,name,explain2,income,sign,explain3,pr,a,name)
  })
  
  observeEvent(input$map_shape_click, {
    click_count <<- click_count+1
  })
  
  output$name <- renderText({
    click <- input$map_shape_click$id
    selection <- state %>% filter(NAME == click)
    sn <<- selection$NAME
    paste("<strong> <span style = \'font-weight: 700;\'> State:            </span> </strong> 
        <strong> <span style = \'font-weight: 500;\'> ",sn, "</span> </strong>")
  })
  
 
}



# Run the application 
shinyApp(ui = ui, server = server)