library(shiny)
library(leaflet)

source("Walk_To_Nairobi.R")

ui <- fluidPage(
    headerPanel(title = "Walk To Nairobi"),
    mainPanel(
      br(),
      p("In order to foster an emulation towards a more healthy and eco-friendly lifestyle, the team at Stats4SD started recording physical activities in terms of steps.
      The idea is to combine these steps into a collective virtual walk towards some target destination related to our work. Our first objective is to walk to Nairobi, the route we have selected being 11,503 km."),
        leafletOutput("map"),
        br(),
        actionButton("walked", "Distance Walked"),
        htmlOutput("dist1"),
        actionButton("left", "Distance Remaining"),
        htmlOutput("dist2")
    )
)


server <- function(input, output, session){
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addMarkers(lat = 51.452389, lng = -0.965250, popup= "Stats4SD, Reading, UK") %>%
            addMarkers(lat = -1.292060, lng = 36.821949, popup = "Nairobi, Kenya") %>% 
            addMarkers(lat = current_location$lat, lng = current_location$lon, popup = "We are here!") %>%
            addPolylines(lat = progress_polyline$lat, lng = progress_polyline$lon, weight = 4, color = "#E74C3C", opacity = .8)  %>%
            addPolylines(lat = other_polyline$lat, lng = other_polyline$lon, weight = 2, color = "#21618C", opacity = 0.25)
        
    })
    
    observeEvent(input$walked, {
        output$dist1 <- renderText({
            paste(distance_progress$total_distance, "km")
        })
    })
    
    observeEvent(input$left, {
        output$dist2 <- renderText({
            paste(distance_progress$distance_left, "km")
        })
    })
}

shinyApp(ui=ui,server=server)