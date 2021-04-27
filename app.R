#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#The code below was from a previous iteration of the app
#I'm going to comment out the preprocessing but will keep the general UI-related code

#Load packages
library(shiny)
library(shinythemes)
library(bslib)
library(tidyverse)
library(plotly)
library(ggplot2)
library(rjson)

###
# cdcData <- read_csv("data/covid19_vaccinations_in_the_united_states.csv", skip=2) 
# cdcData <- cdcData %>% rename_with(~ gsub(" ", "_", names(cdcData)))
# cdcData <- cdcData %>% rename_with(~ gsub("/", "_", names(cdcData)))
# cdcData <- cdcData %>% mutate(State_Territory_Federal_Entity = gsub(" State", "", State_Territory_Federal_Entity))
# cdcData <- cdcData %>% 
#   select(-contains("residence", ignore.case = TRUE)) %>% 
#   select(-contains("fully", ignore.case=TRUE)) %>% 
#   select(-contains("unknown", ignore.case=TRUE))
# 
# stateIDs <- read_csv("data/us_states_hexgrid.csv") %>% 
#   select(-the_geom, -cartodb_id, -created_at, -updated_at, -bees) %>%
#   mutate(google_name = gsub(" \\(United States\\)", "", google_name))
# 
# tidyCDC <- left_join(stateIDs, cdcData, by = c("google_name" = "State_Territory_Federal_Entity"))
# 
# pfizer <- read_csv("data/COVID-19_Vaccine_Distribution_Allocations_by_Jurisdiction_-_Pfizer.csv")
# pfizer <- pfizer %>% rename_with(~ gsub(" ", "_", names(pfizer)))
# moderna <- read_csv("data/COVID-19_Vaccine_Distribution_Allocations_by_Jurisdiction_-_Moderna.csv")
# moderna <- moderna %>%   rename_with(~ gsub(" ", "_", names(moderna)))
# johnson <- read_csv("data/COVID-19_Vaccine_Distribution_Allocations_by_Jurisdiction_-_Janssen.csv")
# johnson <- johnson %>%   rename_with(~ gsub(" ", "_", names(johnson)))
# 
# testData <- read.csv("data/testData.csv")
###

hexes <- "data/us_states_hexgrid.geojson"
states <- rjson::fromJSON(file=hexes)
theme <- "www/bootstrap.min.css"

ui <- fluidPage(
    theme = shinytheme("flatly"),
    #tags$head(
  #      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")
   # ),
    # Application title
    titlePanel("COVID Vaccinations in the United States"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("brand",
                        h3("Vaccine Manufacturer:"),
                        choices= list("Moderna" = 'Moderna',
                                      "Pfizer/BioNTech" = 'Pfizer',
                                      "Johnson and Johnson/Janssen" = 'Janssen')),
            
            radioButtons("delivery",
                               h3("Status of vaccine"),
                               choices= list("Delivered" = 'delivered',
                                             "Administered" = 'administered')),
            
            checkboxGroupInput("age",
                               h3("Age"),
                               choices= list("18-65" = '18',
                                             "65+" = '65'))         
            
            ),
        


        # Show a plot
        mainPanel(
           plotlyOutput("map"),
           plotlyOutput("timeline"),
           verbatimTextOutput("click"),
           verbatimTextOutput("columns"),
        )
    )

)


server <- function(input, output) {
    
    output$map <- renderPlotly({

        
        g <- list(
            fitbounds = "locations",
            visible = FALSE
        )
        
        fig <- plot_ly()
        
        fig <- fig %>% add_trace(
            type="choropleth",
            geojson=states,
            locations=tidyCDC$iso3166_2,
            z=tidyCDC$Doses_Delivered_per_100K,
            colorscale="Blues", reversescale=TRUE,
            featureidkey="properties.iso3166_2", source = 'source')
        
        fig <- fig %>% layout(
            geo = g
        )
        fig
    })
    
    output$timeline <- renderPlotly({
      
      #clickMap <- event_data("plotly_click")
      d <- event_data("plotly_click")
      printPlot <- plot_ly(tidyCDC, y= ~d$z, mode='bars', source='source')
      
      
    })
    
    output$click <- renderPrint({
      d <- event_data("plotly_click")
      
      
      #if (is.null(d) == T) return (NULL);
      
      if (is.null(d)) "Click events appear here (double-click to clear)" else d
    })
    
    output$columns <- renderPrint({

      if(is.null(input$age) || is.na(input$age)){
        ageKeyword <- "total"
      } else if(all(c("18", "65") %in% input$age)){
        ageKeyword <- "total"
      } else {
        ageKeyword <- input$age
      }
      

      
      #colnames(tidyCDC[grep(ageKeyword, names(tidyCDC), ignore.case=TRUE)],)
      #colnames(tidyCDC[grep(input$delivery, names(tidyCDC), ignore.case=TRUE)],)
      colnames(tidyCDC[grep(input$brand, names(tidyCDC), ignore.case=TRUE)],)
    })
}


shinyApp(ui = ui, server = server)
