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

hexes <- "data/us_states_hexgrid.geojson"
states <- rjson::fromJSON(file=hexes)
theme <- "www/bootstrap.min.css"
cdcData_agnosticLong <- read_rds("data/cdcData_agnosticLong.rds")

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
          radioButtons("brand",
                        h3("Select a manufacturer, or look at national trends:"),
                        choices= list("Moderna only" = 'Moderna',
                                      "Pfizer/BioNTech only" = 'Pfizer',
                                      "Johnson and Johnson/Janssen only" = 'Janssen',
                                      "All vaccines" = 'allBrands')),
            
            radioButtons("delivery",
                         h3("Status of vaccine"),
                         choices= list("Delivered" = 'delivered',
                                       "Administered" = 'administered')),
            
            radioButtons("age",
                         h3("Age"),
                         choices= list("All age groups" = 'Total',
                                       "18-65" = '18plus',
                                       "65+" = '65plus'))         
            
            ),
        


        # Show a plot
        mainPanel(
           plotlyOutput("map"),
        )
    )

)


server <- function(input, output) {

      output$map <- renderPlotly({

      if(input$brand == "allBrands"){
        
        plotOfChoice <- cdcData_agnosticLong
        plotOfChoice <- plotOfChoice %>% filter(Group == input$age)
        
        if(input$delivery == "delivered"){
          plotOfChoiceFiltered <- plotOfChoice %>% select(contains("ISO", ignore.case = TRUE) |
                                                            contains("delivered", ignore.case = TRUE))
        } else if(input$delivery == "administered"){
          plotOfChoiceFiltered <- plotOfChoice %>% select(contains("ISO", ignore.case = TRUE) |
                                                            contains("administered", ignore.case = TRUE))
        }
        colnames(plotOfChoiceFiltered) <- c("ISOname", "Stat")
      }
      
      
        g <- list(
            fitbounds = "locations",
            visible = FALSE
        )
        
        fig <- plot_ly()
        
        fig <- fig %>% add_trace(
            type="choropleth",
            geojson=states,
            locations=plotOfChoiceFiltered$ISOname,
            z=plotOfChoiceFiltered$Stat,
            colorscale="Blues", reversescale=TRUE,
            featureidkey="properties.iso3166_2")
        
        fig <- fig %>% layout(
            geo = g
        )
        
        
        fig
        
    })


      
}


shinyApp(ui = ui, server = server)
