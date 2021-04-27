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
           #plotlyOutput("timeline"),
           #verbatimTextOutput("click"),
           #verbatimTextOutput("columns"),
           tableOutput("age")
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
    
    #output$timeline <- renderPlotly({
      
      #clickMap <- event_data("plotly_click")
      #d <- event_data("plotly_click")
      #printPlot <- plot_ly(tidyCDC, y= ~d$z, mode='bars', source='source')
      
      
    #})
    
    # output$click <- renderPrint({
    #   d <- event_data("plotly_click")
    #   
    #   
    #   #if (is.null(d) == T) return (NULL);
    #   
    #   if (is.null(d)) "Click events appear here (double-click to clear)" else d
    # })
    # 
    # output$columns <- renderPrint({
    # 
    #   if(is.null(input$age) || is.na(input$age)){
    #     ageKeyword <- "total"
    #   } else if(all(c("18", "65") %in% input$age)){
    #     ageKeyword <- "total"
    #   } else {
    #     ageKeyword <- input$age
    #   }
    #   
    # 
    #   
    #   #colnames(tidyCDC[grep(ageKeyword, names(tidyCDC), ignore.case=TRUE)],)
    #   #colnames(tidyCDC[grep(input$delivery, names(tidyCDC), ignore.case=TRUE)],)
    #   #colnames(tidyCDC[grep(input$brand, names(tidyCDC), ignore.case=TRUE)],)
    # })
    # 
    # output$table <- renderDataTable(plotOfChoiceFiltered)\
      
      output$age <- renderTable({

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
        plotOfChoiceFiltered
      })
      

      
}


shinyApp(ui = ui, server = server)
