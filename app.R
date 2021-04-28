#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(bslib)
library(tidyverse)
library(plotly)
library(ggplot2)
library(rjson)

# Define UI for application that draws a histogram
ui <- navbarPage("COVID-19 Vaccination Dashboard",
    
    tabPanel("National Trends",
             
             sidebarLayout(
                 sidebarPanel(
                     
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
                     plotlyOutput("timeline")
                 )
             )
    ),
    
    tabPanel("Trends by manufacturer",
             
             sidebarLayout(
                 sidebarPanel(
                     radioButtons("brand2",
                                  h3("Select a manufacturer, or look at national trends:"),
                                  choices= list("Moderna only" = 'Moderna',
                                                "Pfizer/BioNTech only" = 'Pfizer',
                                                "Johnson and Johnson/Janssen only" = 'Janssen')),
                     
                     radioButtons("delivery2",
                                  h3("Status of vaccine"),
                                  choices= list("Delivered" = 'delivered',
                                                "Administered" = 'administered')),
                     
                     radioButtons("age2",
                                  h3("Age"),
                                  choices= list("All age groups" = 'Total',
                                                "18-65" = '18plus',
                                                "65+" = '65plus'))         
                     
                 ),
                 
                 
                 
                 # Show a plot
                 mainPanel(
                     plotlyOutput("map2"),
                     #plotlyOutput("timeline"),
                     #verbatimTextOutput("click"),
                     #verbatimTextOutput("columns"),
                     tableOutput("age2")
                 )
             )
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderPlotly({
        

            plotOfChoice <- cdcData_agnosticLong %>% filter(Group == input$age)
            
            if(input$delivery == "delivered"){
                plotOfChoiceFiltered <- plotOfChoice %>% select(contains("ISO", ignore.case = TRUE) |
                                                                    contains("delivered", ignore.case = TRUE))
            } else if(input$delivery == "administered"){
                plotOfChoiceFiltered <- plotOfChoice %>% select(contains("ISO", ignore.case = TRUE) |
                                                                    contains("administered", ignore.case = TRUE))
            }
            colnames(plotOfChoiceFiltered) <- c("ISOname", "Stat")
        
        
        
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
            geo = g,
            title = "Vaccinations by state"
        )
        
        fig <- fig %>% colorbar(title = 'Vaccinations per 100K')
        
        
        fig
        
    })
    
    output$timeline <- renderPlotly({
        
        if(input$age == "Total"){
            selectedAgeForTimeLine <- agnosticTimeLineGroupedAll_Total
        } else if(input$age == "18plus"){
            selectedAgeForTimeLine <- agnosticTimeLineGroupedAll %>% filter(betterAgeGroup == "18plus")
        } else if(input$age == "65plus"){
                selectedAgeForTimeLine <- agnosticTimeLineGroupedAll %>% filter(betterAgeGroup == "65plus")
        }
        
        timelineplot <- plot_ly(data = selectedAgeForTimeLine, x=~Date)
        
        timelineplot <- timelineplot %>% add_trace(y = ~`Percent of age group with at least one dose`, 
                                                   name = '`At least one dose', 
                                                   mode = 'lines+markers')
        timelineplot <- timelineplot %>% add_trace(y = ~`Percent of age group fully vaccinated`, 
                                                   name = '`Fully Vaccinated', 
                                                   mode = 'lines+markers')
        timelineplot <- timelineplot %>% layout(title = "Vaccinations over time",
                              xaxis = list(title = "Week"),
                              yaxis = list (title = "Percent of people", range = c(0, 100)))
        timelineplot
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
