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
    
    tabPanel("About",
             fluidRow(
                 column(8,
                        h2(p("About this project:")),
                        h6(p("This project aggregates data from the Centers for Disease Control and Prevention (CDC) about the ongoing COVID-19 pandemic.")), 
                        h6(p("It was designed and executed by Chad Stein and Lily Payvandi as part of the BMI 706 final project for Spring 2021.")),
                        h6(p("Weekly vaccination data updates automatically."))))),
                     

    tabPanel("Trends by Manufacturer",
             
             sidebarLayout(
                 sidebarPanel(
                     radioButtons("brand",
                                  h3("Select a manufacturer:"),
                                  choices= list("Moderna" = 'Moderna',
                                                "Pfizer/BioNTech" = 'Pfizer',
                                                "Johnson and Johnson" = 'Johnson')),
                     
                     sliderInput("Dates",
                                 "Dates:",
                                 min = as.Date(oldestDate,"%Y-%m-%d"),
                                 max = as.Date(mostRecentDate,"%Y-%m-%d"),
                                 value=c(as.Date(oldestDate,"%Y-%m-%d"), as.Date(mostRecentDate,"%Y-%m-%d")),
                                 timeFormat="%Y-%m-%d")
                     
                 ),
                 
                 
                 
                 # Show a plot
                 mainPanel(
                     plotlyOutput("vaccineMap"),
                     verbatimTextOutput("instructions"),
                     plotlyOutput("vaccineTimeline")
                     
                 )
             )
    ),
    
    
    
    tabPanel("Cross-manufacturer Trends",
             
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
                     plotlyOutput("timeline")
                 )
             )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    oldestDate <- as.Date(min(johnsonTimeline$Week, modernaTimeline$Week, pfizerTimeline$Week))
    mostRecentDate <- as.Date(max(johnsonTimeline$Week, modernaTimeline$Week, pfizerTimeline$Week))
    
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
    
    output$vaccineMap <- renderPlotly({
        
    
        
        if(input$brand == "Moderna"){
            vaccineDF <- modernaTimeline
        } else if(input$brand == "Pfizer"){
            vaccineDF <- pfizerTimeline
        } else if(input$brand == "Johnson"){
            vaccineDF <- johnsonTimeline
        }
        
        
        filteredData <- vaccineDF %>% 
            filter(Week >= input$Dates[1]) %>% filter(Week <= input$Dates[2]) %>%
            group_by(iso3166_2) %>%
            summarise(`FirstDoseAllocation` = sum(`FirstDoseAllocation`))
        
        
        g <- list(
            fitbounds = "locations",
            visible = FALSE
        )
        
        fig2 <- plot_ly()
        
        fig2 <- fig2 %>% add_trace(
            type="choropleth",
            geojson=states,
            locations=filteredData$iso3166_2,
            z=filteredData$FirstDoseAllocation,
            colorscale="Blues", reversescale=TRUE,
            featureidkey="properties.iso3166_2")
        
        fig2 <- fig2 %>% layout(
            geo = g,
            title = "Vaccinations by state"
        )
    
        
        
        fig2
        
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
    
    output$vaccineTimeline <- renderPlotly({
        
        if(input$brand == "Moderna"){
            vaccineDF <- modernaTimeline
        } else if(input$brand == "Pfizer"){
            vaccineDF <- pfizerTimeline
        } else if(input$brand == "Johnson"){
            vaccineDF <- johnsonTimeline
        }
        
        filteredDataTimeLine <- vaccineDF %>% 
            group_by(iso3166_2)
        
        vaccineTimelinePlot <- plot_ly(data=filteredDataTimeLine)
        
        vaccineTimelinePlot <- vaccineTimelinePlot %>% add_trace(x=~Week, y=~FirstDoseAllocation, 
                                   name = 'FirstDoseAllocation', 
                                   mode = 'lines',
                                   hovertemplate = ~iso3166_2)     
            
        vaccineTimelinePlot
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
