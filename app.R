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

pfizerTimeline<- read_rds("data/pfizerTimeline.rds")
modernaTimeline<- read_rds("data/modernaTimeline")
johnsonTimeline <- read_rds("data/johnsonTimeline")


oldestDate <- as.Date(min(johnsonTimeline$Week, modernaTimeline$Week, pfizerTimeline$Week))
mostRecentDate <- as.Date(max(johnsonTimeline$Week, modernaTimeline$Week, pfizerTimeline$Week))

hexes <- "data/us_states_hexgrid.geojson"
states <- rjson::fromJSON(file=hexes)

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
                                 "Select dates:",
                                 min = as.Date(oldestDate,"%Y-%m-%d"),
                                 max = as.Date(mostRecentDate,"%Y-%m-%d"),
                                 value=c(as.Date(oldestDate,"%Y-%m-%d"), as.Date(mostRecentDate,"%Y-%m-%d")),
                                 timeFormat="%Y-%m-%d"),
                     
                     sliderInput("Population",
                                 "Filter by states with selected population:",
                                  min = min(pfizerTimeline$PopulationEstimate),
                                  max = max(pfizerTimeline$PopulationEstimate),
                                  value=c(min(pfizerTimeline$PopulationEstimate), max(pfizerTimeline$PopulationEstimate))),
                     
                 
                    sliderInput("Spending",
                                "Filter by states with selected public health spending per capita:",
                                min = min(pfizerTimeline$HealthSpendingPerCapita),
                                max = max(pfizerTimeline$HealthSpendingPerCapita),
                                pre = "$", sep = ",",
                                value=c(min(pfizerTimeline$HealthSpendingPerCapita), max(pfizerTimeline$HealthSpendingPerCapita)))),
                 
           
                 
                 # Show a plot
                 mainPanel(
                     plotlyOutput("vaccineMap"),
                     verbatimTextOutput("instructions"),
                     plotlyOutput("vaccineTimeline"),
                     plotlyOutput("stateRanks")
                     
                 )
             )),
 
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

server <- function(input, output, session) {
    

    
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
        
        clickedState <- event_data("plotly_click")
        
        if(input$brand == "Moderna"){
            vaccineDF <- modernaTimeline
        } else if(input$brand == "Pfizer"){
            vaccineDF <- pfizerTimeline
        } else if(input$brand == "Johnson"){
            vaccineDF <- johnsonTimeline
        }
        
        
        vaccineDF <- vaccineDF %>% 
            filter(PopulationEstimate >= input$Population[1]) %>%
            filter(PopulationEstimate <= input$Population[2]) %>%
            filter(HealthSpendingPerCapita >= input$Spending[1]) %>%
            filter(HealthSpendingPerCapita <= input$Spending[2])
        
        filteredData <- vaccineDF %>% 
            filter(Week >= input$Dates[1]) %>% filter(Week <= input$Dates[2]) %>%
            group_by(iso3166_2) %>%
            summarise(`TotalDoseAllocationPer100k` = sum(`TotalDoseAllocationPer100k`))
        
        
        
        g <- list(
            fitbounds = "locations",
            visible = FALSE
        )
        
        fig2 <- plot_ly()
        
        fig2 <- fig2 %>% add_trace(
            type="choropleth",
            geojson=states,
            locations=filteredData$iso3166_2,
            z=filteredData$TotalDoseAllocationPer100k,
            colorscale="Blues", reversescale=TRUE,
            featureidkey="properties.iso3166_2")
        
        fig2 <- fig2 %>% layout(
            geo = g,
            title = "Vaccinations by state")
        
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
        
        clickedState <- event_data("plotly_click")
        
        if(input$brand == "Moderna"){
            vaccineDF <- modernaTimeline
        } else if(input$brand == "Pfizer"){
            vaccineDF <- pfizerTimeline
        } else if(input$brand == "Johnson"){
            vaccineDF <- johnsonTimeline
        }
        
        vaccineDF <- vaccineDF %>% 
            filter(PopulationEstimate >= input$Population[1]) %>%
            filter(PopulationEstimate <= input$Population[2]) %>%
            filter(HealthSpendingPerCapita >= input$Spending[1]) %>%
            filter(HealthSpendingPerCapita <= input$Spending[2])
        
        filteredDataTimeLine <- vaccineDF %>% 
            group_by(iso3166_2)
        
        vaccineTimelinePlot <- plot_ly(data=filteredDataTimeLine, color=I('darkgray'), source="vaccineTimelinePlot") %>%
                                event_register('plotly_relayout')
        
        vaccineTimelinePlot <- vaccineTimelinePlot %>% highlight_key() %>% 
                                   add_trace(x=~Week, y=~TotalDoseAllocationPer100k, 
                                   name = 'Doses allocated per 100K people', 
                                   mode = 'lines',
                                   hovertemplate = ~iso3166_2) %>% layout(
                                       xaxis = list(range = c(input$Dates[1], input$Dates[2])))
            
        vaccineTimelinePlot
        
        
    })
    
    output$stateRanks <- renderPlotly({
        
        clickedState <- event_data("plotly_click")
        
        if(input$brand == "Moderna"){
            vaccineDF <- modernaTimeline
        } else if(input$brand == "Pfizer"){
            vaccineDF <- pfizerTimeline
        } else if(input$brand == "Johnson"){
            vaccineDF <- johnsonTimeline
        }
        
        
        vaccineDF <- vaccineDF %>% 
            filter(PopulationEstimate >= input$Population[1]) %>%
            filter(PopulationEstimate <= input$Population[2]) %>%
            filter(HealthSpendingPerCapita >= input$Spending[1]) %>%
            filter(HealthSpendingPerCapita <= input$Spending[2])
        
        filteredData <- vaccineDF %>% 
            filter(Week >= input$Dates[1]) %>% filter(Week <= input$Dates[2]) #%>%
            #group_by(iso3166_2) %>%
            #summarise(`TotalDoseAllocationPer100k` = sum(`TotalDoseAllocationPer100k`))
        
        
        barPlot <- plot_ly(filteredData, x=~Week, y=~TotalDoseAllocationPer100k, type='bar', color = ~iso3166_2, colors="red") %>% 
            layout(barmode='stack', xaxis = list(range = c(input$Dates[1], input$Dates[2]))) %>% hide_legend()
        barPlot
        
    })
    
    output$instructions <- renderPrint({
        #d <- event_data("plotly_click")e
        
        #if (is.null(d) == T) return (NULL);
        currentView <- event_data(event="plotly_relayout", source="vaccineTimelinePlot")
        if (is.null(currentView) == T) return (NULL) else gsub( " .*$", "", currentView$`xaxis.range[0]`)
        #newMin <- currentView$`xaxis.range[0]`
        #newMax <- currentView$`xaxis.range[2]`
        #if (is.null(d)) "Click events appear here (double-click to clear)" else d
    })
    
    
    observe({
        
        currentView <- event_data(event="plotly_relayout", source="vaccineTimelinePlot")
        
        if(is.null(currentView) || is.na(currentView) || identical(currentView, character(0))){
            newMin <- oldestDate
            newMax <- mostRecentDate
            }
        
        newMin <- gsub( " .*$", "", currentView$`xaxis.range[0]`)
        newMax <- gsub( " .*$", "", currentView$`xaxis.range[1]`)
        
        updateSliderInput(session, 'Dates', 
                          min = as.Date(oldestDate,"%Y-%m-%d"),
                          max = as.Date(mostRecentDate,"%Y-%m-%d"),
                          value = c(as.Date(newMin,"%Y-%m-%d"), as.Date(newMax,"%Y-%m-%d")), timeFormat="%Y-%m-%d")
                          
                          
    })
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
