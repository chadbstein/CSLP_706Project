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
                        h5(p("This project aggregates data from the Centers for Disease Control and Prevention (CDC) about the ongoing COVID-19 pandemic.")), 
                        h5(p("It was designed and executed by Chad Stein and Lily Payvandi as part of the BMI 706 final project for Spring 2021.")),
                        br(),
                        h5(p("Explore the distribution of different vaccines over time on the 'Trends by Manufacturer' tab.")),
                        h5(p("Explore how the vaccines have been used across the country in the 'Cross-manufacturer trends' tab."))
                        ))),
                     

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
                                 timeFormat="%Y-%m-%d", step=7),
                     
                     sliderInput("Population",
                                 "Filter by states with selected population:",
                                  min = 400000,
                                  max = max(pfizerTimeline$PopulationEstimate),
                                  value=c(400000, max(pfizerTimeline$PopulationEstimate)),
                                  step=250000),
                     
                 
                    sliderInput("Spending",
                                "Filter by states with selected public health spending per capita:",
                                min = 5000,
                                max = max(pfizerTimeline$HealthSpendingPerCapita),
                                pre = "$", sep = ",",
                                value=c(5000, max(pfizerTimeline$HealthSpendingPerCapita)),
                                step=100),),
                 
           
                 
                 # Show a plot
                 mainPanel(
                     plotlyOutput("vaccineMap"),
                     verbatimTextOutput("instructions"),
                     br(),
                     plotlyOutput("vaccineTimeline"),
                     br(),
                     br(),
                     plotlyOutput("stackedBar")
                     
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
            featureidkey="properties.iso3166_2") %>%
            colorbar(title = 'Allocation per 100K')
        
        
        fig <- fig %>% layout(
            geo = g,
            title = "Vaccinations by state")
        
        fig <- fig %>% add_trace(type="scattergeo",
                                   locations = plotOfChoiceFiltered$ISOname, 
                                   mode="text",geojson=states,
                                   featureidkey="properties.iso3166_2",
                                   texttemplate=plotOfChoiceFiltered$ISOname,
                                   #text = states,
                                   textfont = list(color=rgb(0,0,0),  size =12))
        
        fig <- fig %>% add_trace(
            type="choropleth",
            geojson=states,
            locations=plotOfChoiceFiltered$ISOname,
            z=plotOfChoiceFiltered$Stat,
            colorscale="Blues", reversescale=TRUE,
            featureidkey="properties.iso3166_2", hovertemplate = paste('Doses', input$delivery, 'per 100K people:<br>%{z}'), showscale = FALSE)
        
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
        timelineplot <- timelineplot %>% layout(title = "Cumulative accinations over time",
                              xaxis = list(title = "Date"),
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
            featureidkey="properties.iso3166_2") %>%
            colorbar(title = "Vaccines per 100K")
        
        
        fig2 <- fig2 %>% layout(
            geo = g,
            title = "Vaccinations allocated by state")
        
        fig2 <- fig2 %>% add_trace(type="scattergeo",
                                   locations = filteredData$iso3166_2, 
                                   mode="text",geojson=states,
                                   featureidkey="properties.iso3166_2",
                                   texttemplate=filteredData$iso3166_2,
                                   #text = states,
                                   textfont = list(color=rgb(0,0,0),  size =12))
        
        fig2 <- fig2 %>% add_trace(
            type="choropleth",
            geojson=states,
            locations=filteredData$iso3166_2,
            z=filteredData$TotalDoseAllocationPer100k,
            colorscale="Blues", reversescale=TRUE,
            featureidkey="properties.iso3166_2", hovertemplate = 'Doses allocated per 100K people:<br>%{z}', showscale = FALSE)
        
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
        timelineplot <- timelineplot %>% layout(title = "Vaccinations administered over time",
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

        vaccineTimelinePlot <- plot_ly(source="vaccineTimelinePlot") %>%
                                event_register('plotly_relayout')
        
        vaccineTimelinePlot <- vaccineTimelinePlot %>% highlight_key() %>% 
                                   add_trace(data=filteredDataTimeLine, x=~Week, y=~TotalDoseAllocationPer100k, text=~iso3166_2,
                                   hoverinfo='text', 
                                   mode = 'lines+markers', color=I('darkgray')) %>% 
                                    layout(xaxis = list(range = c(input$Dates[1], input$Dates[2]), title='Date'),
                                           yaxis = list(title='Doses allocated per 100K'),
                                           title='Vaccinations allocated per state') %>% hide_legend()
        
        d <- event_data("plotly_click")
        if(!is.null(d)){
        activeStates <- unique(vaccineDF[c("iso3166_2")])
        activeStates <- activeStates[order(activeStates$iso3166_2),]
        d <- event_data("plotly_click")
        selectedStateNumber <- d$pointNumber
        selectedStateInfo <- filteredDataTimeLine %>% filter(iso3166_2 == activeStates[selectedStateNumber+1,1])
        
        vaccineTimelinePlot <- vaccineTimelinePlot %>% add_trace(data= selectedStateInfo, x=~Week, y=~TotalDoseAllocationPer100k, 
                                                                 text=~iso3166_2,
                                                                 hoverinfo='text', 
                                                                 mode = 'lines+markers', color=I('red')) %>% 
                                                        layout(xaxis = list(range = c(input$Dates[1], input$Dates[2]), title='Date'),
                                                               yaxis = list(title='Doses allocated per 100K'),
                                                               title='Vaccinations allocated per state') %>% 
                                                        hide_legend()
        }
        
        vaccineTimelinePlot
        
    })
    
    output$stackedBar <- renderPlotly({
        
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
            filter(Week >= input$Dates[1]) %>% filter(Week <= input$Dates[2])

        barPlot <- plot_ly(filteredData, 
                           x=~Week, y=~TotalDoseAllocationPer100k, 
                           type='bar', color = ~iso3166_2, colors="darkgrey", 
                           source="barPlot", marker = list(line = list(width = 0.5,
                                                                       color = 'rgb(0, 0, 0)'))) %>% 
            layout(barmode = 'stack',
                   title = 'Vaccinations across states',
                   xaxis = list(range = c(input$Dates[1], input$Dates[2]), title = 'Date'), 
                   yaxis = list(title = 'Doses allocated per 100K')) %>% 
            hide_legend() %>%
            event_register('plotly_relayout')
        
        barPlot
        
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
    
    observe({
        
        currentView2 <- event_data(event="plotly_relayout", source="barPlot")
        
        if(is.null(currentView2) || is.na(currentView2) || identical(currentView2, character(0))){
            newMin <- oldestDate
            newMax <- mostRecentDate
        }
        
        newMin <- gsub( " .*$", "", currentView2$`xaxis.range[0]`)
        newMax <- gsub( " .*$", "", currentView2$`xaxis.range[1]`)
        
        updateSliderInput(session, 'Dates', 
                          min = as.Date(oldestDate,"%Y-%m-%d"),
                          max = as.Date(mostRecentDate,"%Y-%m-%d"),
                          value = c(as.Date(newMin,"%Y-%m-%d"), as.Date(newMax,"%Y-%m-%d")), timeFormat="%Y-%m-%d")
        
        
    })
    
    output$instructions <- renderPrint({
        
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

        activeStates <- unique(vaccineDF[c("iso3166_2")])
        activeStates <- activeStates[order(activeStates$iso3166_2),]
        d <- event_data("plotly_click")
        selectedState <- d$pointNumber

        if(is.null(d)){
        "Click  a state on the map to highlight it below. Double-click to reset."
        } else {
        paste("Currently highlighted state: ", activeStates[selectedState+1,])
        }

    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
