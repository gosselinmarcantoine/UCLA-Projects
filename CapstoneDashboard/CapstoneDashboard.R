
# Dashboard Set up --------------------------------------------------------

library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(plyr)
library(stringr)
library(forecast)
library(gganimate)
library(plotly)
library(leaflet)
library(forecast)
library(quantmod) 
library(tseries)

setwd('~/Documents/DataScience/UCLADatascience/CapstoneDashboard')

# Gun Violence Incidents --------------------------------------------------

# gun <- data.table::fread('gun-violence-data_01-2013_03-2018.csv', header=TRUE, stringsAsFactors = FALSE) # Fread() 
PieChart <- read.csv("PieChartCapstone", header = T)
victimsPerState <- read.csv('victimsPerStateCapstone', header = T, sep = ",")
percVictimsPerState <- read.csv('percVictimsPerStateCapstone', header = T, sep = ",")
time_incidents <- read.csv('TimeIncidentsCapstone', header = T, sep = ",")
X <- read.csv("XCapstone", header = T, sep = ",")

# # Histogram of State comparisons
# 
# incident_type_COL <- PieChart$incident_type
# diffStates <- PieChart[, 2:53]
# denom <- rep(colSums(diffStates), each = 10)
# diffStates <- diffStates/denom * 100
# diffStates <- cbind(incident_type_COL, diffStates)
# colnames(diffStates)[1] <- "incident_type"
# write.csv(diffStates, "StatesComp", row.names = FALSE, quote = FALSE)
# StatesComp <- read.csv("StatesComp", header = T)

# dashHead (UI) -----------------------------------------------------------

dashHead = dashboardHeader(title = "Gun Violence Incidents")

# sideBar (UI) ------------------------------------------------------------

sideBar = dashboardSidebar(
    sidebarMenu(
        menuItem("Country Level", tabName = "Xstate"),
        menuItem("State Level", tabName = "Xpie"),
#        menuItem("State Level", tabName = "XstateComp"),
        menuItem("Incident Level", tabName = "Xmap"),
        menuItem("Time Series Analysis", tabName = "timeseries")
    )
)

# body (UI) ---------------------------------------------------------------

body = dashboardBody(
    tabItems(
         tabItem(
         tabName = "Xmap", fluidPage(
            sidebarPanel(
                selectInput("filter", label = NULL, choices = names(victimsPerState[, 2:11]),
                            selected = "Terrorism")
            ),
            mainPanel(
                leafletOutput(
                    "map"
                    )
            )
         )),
        tabItem(
            tabName = "Xpie", fluidPage(
                sidebarLayout(
                    sidebarPanel(
                        selectInput("select", 
                                    label = h3("State of interest"), 
                                    choices = names(PieChart[,2:53]),
                                    selected = "USA") 
                    ), 
                    mainPanel(
                        plotlyOutput(
                            "pie"
                        )
                    )
                )
            )
            ),
        # tabItem(
        #   tabName = "XstateComp", fluidPage(
        #     sidebarPanel(
        #       selectInput("contrast",
        #                   label = h3("States for Comparison"),
        #                   choices = names(PieChart[,2:53]),
        #                   multiple = TRUE
        #         
        #       )
        #     ),
        #     mainPanel(
        #       plotOutput(
        #         "comp"
        #         )
        #     )
        #   )
        # ),
        tabItem(
            tabName = "Xstate", fluidPage(
                titlePanel("Number of victims from gun violence incidents in USA per 100,000"),
                sidebarLayout(
                    sidebarPanel(
                        selectInput("type", h3("State"),
                                    choices = names(victimsPerState[, 2:11]),
                                    selected = 1),
                        radioButtons("denominator", NULL,
                                     choices = c("Country Percentage", "Incidents per Capita"),
                                     selected = "Country Percentage"
                        )),
                    mainPanel(
                        plotOutput(
                            "state"
                        )
                    )
                )
            )
        ),
        tabItem(
          tabName = "timeseries", fluidPage(
            titlePanel("Time Series Analysis of Gun Violence Incidents"),
            sidebarLayout(
              sidebarPanel(
                wellPanel(
                  checkboxInput("checkbox", label = "Forecast", value = FALSE),
                  uiOutput("conditionalInput")
                ),
                radioButtons("frequency", "Frequency",
                             choices = c("Quarter", "Monthly")
                ),
                selectInput("select2", label = h3("Type of Incident"), 
                            choices = c("Wounded" = 2, "Killed" = 3, "Uninjured" = 4,
                                           "Terrorism" = 5, "Police" = 6, "Gang" = 7,
                                           "NonViolent" = 8, "SelfDefense" = 9, 
                                           "CrimeWithGun" = 10, "InvolvingChildren" = 11,
                                           "Accidents" = 12, "School" = 13, "Suicide" = 14))
              ),
              mainPanel(
                plotOutput(
                  "time"
                )
              )
            )
          )
        )
        )
    )

# Define UI ---------------------------------------------------------------

ui <- dashboardPage(dashHead, sideBar, body)

# Define server logic -----------------------------------------------------

server <- function(input, output) {
  
  output$map <- renderLeaflet({

    leaflet(X %>% filter(incident_characteristics == input$filter)) %>% 
      addTiles() %>% 
      addScaleBar %>%
      addMarkers(~longitude, ~latitude, 
                 label = paste0("<strong>Date: </strong>", X$date, 
                                "<br><strong>Victims: </strong>", X$victims, 
                                "<br><strong>Notes: </strong>", X$notes) %>% 
                   lapply(htmltools::HTML),
                 clusterOptions = markerClusterOptions())
  })
  
  output$pie <- renderPlotly({ 
      INPUT2 <- eval(parse(text = paste('PieChart$', input$select, sep = '')))
      plot_ly(PieChart, 
              labels = ~ incident_type, 
              values = ~ INPUT2, 
              type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = 'black'),
              hoverinfo = 'text',
              text = ~ paste0("Incident type: ", PieChart$incident_type, "\n", 
                              "Percentage: ", round(INPUT2/sum(INPUT2)*100, 2), "%"),
              marker = list(colors = factor(PieChart$incident_type, 
                                            labels = RColorBrewer::brewer.pal(
                                                length(unique(PieChart$incident_type)), 
                                                name = "Paired")
              ),
              line = list(color = '#FFFFFF', width = 1)),
              #The 'pull' attribute can also be used to create space between the sectors
              showlegend = FALSE) %>%
          layout(title = paste('Types of Incidents in ', input$select),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # output$comp <- renderPlot({
  #   ggplot(data = StatesComp, aes(x = incident_type, y = input$contrast)) + geom_col(position = "dodge2")
  # })
  
  output$state <- renderPlot({ 
      dataframe <- switch(input$denominator,
                          "Country Percentage" = percVictimsPerState,
                          "Incidents per Capita" = victimsPerState)
      legend <- switch(input$denominator,
                       "Country Percentage" = "Percent",
                       "Incidents per Capita" = "Incidents per 100,000")
      usmap::plot_usmap(regions = "state", data = dataframe, values = input$type) +
          theme(panel.background = element_rect(colour = "black")) +
          scale_fill_continuous(
              low = "white", high = "red", name = legend, label = scales::comma
          ) + theme(legend.position = "right")
  })
    
  output$conditionalInput <- renderUI({
    if(input$checkbox){
      sliderInput("slider1", label = h5("Forecast Periods"), min = 1,max = 25, value = 3)
    }
  })
    
  output$time <- renderPlot({
    
    quarter.ts <- ts(time_incidents, start = c(2014, 1), end = c(2018, 1), frequency = 4)
    monthly.ts <- ts(time_incidents, start = c(2014, 1), end = c(2018, 3), frequency = 12)
    
    if(input$checkbox == TRUE){
      # linear model prediction with weekday
      week <- switch(input$frequency,
                     "Quarter" = forecast(auto.arima(quarter.ts[,as.numeric(input$select2)]), input$slider1),
                     "Monthly" = forecast(auto.arima(monthly.ts[,as.numeric(input$select2)]), input$slider1)
      )
      plot(week)
      }
    else{
           freq <- switch(input$frequency,
                     "Quarter" = quarter.ts,
                     "Monthly" = monthly.ts)
           plot(decompose(freq[,as.numeric(input$select2)]))
    }
  })
}


# Run the app -------------------------------------------------------------

shinyApp(ui = ui, server = server)
