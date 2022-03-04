library(shiny)
library(dplyr) # core "tidyverse" package for data manipulation
library(readr) # tools for reading all sorts of data into R
library(ggplot2) # data visualization package
library(ggiraph) # Tool to create dynamic ggplot graphs
library(tigris) # package for reading "tiger" "shape files" for making maps
options(tigris_class = "sf") # causes tigris to read files in the "simple features" format
library(lubridate) # makes working with dates easy

# Read in California COVID-19 data 
covid_data <- readRDS("covid_data.RDS")

# Read in data for mapping
county_map <- readRDS("county_map.RDS")

# plotting function to plot trend for selected county
example_trend_plot_function <- function(myCounty="Fresno", myMeasure = "Cases") {

    tDat <- covid_data %>% filter(county == myCounty)
    
    ggplot(data = tDat, aes(x=year_month, y=get(myMeasure))) + 
    geom_line(size = 1) +
    labs(title = myCounty, x = "Year-Month", y = myMeasure) +
      theme_bw() +
      theme(plot.title = element_text(face = "bold", size = 20, colour = "darkblue"), 
            axis.title = element_text(face = "bold", size = 18), 
            axis.text = element_text(size = 16))
}

#=============================================================================

ui <- fluidPage(theme = "sandstone",

    titlePanel("R / R Shiny / Map Example"),
    
    br(), 
    
    selectInput("metric",   # not used yet
                "metric:",
                choices=c("Cases","Deaths"), 
                selected="Deaths"),
    
    hr(), 
    
    fluidRow(
      column(width = 1),
      column(width = 3, 
             ggiraphOutput("cmap", height = "500px")), 
      column(width = 7, 
             plotOutput("trendjunk", height = "500px")), 
      column(width = 1)
    )
)

server <- function(input, output) {

        output$cmap <- renderggiraph({
            
        g <- ggplot(county_map) +
                geom_sf(size=3,col="black") +
                geom_sf_interactive(aes(tooltip = county,
                                        data_id = county),
                                    size = 0.5) + 
                theme_void()
            
         ggiraph(code = print(g),
                    height_svg = 8,
                    selection_type = "single",
                    hover_css = "cursor:pointer;fill:blue;stroke:blue;")

        })
  
        selectedCounty <- reactive(input$cmap_selected)
        myCounty <- reactive( if ( is.null( selectedCounty() ) ) "Fresno" else input$cmap_selected )
        
        trendStep        <- reactive(example_trend_plot_function( myCounty = myCounty(), myMeasure = input$metric ))
        
        output$trendjunk <- renderPlot(trendStep())
                           
        
}


shinyApp(ui = ui, server = server)
