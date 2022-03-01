library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(ggiraph)
library(tigris); options(tigris_class = "sf")
library(lubridate)

#=============================================================================

# Make California county map with simplified shapes
county_map <- counties(state = "CA", cb = TRUE) %>%
    select(county = NAME) # geometry automatically saved with sf object


ca_covid_url <- "https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv"

# Read Covid data directly from CHHS Open Data Portal and process data
covid_data <- 
    read_csv(ca_covid_url) %>%
    mutate(year_month = floor_date(date, "month")) %>%
    rename(county = area) %>%
    group_by(county, year_month) %>%
         summarize(Cases = sum(cases),
                   Deaths = sum(deaths))

# join simple feature "shape file" and data
data_map <- left_join(county_map, covid_data, by= "county")

# plotting function to plot trend for selected county
example_trend_plot_function <- function(myCounty="Fresno") {

    tDat <- covid_data %>% filter(county == myCounty)
    
    ggplot(data = tDat, aes(x=year_month, y=Cases)) + 
    geom_line(size = 1) +
    labs(title = myCounty) +
      # theme_bw() +
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
                choices=c("cases","deaths"), 
                selected="deaths"),
    
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
            
        g <- ggplot(data_map) +
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
        
        trendStep        <- reactive(example_trend_plot_function( myCounty() ))
        
        
        
        output$trendjunk <- renderPlot(trendStep())
                           #  DOES NOT WORK:    
                           #  example_trend_plot_function(input$cmap_selected)
                           # NOR DOES THIS:
                           # trendStep()
        
}


shinyApp(ui = ui, server = server)
