library(shiny)
library(ggplot2)
library(dplyr)
exceedance_beach_monthly = read.csv("C:/Users/Kyle/Desktop/Galveston Consulting/Past Work/Task 2/Data/exceedance_beach_monthly.csv")
exceedance_beach_monthly = exceedance_beach_monthly %>% mutate(date_num = seq(1,12))

exceedance_station_monthly  = read.csv("C:/Users/Kyle/Desktop/Galveston Consulting/Past Work/Task 2/Data/exceedance_station_monthly.csv")
exceedance_station_monthly  = exceedance_station_monthly %>% mutate(date_num = seq(1,12))

sites = c(1,3,5,7,13,14,17,19,21,22,23,24,25,26,27,28,30,32,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,53,55)
sites = paste("Site", sites)

beaches = seq(1, 14)
beaches = paste("Beach", beaches)

ui <- fluidPage(
  wellPanel(
    selectInput("time_scale", label ="Choose a Time Scale", choices = c("Monthly" = "Month", "Yearly" = "Year")),
    selectInput("measurement", label = "Choose a Measurement Type", choices = c("Exceedence", "Geometric Mean")),
    selectInput("location_type", label = "Choose a Location Type", choices = c("Beach", "Station")),
    conditionalPanel(
      condition="input.location_type == 'Beach'",
      selectInput(inputId = "beach_id", 
                  label = "Select Beach ID:",
                  choices = c("Beach 1" = "TX822495",
                              "Beach 2" = "TX767833",
                              "Beach 3" = "TX239942",
                              "Beach 4" = "TX974690",
                              "Beach 5" = "TX334226",
                              "Beach 6" = "TX226514",
                              "Beach 7" = "TX751320",
                              "Beach 8" = "TX163187",
                              "Beach 9" = "TX393353",
                              "Beach 10" = "TX486021",
                              "Beach 11" = "TX214299",
                              "Beach 12" = "TX710697",
                              "Beach 13" = "TX451421",
                              "Beach 14" = "TX327206")
      ),
    ),
    conditionalPanel(
      condition="input.location_type == 'Station'",
      selectInput(inputId = "station_id", 
                  label = "Select Station ID:",
                  choices = c("Site 1" = "GAL001",
                              "Site 3" = "GAL003",
                              "Site 5" = "GAL005",
                              "Site 7" = "GAL007",
                              "Site 13" = "GAL013",
                              "Site 14" = "GAL014",
                              "Site 17" = "GAL017",
                              "Site 19" = "GAL019",
                              "Site 21" = "GAL021",
                              "Site 22" = "GAL022",
                              "Site 23" = "GAL023",
                              "Site 24" = "GAL024",
                              "Site 25" = "GAL025",
                              "Site 26" = "GAL026",
                              "Site 27" = "GAL027",
                              "Site 28" = "GAL028",
                              "Site 30" = "GAL030",
                              "Site 32" = "GAL032",
                              "Site 34" = "GAL032",
                              "Site 35" = "GAL035",
                              "Site 36" = "GAL036",
                              "Site 37" = "GAL037",
                              "Site 38" = "GAL038",
                              "Site 39" = "GAL039",
                              "Site 40" = "GAL040",
                              "Site 41" = "GAL041",
                              "Site 42" = "GAL042",
                              "Site 44" = "GAL044",
                              "Site 45" = "GAL045",
                              "Site 46" = "GAL046",
                              "Site 47" = "GAL047",
                              "Site 48" = "GAL048",
                              "Site 49" = "GAL049",
                              "Site 50" = "GAL050",
                              "Site 53" = "GAL053",
                              "Site 55" = "GAL055"
                              )
                  )
      ),
  ),
  #plotOutput("plot", width = "400px"),
  conditionalPanel(
    condition="input.location_type == 'Beach'",
    plotOutput("beach_plot", width = "500px")
  ),
  conditionalPanel(
    condition="input.location_type == 'Station'",
    plotOutput("station_plot", width = "500px")
  )
)
server <- function(input, output, session) {
  output$beach_plot = renderPlot({ggplot(data = exceedance_beach_monthly, aes_string(x = "date_num", y = input$beach_id)) +
      geom_point() +
      stat_smooth(method = "lm",
                  formula = y ~ x,
                  geom = "smooth") +
      xlab(input$time_scale) +
      ylab(input$measurement) +
      scale_x_continuous(breaks = seq(1,12),labels = substr(month.name, 1, 3))
    }, res = 96)
  
  output$station_plot = renderPlot({ggplot(data = exceedance_station_monthly, aes_string(x = "date_num", y = input$station_id)) +
      geom_point() +
      stat_smooth(method = "lm",
                  formula = y ~ x,
                  geom = "smooth") +
      xlab(input$time_scale) +
      ylab(input$measurement) +
      scale_x_continuous(breaks = seq(1,12),labels = substr(month.name, 1, 3))
    }, res = 96)
  
}

shinyApp(ui, server)