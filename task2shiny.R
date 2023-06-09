library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(readxl)

#centers ggplot title
theme_update(plot.title = element_text(hjust = 0.5))

#reads in exceedance beach monthly then converts measurements to percents
#by multiplying by 100
#then adds month nums
exceedance_beach_monthly = read.csv("exceedance_beach_monthly.csv")
exceedance_beach_monthly = data.frame(exceedance_beach_monthly[1], exceedance_beach_monthly[-1] * 100)
exceedance_beach_monthly = exceedance_beach_monthly %>% mutate(date_num = seq(1,12))

#reads in exceedance station monthly then converts measurements to percents
#by multiplying by 100
#then adds month nums
exceedance_station_monthly  = read.csv("exceedance_station_monthly.csv")
exceedance_station_monthly = data.frame(exceedance_station_monthly[1], exceedance_station_monthly[-1] * 100)
exceedance_station_monthly  = exceedance_station_monthly %>% mutate(date_num = seq(1,12))

#reads in exceedance beach yearly
#the first column is years which is correctly read in as int
#all the other columns are read as chr since they have a percent sign in them
#example read in as chr "3.45%" so we need to remove the percent sign and conver to numeric
exceedance_beach_yearly = read.csv("exceedance_beach_yearly.csv")
#deletes percent signs by deleting last charachter of every column except the first which is a year
exceedance_beach_yearly = data.frame(exceedance_beach_yearly[1], as.data.frame(sapply(exceedance_beach_yearly[-1], str_sub, end = -2)))
#converts to numeric
exceedance_beach_yearly = as.data.frame(sapply(exceedance_beach_yearly, as.numeric))
colnames(exceedance_beach_yearly)[1] = "date_num"

#reads in exceedance station yearls
#exceedance station yearly is not stored as percentages so we will convert it 
#to percents to be consistent with exceedance beach yearly
#so we will multiply every column by 100 excent the first column which are years
exceedance_station_yearly = read.csv("exceedance_station_yearly.csv")
exceedance_station_yearly = data.frame(exceedance_station_yearly[1], exceedance_station_yearly[-1] * 100)
colnames(exceedance_station_yearly)[1] = "date_num"

#reads in gm beach yearly
#the csv file was saved with a bunch of missing rows so we have to remove them using na.omit()
gm_beach_yearly = read.csv("gm_beach_yearly.csv")
colnames(gm_beach_yearly)[1] = "date_num"
gm_beach_yearly = gm_beach_yearly %>% na.omit()

#reads in gm station yearly
#again have to correct missing values
gm_station_yearly = read.csv("gm_station_yearly.csv")
colnames(gm_station_yearly)[1] = "date_num"
gm_station_yearly = gm_station_yearly %>% na.omit()

#reads in gm beach monthly
#in gm beach monthly csv, date is stored as character month followed by year
#ex: "January-09" for January 2009
#we change to a date time value, note the date time value adds a day value (first of the month)
#this is uninformative since the row represents the entire month, not a specific day
gm_beach_monthly = read.csv("gm_beach_monthly.csv")
colnames(gm_beach_monthly)[1] = "date_num"
gm_beach_monthly = gm_beach_monthly %>% separate(col = date_num, into = c("Month", 'Year'))
gm_beach_monthly$Month = substr(gm_beach_monthly$Month, 1, 3)
gm_beach_monthly = gm_beach_monthly %>% unite(col = "date_num", c("Year", "Month"), sep = '-')
gm_beach_monthly$date_num = ym(gm_beach_monthly$date_num)

#reads in gm station yearly
#it adds day to the date which I don't like but will fix later
#for now just know that the day is meaningless since it is an average across
#a specific month in a given year
gm_station_monthly = read.csv("gm_station_monthly.csv")
gm_station_monthly = gm_station_monthly %>% unite(col = "date_num", c("YEAR", "Month"), sep = '-')
gm_station_monthly$date_num = ym(gm_station_monthly$date_num)

#reads in name of beaches and other info from web
names = read_excel("beach_names.xlsx")

#creates the UI page layout
ui <- fluidPage(
  wellPanel(
    #whether the user wants monthly or yearly data
    selectInput("time_scale", label ="Choose a Time Scale", choices = c("Monthly" = "monthly", "Yearly" = "yearly")),
    
    #whether the user wants exeedence or geometric mean
    selectInput("measurement", label = "Choose a Measurement Type", choices = c("Exceedence" = "exceedance", 
                                                                                "Geometric Mean" = "gm")),
    
    #whether the user wants the beach or the station
    selectInput("location_type", label = "Choose a Location Type", choices = c("Beach" = "beach", 
                                                                               "Station" = "station")),
    
    #if the user wants beach data, allows them to select the beach
    #will only show up if location_type is beach
    conditionalPanel(
      condition="input.location_type == 'beach'",
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
    
    #if the user wants station data, allows them to select the station
    #will only show up if location_type is station
    conditionalPanel(
      condition="input.location_type == 'station'",
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
  
  #plots the output graph
  plotOutput("output_plot", width = "500px"),
)

#creates the graph the user will actually see
server <- function(input, output, session) {
  
  output$output_plot = renderPlot({
      
      #dynamically changes the name of the accessed data set based on the user input
      data_Set_name = paste0(input$measurement,"_", input$location_type,"_", input$time_scale)
      #since it is stored as a character we need to change it to a symbol so
      #that r recognizes it as an object
      data_set = eval(as.symbol(data_Set_name))
      
      if(input$measurement == "gm"){
        y_label = "Geometric Mean"
      } else{
        y_label = "Percent Exceedance"
      }
      
      #changes the column the plot will access
      #based on if selected as beach or station
      if(input$location_type == 'beach'){
        location_id = input$beach_id
        title_location = 'Beach'
      } else{
        location_id = input$station_id
        title_location = 'Site'
      }
      
      #creates the ticks marks for the ggplot based on if data
      #is monthly or yearly
      #break list is the location of the tick marks for the x-axis
      #label list is the names that will show up
      #the names are years if yearly and months if monthly
      
      if(input$time_scale == 'monthly'){
        break_list = seq(1,12)
        label_list = substr(month.name, 1, 3)
        
        x_label = "Month"
        title_time_Scale = "Monthly"
      } else{ #occurs if time_scale is Year
        #since different data sets have different start and end years we choose
        #the years that will be represented dynamically
        min_year = min(data_set[1])
        max_year = max(data_set[1])
        break_list = seq(min_year, max_year, by = 2)
        label_list = break_list
        
        x_label = "Year"
        title_time_Scale = "Yearly"
      }
      
      title = paste(title_location, title_time_Scale, y_label)
      
      #we needed to use na.rm = TRUE because in the gm monthly for beaches and stations
      #there are months without measurements
      
      #plots the dynamically created graph
      ggplot(data = data_set, aes_string(x = "date_num", y = location_id)) +
      geom_point(na.rm = TRUE, colour = "purple") +
      stat_smooth(method = "lm",
                  formula = y ~ x,
                  geom = "smooth",
                  na.rm = TRUE) +
      xlab(x_label) +
      ylab(y_label) +
      scale_x_continuous(breaks = break_list,labels = label_list) +
      ggtitle(title)
  }, res = 96)
}

#runs the app
shinyApp(ui, server)