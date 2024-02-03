
#Shiny app

#Installing all the required packages 
#install.packages("shiny")
#install.packages("shinythemes")
#install.packages("ggplot2")
#install.packages("readr")
#install.packages("leaflet")

library(shiny)
library(shinythemes)
library(ggplot2)
library(readr)
library(leaflet)
library(arrow)

# Load the dataset
df <- read_parquet("/Users/hr/Downloads/data_final.parquet", stringsAsFactors = FALSE)
df$time <- as.Date(df$time)  # Convert the time column to Date type
test_data <- na.omit(df)

# Define UI for the Shiny app
ui <- fluidPage(
  theme = shinytheme("united"),  
  titlePanel("Energy & Temperature Variations Across South Carolina"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countySelect", "Select County:", 
                  choices = c("All Counties", unique(test_data$in.county.x)),
                  selected = "All Counties"),
      selectInput("bldgSelect", "Select Building:",
                  choices = c("All Buildings", sort(unique(test_data$bldg_id))),
                  selected = "All Buildings"),
      sliderInput("tempRange", "Temperature Range:",
                  min = min(test_data$median_Dry_Bulb_Temperature, na.rm = TRUE), 
                  max = max(test_data$median_Dry_Bulb_Temperature, na.rm = TRUE),
                  value = c(min(test_data$median_Dry_Bulb_Temperature, na.rm = TRUE), 
                            max(test_data$median_Dry_Bulb_Temperature, na.rm = TRUE))),
      sliderInput("energyRange", "Energy Consumption Range:",
                  min = min(test_data$total_energy_consumed, na.rm = TRUE), 
                  max = max(test_data$total_energy_consumed, na.rm = TRUE),
                  value = c(min(test_data$total_energy_consumed, na.rm = TRUE), 
                            max(test_data$total_energy_consumed, na.rm = TRUE))),
      dateRangeInput("dateRange", "Date Range:",
                     start = min(test_data$time), end = max(test_data$time),
                     min = min(test_data$time), max = max(test_data$time))
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Temperature Distribution", plotOutput("tempPlot")),
                  tabPanel("Energy Consumption", plotOutput("energyPlot"))
      )
    )
  )
)

# Define the server logic for the Shiny app
server <- function(input, output) {
  
  filteredData <- reactive({
    data <- test_data
    
    # If a specific county is selected (not 'All Counties'), filter the data for that county
    if (input$countySelect != "All Counties") {
      data <- data[data$in.county.x == input$countySelect, ]
    }
    
    # If a specific building is selected (not 'All Buildings'), filter the data for that building
    if (input$bldgSelect != "All Buildings") {
      data <- data[data$bldg_id == input$bldgSelect, ]
    }
    
    # Further filter the data based on the temperature range selected by the user
    data <- data[data$median_Dry_Bulb_Temperature >= input$tempRange[1] & 
                   data$median_Dry_Bulb_Temperature <= input$tempRange[2], ]
    
    # Filter the data based on the energy consumption range selected by the user
    data <- data[data$total_energy_consumed >= input$energyRange[1] & 
                   data$total_energy_consumed <= input$energyRange[2], ]
    
    # If a date range is selected, filter the data based on the selected date range
    if (!is.null(input$dateRange)) {
      data <- data[data$time >= input$dateRange[1] & data$time <= input$dateRange[2], ]
    }
    
    # Return the filtered dataset
    return(data)
  })
  
  # Generate the plot for energy consumption over time
  output$energyPlot <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = time, y = total_energy_consumed)) + 
      geom_line(group = 1, color = "steelblue") +  # Use a line plot to show energy consumption over time
      labs(title = "Energy Consumption Over Time", x = "Day", y = "Energy Consumed") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Generate the plot for temperature distribution with day on x-axis and temperature on y-axis
  output$tempPlot <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = time, y = median_Dry_Bulb_Temperature)) + 
      geom_line(group = 1, color = "darkred") +  # Use a line plot
      labs(title = "Temperature Distribution Over Time", x = "Day", y = "Temperature (°C)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)