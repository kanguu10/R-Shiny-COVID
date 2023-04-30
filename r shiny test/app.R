# setwd('C:/work/projects/r shiny test')
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")
#-------------------------Package Installer--------------------------
# load packages and install if missing
# thanks to Richard Schwinn for the code, http://stackoverflow.com/a/33876492

# # list the packages you need
# p <- c("tidyverse", 'sf', 'purrr','readxl', 'data.table', 'tictoc',
#        'beepr','readr','lubridate', 'ggthemes', 'scales', 'extrafont', 'dplyr',
#        'zoo', 'shiny','bslib', 'plotly', 'shinythemes', 'tmap', 'leaflet', 'extrafont', 'showtext')
# 
# # this is a package loading function
# 
# # this is a package loading function
# loadpacks <- function(package.list = p){
# new.packages <- package.list[!(package.list %in% installed.packages()[,'Package'])]
#   if(length(new.packages)) {
#     install.packages(new.packages, Ncpus = parallel::detectCores(), type = "binary")
#   }
# lapply(eval(package.list), require, character.only = TRUE)
# }
# 
# loadpacks(p) # calling function to load and/or install packages
# rm(loadpacks, p) # cleanup namespace
# 
# #----------------------End of Package Installer----------------------
# 
# ## Loading Google fonts (https://fonts.google.com/)
# font_add_google("Roboto", "roboto")
# 
# ## Automatically use showtext to render text
# showtext_auto()

# The dataset of our study contains daily & cumulative number of COVID-19 tests conducted, number of positive, hospitalized, recovered & death cases reported by country. In details here are the columns in the dataset:
# 
# Date: Date
# Continent_Name: Continent names
# Two_Letter_Country_Code: Country codes
# Country_Region: Country names
# Province_State: States/province names; value is All States when state/provincial level data is not available
# positive: Cumulative number of positive cases reported.
# active: Number of actively cases on that day.
# hospitalized: Cumulative number of hospitalized cases reported.
# hospitalizedCurr: Number of actively hospitalized cases on that day.
# recovered: Cumulative number of recovered cases reported.
# death: Cumulative number of deaths reported.
# total_tested: Cumulative number of tests conducted.
# daily_tested: Number of tests conducted on the day; if daily data is unavailable, daily tested is averaged across number of days in between.
# daily_positive: Number of positive cases reported on the day; if daily data is unavailable, daily positive is averaged across number of days in.

library(shiny)
library(tidyverse)
library(ggplot2)
library(data.table)
library(maps)
require(scales)


#reading data
df = fread('owid-covid-data.csv')

'%nin%' <- Negate('%in%')
not_countries = c('World', 'Asia','Lower middle income',"Africa", "European Union", "Low income", "South America",
                  "Upper middle income", "High income", "Oceania",'Europe', "North America")
df = df %>%
  filter(location %nin% not_countries)
# df = fread('covid19.csv')
# df = df %>%
#   filter(Province_State=='All States')
# 
# df_grouped_date = df %>%
#   group_by(Date) %>%
#   summarise(total_tested=sum(total_tested))
# df_grouped_date$Date = ymd(df_grouped_date$Date)

# country list for selectInput
countrylist <- sort(unique(df$location))
# y-axis columns
columns_yaxis = colnames(df[, 5:67])


#plot function for country selector
df = df[order(df$location),]
draw_plot <- function(country_to_filter_by) {
  filtered_df <- df %>%
    filter(location == country_to_filter_by)
  ggplot(filtered_df, aes(x=date, y=total_tested)) +
    geom_line(color='pink',size=2) +
    scale_y_continuous(name="Number", labels = comma) +
    theme(text = element_text(size = 20),
          plot.background = element_rect(fill = "grey"))
}

# map data preps
world_data <- ggplot2::map_data('world')
# world_data <- fortify(world_data)
head(world_data)











# Define UI
ui <- fluidPage(
  sidebarLayout(
        sidebarPanel(
          selectInput("y_var", label = "Select Y-axis variable:", choices = columns_yaxis),
          selectInput("date", "Select Date:", choices = unique(df$date)),
          selectInput("region", label = "Select Country:", choices = sort(unique(df$location)))
                    )
      ,
      # mainPanel(h2("This dashboard is about COVID-19 stats"),
      #           p("It consists of the sidebar panel on the left, where you can select Y-axis, date and country,\nand three plots: lineplot, map and barplot"),
      #           plotOutput("line_plot", width = '1000px'),
      #           plotOutput("map", width = '1000px'),
      #           p(strong("Please select the date and the Y-Axis to display them on the barplot below")),
      #           plotOutput("barplot")
      #           )
      
      mainPanel(h2("This dashboard is about COVID-19 stats"),
                p("It consists of the sidebar panel on the left, where you can select Y-axis, date and country,\nand three plots: lineplot, map and barplot"),
        fluidRow(
          column(width = 6, plotOutput("line_plot")),
          column(width = 6, plotOutput("map"))
        ),
        p(strong("Please select the date and the Y-Axis to display them on the barplot below")),
        fluidRow(
          column(width = 12, plotOutput("barplot"))
        )
      )
                )
)


# Define server
server <- function(input, output) {
  
  # Filter data based on user input
  data <- reactive({
    df %>% 
      filter(location == input$region)
  })
  
  # Create line plot
  output$line_plot <- renderPlot({
    ggplot(data(), aes(x = date, y = !!sym(input$y_var))) +
      geom_line(color='red', size=2) +
      labs(title = paste0("Line Plot of ", input$y_var, " by Date for ", input$region, '\n\nSelect the Y-axis from the sidebar to see some different variable'), family='roboto') +
      theme(text = element_text(size = 10),
            axis.text=element_text(size=12)) +
      title(family = "roboto") +
      scale_y_continuous(labels = comma)
  })
  
  # Create a reactive expression for the selected country
  selected_country <- reactive({
    world_map <- map_data("world")
    subset(world_map, region == input$region)
  })
  
  # Plot the world map with the selected country highlighted
  output$map <- renderPlot({
    ggplot() +
      geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), fill = "gray80") +
      geom_polygon(data = selected_country(), aes(x = long, y = lat, group = group), fill = "red") +
      theme_void()
  })
  
  # Filter data based on selected date
  filtered_data <- reactive({
    df %>%
      filter(date == input$date)
  })
  
  # Create barplot showing 10 locations with biggest value of selected column
  output$barplot <- renderPlot({
    top_10 <- filtered_data() %>%
      arrange(desc(!!sym(input$y_var))) %>%
      head(10)
    
    ggplot(top_10, aes(x = reorder(location, !!sym(input$y_var)), y = !!sym(input$y_var))) +
      geom_bar(stat = "identity", fill='skyblue') +
      xlab("Location") +
      ylab(input$y_var) +
      ggtitle(paste0("Top 10 Locations with Highest ", input$y_var, " on ", input$date)) +
      theme(text = element_text(size = 10),
            axis.text=element_text(size=12)) +
      title(family = "roboto") +
      scale_y_continuous(labels = comma)
  })
}

# Run the app
shinyApp(ui, server)