library(tidyverse)
library(ggiraph)
library(ggplot2)
library(maps)
library(stringr)
library(RColorBrewer)
library(scales)

options(encoding = 'UTF-8')

world_data = ggplot2::map_data('world')
world_data = fortify(world_data)

global_inflation = read_csv("https://github.com/zaynez4/stat436/raw/refs/heads/main/project/milestone2/tidy_global_inflation_with_NAs.csv")

covid_data = read_csv("https://github.com/zaynez4/stat436/raw/refs/heads/main/project/milestone3/covid-data.csv") %>% 
  group_by(location, year(date), continent) %>% 
  summarize(cases = max(total_cases)) %>% 
  rename(year = "year(date)",
         region = location) %>% 
  drop_na() %>% 
  mutate(region = case_when(
    str_detect(region, "United States") ~ "USA",
    str_detect(region, "United Kingdom") ~ "UK",
    str_detect(region, "Czechia") ~ "Czech Republic",
    str_detect(region, "Democratic Republic of Congo") ~ "Democratic Republic of the Congo",
    str_detect(region, "Congo") ~ "Republic of Congo",
    TRUE ~ region
  ))
  

# correct the names so we can do a successful left_join
corrected_country_names = global_inflation %>% 
  mutate(Country = case_when(
    str_detect(Country, "United States") ~ "USA",
    str_detect(Country, "Congo, Dem.") ~ "Democratic Republic of the Congo",
    str_detect(Country, "Congo, Rep.") ~ "Republic of Congo",
    str_detect(Country, "Venezuela") ~ "Venezuela",
    str_detect(Country, "Egypt") ~ "Egypt",
    str_detect(Country, "Russian") ~ "Russia",
    str_detect(Country, "Kyrgyz Republic") ~ "Kyrgyzstan",
    str_detect(Country, "Iran, Islamic") ~ "Iran",
    str_detect(Country, "United Kingdom") ~ "UK",
    str_detect(Country, "Slovak Republic") ~ "Slovakia",
    str_detect(Country, "Lao PDR") ~ "Laos",
    str_detect(Country, "Yemen, Rep.") ~ "Yemen",
    str_detect(Country, "Tanzania") ~ "Tanzania",
    TRUE ~ Country
  ))

# join the country names with world_data and get rid of subregion column
global_map_data = corrected_country_names %>% 
  rename(region = Country) %>%
  inner_join(world_data, by="region") %>% 
  select(-subregion)

# get the five inflation types and add the average option
inflation_types = append(
  global_map_data %>% 
    pull(type) %>% 
    unique(),
  "Average Inflation Across All Types"
)

# get all country names
all_countries = global_map_data %>% 
  arrange(region) %>% 
  pull(region) %>% 
  unique()
  
# join headliner consumer inflation rates with the covid dataset
joined_covid_inflation = global_map_data %>% 
  filter(type == "Headline Consumer Price Inflation") %>% 
  select(region, rate, year) %>% 
  filter(year >= 2020 & year <= 2022) %>% 
  drop_na() %>% 
  unique() %>% 
  inner_join(covid_data, by="region") %>% 
  select(-year.y) %>% 
  rename(year = year.x) %>% 
  group_by(region, rate, year, continent) %>% 
  summarize(cases = max(cases))

# create a function to recreate the world map given the df
map = function(world_data) {
  # make the rate range be from -25% to 25% so super higher rates 
  # do not skew the coloring
  world_data = world_data %>% 
    mutate(rate = case_when(
      rate > 25 ~ 25,
      rate < -25 ~ -25,
      TRUE ~ rate
    ))
  
  g = ggplot(world_data) +
    geom_polygon_interactive(aes(x = long, y = lat, 
                                 fill=rate, group = group),
                             size = 0.1) +
    scale_fill_gradientn(colors=rev(brewer.pal(8, "RdBu")),
                         na.value="gray55",
                         breaks = c(-25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25), 
                         labels = c("<= -25", "-20", "-15", "-10", "-5", "0", "5", "10", "15", "20", ">= 25")) +
    labs(title = "Choropleth Map of Given Inflation Type",
         x = NULL,
         y = NULL,
         fill = "Inflation Rate") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_text(size=7),
          legend.text = element_text(size=6),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  
  return(g)
}

# create a function to display the time series
timeseries = function(world_data) {
  ggplot(world_data, aes(year, rate, col=region)) +
    geom_line() +
    annotate("rect", xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf, 
             alpha = 0.2, fill = "red") +
    scale_color_manual(values = rev(brewer.pal(4, "RdBu"))) +
    labs(title = "Time Series Comparing the Given Countries",
         subtitle = "based upon the inflation type selected",
         x = "Year",
         y = "Inflation Rate",
         col = "Country")
}

# create a function to show the scatterplot comparing headline consumer
# price inflation rate and COVID cases
scatterplot = function(rate_covid_data) {
  ggplot(rate_covid_data, aes(cases, rate, col=continent)) +
    geom_point() +
    scale_x_log10(oob = scales::squish_infinite) +
    scale_y_log10(oob = scales::squish_infinite) +
    scale_color_brewer(palette = "RdYlBu") +
    labs(title = "Comparative Plot of COVID-19 Cases and Inflation Rate",
         x = "Log of COVID-19 Cases",
         y = "Log of Headline Consumer Price Inflation Rate",
         col = "Continent")
}

# create the UI
ui = fluidPage(
  titlePanel("Analyzing Inflation Rates for each Country"),
  tabsetPanel(
    tabPanel("Global Heatmap",
              sidebarLayout(
               sidebarPanel(
                h4("Choose from the inputs below to compare inflation rates"),
                # first input, which inflation type
                selectInput("i_type", 
                           "Choose the inflation type you want on the map",
                           choices=inflation_types),
               
               # hide the blue line in the slider
               tags$style(HTML("
                          .js-irs-0 .irs-bar {
                          display: none;
                          }
                          ")),
               h5(strong("About each inflation type:")),
               p("Headline Consumer Price Inflation is the total inflation in the economy."),
               p("Energy Consumer Price Inflation is the rate at which energy products (e.g. electricity) increase."),
               p("Food Consumer Price Inflation is how food prices change over time."),
               p("Official Core Consumer Price Inflation is the changes in price of goods and services, excluding food and energy."),
               p("Producer Price Inflation is the average change of selling prices by domestic producers."),
               
               # second input, which year
               sliderInput("year", "Select the year", 
                           min=min(global_inflation$year),
                           max=max(global_inflation$year), 
                           value=2000,
                           sep="")
             ),
             # display the outputs
             mainPanel(
               girafeOutput("map", width="82%", height="82%")
             )
          ),
          
          h5(strong("Important things to note on this visualization below:")),
          p("The inflation rates were measured at the **beginning** of the year and are a positive or negative change from the previous year."),
          p("If a country is colored gray, then there is no data recorded for those inputs."),
          p("If a country is not outlined/filled in, then it does not keep track of data for the inflation type.")
    ),
    tabPanel("Comparative Time Series",
             sidebarLayout(
               sidebarPanel(
                 # first input, which inflation type
                 selectInput("i_type2", 
                             "Choose the inflation type you want on the map",
                             choices=inflation_types),
                 
                 h5(strong("About each inflation type:")),
                 p("Headline Consumer Price Inflation is the total inflation in the economy."),
                 p("Energy Consumer Price Inflation is the rate at which energy product (e.g. electricity) increase."),
                 p("Food Consumer Price Inflation is how food prices change over time."),
                 p("Official Core Consumer Price Inflation is the changes in price of goods and services, excluding food and energy."),
                 p("Producer Price Inflation is the average change of selling prices by domestic producers."),
                 
                 # second input, countries to compare
                 selectizeInput("countries", 
                                "Choose up to 4 countries",
                                choices = all_countries,
                                multiple = TRUE,
                                options = list(maxItems = 4))
              ),
              mainPanel(
                plotOutput("timeSeries"),
              )
            ),
            h5(strong("Important things to note on this visualization below:")),
            p("The red highlighted area between 2020-2022 is help focus on the effects of COVID-19."),
            p("Some countries may not have data recorded at a year."),
          ),
      tabPanel("The Effect of COVID-19",
               sidebarLayout(
                 sidebarPanel(
                   # the only input will be choosing the year
                   selectInput("year_2", "Select a year",
                               choices = c(2020, 2021, 2022)),
                   h5(strong("How to interact with the scatterplot:")),
                   p("Drag your mouse over the data points you want to display in the table below the notes."),
                 ),
                 mainPanel(
                   plotOutput("scatterplot", brush = "plot_brush")
                 )
               ),
               
               h5(strong("Important things to note on this visualization below:")),
               p("Only Headline Consumer Price Inflation is used because that is the total inflation rate in the economy. 
                 This is represented on the y-axis."),
               p("Both the x-axis and y-axis were scaled by log10."),
               p("The x-axis is labeled in scientific notation, so 1e04 means 10,000, 1e5 means 100,000, etc."),
               p("The y-axis is labeled in scientific notation only for 2021 from higher inflation rates."),
               br(),
               h5(strong("The data points that you brushed are below:")),
               dataTableOutput("df")
          )
    )
  )


# create the server
server = function(input, output) {
  subset_world_data_heatmap = reactive({
    if (input$i_type == "Average Inflation Across All Types") {
      # summarize to get the average across all types given the year
      global_map_data %>% 
        filter(year == input$year) %>% 
        drop_na() %>% # cannot take a mean with NA values
        group_by(region) %>% 
        summarize(rate = mean(rate)) %>% 
        left_join(world_data, by="region") %>% 
        select(-subregion)
    }
    else{
      # filter to the specific inflation type chosen
      global_map_data %>% 
        filter(year == input$year &
                 type == input$i_type)
    }
  })
  
  subset_world_data_timeseries = reactive({
    if (input$i_type2 == "Average Inflation Across All Types") {
      # summarize to get the average across all types given the year
      global_map_data %>% 
        drop_na() %>% # cannot take a mean with NA values
        group_by(region, year) %>% 
        summarize(rate = mean(rate)) %>% 
        left_join(world_data, by="region") %>% 
        select(-subregion) %>% 
        filter(region %in% input$countries)
    }
    else{
      # filter to the specific inflation type chosen
      global_map_data %>% 
        filter(type == input$i_type2 &
               region %in% input$countries)
    }
  })
  
  subset_covid_inflation = reactive({
    joined_covid_inflation %>% 
      filter(year == input$year_2)
  })
  
  selected = reactiveVal(data.frame(region = character(0), continent = character(0),
                                    year = numeric(0), cases = numeric(0),
                                    rate = numeric(0), stringsAsFactors = FALSE)
  )
  
  observeEvent(
    input$plot_brush,
    selected(brushedPoints(subset_covid_inflation(), input$plot_brush))
  )
  
  output$map = renderGirafe({
    girafe(code = print(map(subset_world_data_heatmap())))
  })
  
  output$timeSeries = renderPlot({
    timeseries(subset_world_data_timeseries())
  })
  
  output$scatterplot = renderPlot({
    scatterplot(subset_covid_inflation())
  })
  
  output$df = renderDataTable({
    selected() %>% 
      relocate(continent, .after=region) %>% 
      relocate(rate, .after=cases)
  })
}

shinyApp(ui, server)
