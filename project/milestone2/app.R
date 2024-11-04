library(tidyverse)
library(ggiraph)
library(ggplot2)
library(maps)

options(encoding = 'UTF-8')

world_data = ggplot2::map_data('world')
world_data = fortify(world_data)

global_inflation = read_csv("https://github.com/zaynez4/stat436/raw/refs/heads/main/project/milestone2/tidy_global_inflation_with_NAs.csv")

# correct the names so we can do a successfull left_join
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
    TRUE ~ Country
  ))

# join the country names with world_data and get rid of subregion column
global_map_data = corrected_country_names %>% 
  rename(region = Country) %>%
  left_join(world_data, by="region") %>% 
  select(-subregion)

# get the five inflation types and add the average option
inflation_types = append(
  global_map_data %>% 
    pull(type) %>% 
    unique(),
  "Average Inflation Across All Types"
)

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
    scale_fill_gradientn(colors=rev(topo.colors(8)),
                         na.value="gray55") +
    labs(title = "Chloropleth Map of Given Inflation Type",
         x = NULL,
         y = NULL,
         fill = "Inflation Rate (capped at 25% for better distribution)") +
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

# create the UI
ui = fluidPage(
  titlePanel("Analyzing Various Types of Inflation Rates per Country"),
  h4("The inflation rates per country were measured at the **beginning** of the year. 
     This is apparent because of the lower inflation rates seen in 2020 compared to 2021, 
     where the result of COVID can first be seen in 2021."),
  h4("If a country is colored gray, then it has no data for that year."),
  h4("If a country is not filled in (so it looks like a \"missing\" country), then it does not keep track of that inflation type.\n"),
  htmlOutput("empty_line"), # add empty line for spacing
  h4("Choose from the inputs below to compare inflation rates"),
  # make a sidebar for the inputs
  sidebarLayout(
    sidebarPanel(
      # first input, which year
      selectInput("i_type", 
                  "Choose the inflation type you want on the map",
                  choices=inflation_types),
      
      # second input, which type of inflation
      sliderInput("year", "Select the year", 
                  min=min(global_inflation$year),
                  max=max(global_inflation$year), 
                  value=2000,
                  sep="")
    ),
    
    # display the outputs
    mainPanel(
      girafeOutput("map")
    )
  )
)

# create the server
server = function(input, output) {
  subset_world_data = reactive({
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
  
  output$empty_line = renderUI({
    HTML("<br>")
  })
  
  output$map = renderGirafe({
    girafe(code = print(map(subset_world_data())))
  })
}

shinyApp(ui, server)