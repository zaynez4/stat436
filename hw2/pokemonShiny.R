library(tidyverse)
library(shiny)

pokemon = read_csv("https://uwmadison.box.com/shared/static/hf5cmx3ew3ch0v6t0c2x56838er1lt2c.csv")

# get all the unique generations for the multi-select input
gens = pull(pokemon, Generation) %>% 
  unique() %>% 
  na.omit()

# create the histogram of HP distribution
histogram = function(pokemon) {
  ggplot(pokemon, aes(HP, fill = as.factor(Generation))) +
    geom_histogram(position = "stack", color = "black", bins=30) +
    scale_fill_brewer(palette = "Set1") +
    labs(title = "HP of Pokemon", 
         x = "HP (Health Points)", 
         y = "Frequency") +
    guides(fill = "none")
}

# create the scatter plot to show the Attack/Defense relationship
scatterplot = function(pokemon) {
  ggplot(pokemon, aes(Defense, Attack, color=as.factor(Generation))) +
    geom_point(alpha = 0.7) +
    scale_color_brewer(palette = "Set1") +
    labs(title = "Attack and Defense of a Pokemon", 
         x = "Defense", 
         y = "Attack",
         color = "Generation") +
    theme(legend.position = "left") +
    guides(color = guide_legend(override.aes = list(size = 5)))
}

# create the UI layout that has a title, 4 inputs, then the outputs
ui = fluidPage(
  titlePanel("Pokemon HP to Attack/Defense Analysis Across Generations"),
  h3("Select various values from the four inputs below to find your perfect Pokemon!"),
  selectInput("gens", "Select Pokemon Generation(s)", gens, multiple=TRUE),
  sliderInput("hp", "Choose HP Range", 
              min=min(pokemon$HP), max=max(pokemon$HP),
              c(1,255), sep=""),
  sliderInput("attack", "Choose Attack Range", 
              min=min(pokemon$Attack), max=max(pokemon$Attack),
              c(5,190), sep=""),
  sliderInput("defense", "Choose Defense Range", 
              min=min(pokemon$Defense), max=max(pokemon$Defense),
              c(5,230), sep=""),
  fluidRow(
    column(6, plotOutput("histogram")),
    column(6, plotOutput("scatterplot"))
  ),
  h3("Here are all the Pokemon based on your inputs, sorted from highest to lowest HP!"),
  dataTableOutput("table")
)

# create the server to produce the outputs and renders
server = function(input, output) {
  # any time an input is changed, get this subset that matches
  # the filter
  pokemon_subset = reactive({
    pokemon %>% 
      filter(Generation %in% input$gens &
               HP >= input$hp[1] & 
               HP <= input$hp[2] & 
               Attack >= input$attack[1] & 
               Attack <= input$attack[2] & 
               Defense >= input$defense[1] &
               Defense <= input$defense[2])
  })
  
  output$histogram = renderPlot({histogram(pokemon_subset())})
  
  output$scatterplot = renderPlot({scatterplot(pokemon_subset())})
  
  # select only the important information and arrange from high-low HP
  output$table = renderDataTable({
    pokemon_subset() %>% 
      select(Name, type_1, type_2, HP, Attack, Defense, Generation) %>% 
      arrange(desc(HP))
  })
}

shinyApp(ui, server)