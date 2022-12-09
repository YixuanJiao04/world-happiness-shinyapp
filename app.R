library(shiny)
library(plotly)
library(tidyverse)
library(shinythemes)
library(shinycssloaders)
library(DT)
 

#preparing dataset
iso_code <- 
  read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv") %>%
  janitor::clean_names() %>%
  rename(country_name = country) %>%
  select(-gdp_billions) %>%
  add_row(country_name = "Palestinian Territories", code = "PSE")
happy <- 
  read_csv('happiness.csv') %>%
  filter(year != 2005)
happy_name <- unique(happy$country_name)
name <- unique(iso_code$country_name)
diff_name <- setdiff(happy_name,name)
code_name <- c("Congo, Republic of the","Gambia, The", "Hong Kong", "Cote d'Ivoire" ,"Burma", "Cyprus", "Macedonia", "Korea, South", "Taiwan")
for (i in 1:length(code_name)){
  iso_code$country_name[iso_code$country_name == code_name[i]] <- diff_name[i]
}

happy <- left_join(happy,iso_code)

ui <- fluidPage(
  navbarPage(title = "World Happiness Data Explore", theme = shinytheme("lumen"),
             tabPanel("Country Comparison", fluid = TRUE,
                      fluidRow(
                        column(4,
                               selectizeInput(inputId = "countries_comparison_list",
                                              label = "Choose countries (Maximum 5)",
                                              choices = unique(happy$country_name),
                                              multiple = TRUE,
                                              options = list(maxItems = 5, placeholder = 'Choose countries')),
                               helpText("Select at most 4 countries to generate plot")
                        ),
                        column(4,
                               selectInput(inputId = "countries_comparison_variables",
                                           label = "Select factor you interested in",
                                           choices = colnames(happy)[c(-1:-3,-11)],
                                           selected = "ladder_score"))
                        ),
                      fluidRow(
                        column(4,
                               withSpinner(plotOutput(outputId = "countries_comparison_boxplot"))
                        ),
                        column(6,
                              DTOutput(outputId = "countries_comparison_table"))
                      )))
)

server <- function(input,output) {
  countries_happy_filtered <- reactive({
    req(input$countries_comparison_list)
    req(input$countries_comparison_variables)
    happy %>% 
      filter(country_name %in% input$countries_comparison_list)
  })
  output$countries_comparison_boxplot <- renderPlot({
    countries_happy_filtered() %>%
      ggplot(aes_string(y = input$countries_comparison_variables, x = 'country_name', color = 'country_name')) +
      geom_boxplot()
  })
  output$countries_comparison_table <- DT::renderDT(
    countries_happy_filtered() %>%
      arrange(-year) %>%
      select(-code),
    rownames = FALSE
  )
}

shinyApp(ui = ui, server = server)