library(shiny)
library(plotly)
library(tidyverse)
library(viridis)
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

model <- lm(data = happy, formula = ladder_score ~ regional_indicator+ logged_gdp_per_capita + social_support + freedom_to_make_life_choices + logged_gdp_per_capita*freedom_to_make_life_choices)
#Design UI
ui <- fluidPage(
  #navbar includes 4 tab panel in total
  navbarPage(title = "World Happiness Data Explore", theme = shinytheme("cosmo"),
             #lay out inside the tabpanel
             tabPanel("Model", fluid = TRUE,
                      titlePanel("3D Demonstration of Our Final Model"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "new_region",
                                      label = "Choose a region",
                                      choices = unique(happy$regional_indicator),
                                      selected = "North America and ANZ"),
                          sliderInput(inputId = "new_gdp",
                                      label = "Input GDP per Capita",
                                      min = 500,
                                      max = 120000,
                                      value = 100000),
                          sliderInput(inputId = "new_social_support",
                                      label = "Input Social support index",
                                      min = 0.1,
                                      max = 1,
                                      value = 0.8,
                                      step = 0.01),
                          sliderInput(inputId = "new_freedom",
                                      label = "Input freedom to make life choices index",
                                      min = 0.1,
                                      max = 1,
                                      value = 0.8,
                                      step = 0.01),
                          helpText("Try different values to see how would fitted ladder score change")
                        ),
                        mainPanel(
                          plotlyOutput(outputId = "model_plotly"),
                          textOutput(outputId = "model_fit"),
                          helpText("Rotate the graph to see the spacial distribution (Size of the markers represents gdp per capital, color represents region,size of fitted point is fixed inorder to get better visual effect)")
                        )
                      )),
             tabPanel("Country Comparison", fluid = TRUE,
                      fluidRow(
                        titlePanel("Country Comparison"),
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
                      )),
             tabPanel("Region Comparison", fluid = TRUE,
                      titlePanel("Region Comparison"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "region_comparison_year",
                                      label = "Select Year",
                                      choices = c(2006:2021),
                                      selected = 2021),
                          checkboxGroupInput(inputId = "region_comparison_continent",
                                         label = "Select Regions",
                                         choices = unique(happy$regional_indicator),
                                         selected = c("North America and ANZ","Sub-Saharan Africa")),
                          selectInput(inputId = "region_comparison_variables",
                                      label = "Select factor you interested in",
                                      choices = colnames(happy)[c(-1:-3,-11)],
                                      selected = "ladder_score")),
                        mainPanel(
                          plotlyOutput(outputId = "region_plotly"),
                          DTOutput(outputId = "region_dt")
                        )
                        )
                      ),
             tabPanel("Association Check", fluid = TRUE,
                      titlePanel("Association Check"),
                      sidebarLayout(
                        sidebarPanel(selectInput(inputId = "var1",
                                                 label = "Select first variable",
                                                 choices = colnames(happy)[c(-1:-3,-11)],
                                                 selected = "logged_gdp_per_capita"),
                                      selectInput(inputId = "var2",
                                                 label = "Select second variable",
                                                 choices = colnames(happy)[c(-1:-3,-11)],
                                                 selected = "ladder_score")),
                        mainPanel(
                          plotOutput(outputId = "asscociation_plot"),
                          textOutput(outputId = "corr_text")
                        ))
                      ))
             )


server <- function(input,output) {
  #all rective objects required
  countries_happy_filtered <- reactive({
    req(input$countries_comparison_list)
    req(input$countries_comparison_variables)
    happy %>% 
      filter(country_name %in% input$countries_comparison_list)
  })
  
  region_plotly_filtered <- reactive({
    req(input$region_comparison_year)
    req(input$region_comparison_continent)
    happy %>%
      filter(regional_indicator %in% input$region_comparison_continent) %>%
      filter(year %in% as.numeric(input$region_comparison_year))
  })
  
  #Rendering output
  output$model_plotly <- renderPlotly({
    req(input$new_region)
    req(input$new_gdp)
    req(input$new_social_support)
    req(input$new_freedom)
    logged_gdp <- log(input$new_gdp)
    new_point <- data.frame(regional_indicator = c(input$new_region),
                            logged_gdp_per_capita = c(logged_gdp),
                            social_support = c(input$new_social_support),
                            freedom_to_make_life_choices = c(input$new_freedom))
    fitted_ladder <- predict(model, newdata = new_point) %>% as.numeric
    happy %>% 
      filter(year == 2021) %>%
      mutate(gdp_per_capita = exp(logged_gdp_per_capita)) %>% 
      mutate(regional_indicator = fct_reorder(regional_indicator,ladder_score)) %>%
      plot_ly(x = ~social_support, 
              y = ~freedom_to_make_life_choices, 
              z = ~ladder_score, 
              type="scatter3d", 
              mode="markers",
              color = ~regional_indicator,
              marker = list(symbol = 'circle', sizemode = 'diameter'),
              size = ~gdp_per_capita,
              sizes = c(5,100),
              opacity = 0.4,
              showlegend = FALSE) %>%
      add_trace(x = input$new_social_support,
                y = input$new_freedom,
                z = fitted_ladder,
                marker = list(size = 10, color = 'blue'))
  })
  output$model_fit <- renderText({
    req(input$new_region)
    req(input$new_gdp)
    req(input$new_social_support)
    req(input$new_freedom)
    logged_gdp <- log(input$new_gdp)
    new_point <- data.frame(regional_indicator = c(input$new_region),
                            logged_gdp_per_capita = c(logged_gdp),
                            social_support = c(input$new_social_support),
                            freedom_to_make_life_choices = c(input$new_freedom))
    fitted_ladder <- predict(model, newdata = new_point) %>% as.numeric %>% round(2) %>% as.character
    paste("The fitted ladder score is", fitted_ladder)
  })
  output$countries_comparison_boxplot <- renderPlot({
    countries_happy_filtered() %>%
      ggplot(aes_string(y = input$countries_comparison_variables, x = 'country_name', color = 'country_name')) +
      geom_boxplot()
  })
  
  output$countries_comparison_table <- renderDT(
    countries_happy_filtered() %>%
      arrange(-year) %>%
      select(-code),
    rownames = FALSE
  )
  
  output$region_plotly <- renderPlotly({
    req(input$region_comparison_variables)
    df <- region_plotly_filtered()
    df$zz <- df[[input$region_comparison_variables]]
    plot_ly(df,
            type = 'choropleth', 
            locations = df$code, 
            z = ~zz, 
            text = df$country_name,
            colorscale = "Viridis")
  })
  
  output$region_dt <- renderDT(
    region_plotly_filtered()[c('country_name','regional_indicator',input$region_comparison_variables)]
  )
  
  output$asscociation_plot <- renderPlot({
    happy %>%
      mutate(regional_indicator = fct_reorder(regional_indicator,ladder_score)) %>%
      ggplot(aes_string(x = input$var1, y = input$var2)) + 
      geom_point(aes( color = regional_indicator)) +
      scale_color_viridis(discrete = TRUE, option = "D")
  })
  
  output$corr_text <- renderText({
    v1 <- happy[[input$var1]]
    v2 <- happy[[input$var2]]
    corr_val <- as.character(round(cor(v1,v2),2))
    paste("The correlation bewteen",input$var1,"and",input$var2,"is:",corr_val)
  })
}

shinyApp(ui = ui, server = server)