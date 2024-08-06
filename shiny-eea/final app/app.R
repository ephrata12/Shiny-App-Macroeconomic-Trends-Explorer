library(shiny)
library(hrbrthemes)
library(tidyverse)
library(ggrepel)
library(bslib)
library(plotly)
library(sf)
library(ggplot2)
library(viridis)
library(dplyr)
library(readr)
library(leaflet)
library(DT)

dev_indicators <- read_csv("data/dev_indicators_clean.csv")
growth_rate_leaflet <- readRDS("data/growth_rate_leaflet.rds")
inflation_rate_clean <- read.csv("data/inflation_rate_clean.csv")

#mutating gdp to better interpretability in scatterplot
dev_indicators <- dev_indicators %>% 
  mutate(LogGDP = log(gdp)) 

#Removing original gdp values
dev_indicators <- dev_indicators %>% 
  dplyr::select(!gdp)

#For radio buttons
dev_choice_values <- c("Labor", "Education", "Depletion")
dev_choice_names <- c("Labor, total", "Educational Attainment, Upper Secondary Completion (%)", "Depletion of Natural Resources (%)")
names(dev_choice_values) <- dev_choice_names

#For selectizeInput 
ctry_choices <- unique(dev_indicators$Country)

# Defining color pallete for the map across the entire dataset
my_palette <- colorNumeric(palette = "Spectral"
                           , domain = growth_rate_leaflet$growth_rate)

# Defining html layout of the leaflet map legend
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))

# Define UI for application that draws a scatterplot
ui <- fluidPage(
  
  # Rearranging outline of legend HTML to keep all of the boxes in one column
  # Source code: https://github.com/rstudio/leaflet/issues/615
  HTML(html_fix),
  
  # Application title
  title = "EEA: Shiny App",
  titlePanel("Macroeconomics Trends Exploration"),
  # Database citation
  p(tags$div(
      HTML(paste("Data is sourced by "
                 , tags$a(href="https://data.worldbank.org", "World Bank Open Data")
                 , sep = ""))
    )),
  
  # GDP Growth Rate Map page layout
  tabsetPanel(
    tabPanel("GDP Growth Rate Map", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "year", 
                             label = "Select a year", 
                             min=2000, 
                             max=2022, 
                             value=2021, 
                             sep = "")
               ),
               mainPanel(
                 h3(strong("Interactive GDP Growth Rate Map: World")),
                 leafletOutput("mymap"),
                 em("*click on a specific country to learn more about their GDP Growth Rate.")
               )
             )
  ),
  # GDP Indicators page layout
    tabPanel("GDP Indicators", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "VarX"
                              , label = "Select an indicator"
                              , choices = dev_choice_values
                              , selected = "Education"),
                 selectizeInput(inputId = "ctry"
                                , label = "Select a country"
                                , choices = ctry_choices
                                , selected = c("Netherlands", "Japan")
                                , multiple = TRUE),
                 #To display correlation
                 value_box(
                   title = NULL,
                   value = tableOutput("cor"),
                   #value box cited from https://rstudio.github.io/bslib/articles/value-boxes/index.html
                 )
               ),
               mainPanel(
                 plotOutput(outputId = "scatter")
               )
             )
  ),

  # Inflation Trends page layout
    tabPanel("Inflation Trends by Country", fluid = TRUE,
             a(href = "https://databank.worldbank.org/source/world-development-indicators", "Data source "),
             sidebarLayout(
               sidebarPanel(
      # Create a select input for choosing multiple countries
      selectInput("multi_country", "Select Single/Multiple Countries:", multiple = TRUE, 
                  choices = unique(inflation_rate_clean$country),
                  selected = unique(inflation_rate_clean$country)[1])
    ),
    mainPanel(
      # Create tabs for different sections
      tabsetPanel(
        tabPanel("Inflation Chart", plotlyOutput("inflation_plot")),
        tabPanel("Inflation Rate Table", DTOutput("inflation_table")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary")))
    )
             )
    ),
  )
)

# Define server logic 
server <- function(input,output){
  
  # Interactive scatterplot
  output$scatter <- renderPlot({
    if(input$VarX == "Labor") {
      dev_indicators %>%
        filter(Country%in%input$ctry) %>% 
        ggplot(aes_string(x=input$VarX, y="LogGDP", color="Country")) +
        geom_point() +
        labs(x = dev_choice_names[dev_choice_values == input$VarX], y = "GDP ($)"
             , title = "GDP Indicators", face = "bold", size = 14, subtitle = "Potential predictors of GDP from 2000 to 2022"
             , size = dev_choice_names[dev_choice_values == input$VarX]) +
        geom_smooth(method="lm", se=FALSE) +
        facet_wrap(~Country, scales="free") +
        #To transform and scale 'Labor' by Millions
        scale_x_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))  +
        theme_minimal() +
        #To bold title, and and axes
        #code cited from https://stackoverflow.com/questions/52884194/trying-to-bold-y-axis-in-ggplot
        theme(plot.title = element_text(size=20, face = "bold")
              ,plot.subtitle = element_text(size=20)
              ,axis.title.x = element_text(size=15, face = "bold")
              ,axis.title.y = element_text(size=15, face = "bold")
              ,strip.text.x = element_text(size=12, face="bold")
              ,axis.text.x = element_text(size=10, face="bold")
              ,axis.text.y = element_text(size=10, face="bold")
              ,legend.title = element_text(size=12, face="bold"))
    }
    else {
      dev_indicators %>%
        filter(Country%in%input$ctry) %>% 
        ggplot(aes_string(x=input$VarX, y="LogGDP", color="Country")) +
        geom_point() +
        labs(x = dev_choice_names[dev_choice_values == input$VarX], y = "GDP ($)"
             , title = "GDP Indicators", face = "bold", subtitle = "Potential predictors of GDP from 2000 to 2022"
             , size = dev_choice_names[dev_choice_values == input$VarX]) +
        geom_smooth(method="lm", se=FALSE) +
        facet_wrap(~Country, scales="free") +
        theme_minimal() +
        theme(plot.title = element_text(size=23, face = "bold")
              ,plot.subtitle = element_text(size=20)
              ,axis.title.x = element_text(size=15, face = "bold")
              ,axis.title.y = element_text(size=15,face = "bold")
              ,strip.text.x = element_text(size=12, face="bold")
              ,axis.text.x = element_text(size=10, face="bold")
              ,axis.text.y = element_text(size=10, face="bold")
              ,legend.title = element_text(size=12, face="bold"))
    }
    
    
  })
  # Correlation and relationship for value box
  output$cor <- renderTable({
    dev_indicators0 <- dev_indicators |>
      filter(Country%in%input$ctry) |>
      group_by(Country) |>
      select(LogGDP, input$VarX) |>
      summarize(Correlation = cor(LogGDP, !!sym(input$VarX), use="pairwise")) |>
      mutate(Relationship = case_when(
        Correlation < -0.6 ~ "Strong",
        Correlation < -0.4 ~ "Moderate",
        Correlation < -0.0 ~ "Weak",
        Correlation < 0.4 ~ "Weak",
        Correlation < 0.6 ~"Moderate",
        Correlation <= 1 ~"Strong",
        TRUE ~ "NA"
      ))
    return(dev_indicators0)
  })
  
  # Filtering dataset for data pertaining to a specific year indicated by toggle slider
  lf <- reactive({
    filter(growth_rate_leaflet, year == input$year)
  })
  
  # Creating a map corresponding to the data of a specifically indicated year (via toggle slider)
  output$mymap <- renderLeaflet({
    # Setting dataset to one generated by the reactive element
    lf() %>%
      leaflet() %>%               
      addTiles() %>%
      addPolygons(
        fillColor = ~my_palette(lf()$growth_rate),
        fillOpacity = 0.7,
        stroke = FALSE,
        # Defining the content of popups that will be displayed when the user clicks on a country
        popup = ~ glue::glue("Country: {str_to_title(country_name)} <br> Year: {year} <br>
                         GDP Growth Rate: 
                            {paste0(round(growth_rate),'%')}")
      ) %>%
      # Fixing the position of the world map to show a close up display by default
      # Source Code: https://stackoverflow.com/questions/48443133/what-is-the-max-bounds-of-the-entire-world-leaflet
      setView( lng = -90
               , lat = 180
               , zoom = 1 ) %>%
      setMaxBounds( lng1 = -90
                    , lat1 = -180
                    , lng2 = 90
                    , lat2 = 180 ) %>%
      # Adding a color legend to the map
      # Source Code: http://rstudio.github.io/leaflet/legends.html
      addLegend(position = "bottomright", pal = my_palette, values = ~growth_rate,
                title = " Annual GDP Growth",
                labFormat = labelFormat(suffix = "%", transform = function(growth_rate) sort(growth_rate, decreasing = TRUE)), 
                opacity = 1,
                na.label = glue::glue("No data available <br> for the year {lf()$year[1]}")
      )
  })
  
  
  # Define a reactive expression to update selected country based on user input
  selected_data <- reactive({
    inflation_rate_clean %>% filter(country %in% input$multi_country)
  })
  
  # Render a Plotly plot based on selected countries
  output$inflation_plot <- renderPlotly({
    plot_data <- selected_data()
    ordered_years <- as.character(2000:2022)
    plot_data %>%
      plot_ly(
        x = ~year,
        y = ~inflationRate,
        color = ~country,
        type = "scatter",
        mode = "lines+markers"
      ) %>%
      layout(
        title = "Inflation Trends",
        xaxis = list(
          title = "Year",
          type = "category",
          tickangle = -45,
          categoryorder = "array",
          categoryarray = ordered_years,
          dtick = 1  
        ),
        yaxis = list(title = "Inflation Rate(%)")
      )
  })
  
  # Render table (DT) based on selected countries
  output$inflation_table <- renderDT({
    table_data <- selected_data()
    datatable(table_data, rownames = FALSE)
  })
  
  ## learned about lapply function from:
  # <<https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/lapply>>
  # <<https://r-coder.com/lapply-r/>>
  #<<https://youtu.be/GgdR2OV8r_M?si=xRwE3rquXkzU8GEd>>
  
  # Calculate and display summary statistics for selected countries 
  summaries <- reactive({
    data <- selected_data()
    unique_countries <- unique(data$country)
    country_summaries <- lapply(unique_countries, function(country) {
      country_data <- data %>% filter(country == country)
      summary_stats <- summary(country_data$inflationRate)
      summary_text <- paste("Summary Statistics of inflation rate for", country, "\n")
      summary_text <- paste(summary_text, "Mean:", round(mean(country_data$inflationRate), 3))
      summary_text <- paste(summary_text, "Median:", round(median(country_data$inflationRate), 3))
      summary_text <- paste(summary_text, "Minimum:", round(min(country_data$inflationRate), 3))
      summary_text <- paste(summary_text, "Maximum:", round(max(country_data$inflationRate), 3))
      summary_text
    })
    unlist(country_summaries, use.names = FALSE)
  })
  
  # Render and display the summary statistics as text
  output$summary <- renderPrint({
    summaries_data <- summaries()
    cat(summaries_data, sep = "\n\n")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
