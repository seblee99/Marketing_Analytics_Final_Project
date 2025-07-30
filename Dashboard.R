library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(ggplot2)

# Load Data
# Assumes that the format of the data columns will remain the same with different datasets

## TODO
# Improve Graphics
# https://rstudio.github.io/shinydashboard/structure.html#:~:text=Boxes%20are%20the%20main%20building,most)%20any%20Shiny%20UI%20content.&text=Boxes%20can%20have%20titles%20and,different%20possible%20statuses%20are%20shown

# UI Stuff
ui <- dashboardPage(
  skin = "purple",
  
  # Dashboard Header Section
  dashboardHeader(title = tags$a(href='https://www.dirtea.com', tags$img(src='https://www.dirtea.com/images/Logo_Dirtea_negativ.png', height='100%', width='100%'))),
  
  # Dashboard Sidebar Section
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("KPIs", tabName = "kpis", icon = icon("chart-simple")),
      menuItem("Mindset Metrics", tabName = "mindset", icon = icon("brain")),
      menuItem("Sales and Ads Analysis", tabName = "salesandads", icon = icon("rectangle-ad")),
      menuItem("Funnel Analysis", tabName = "funnel", icon = icon("filter")),
      menuItem("Simulations", tabName = "simulations", icon = icon("chart-line")),
      menuItem("Settings", tabName = "settings", icon = icon("gear")),
      menuItem("Help", tabName = "help", icon = icon("circle-question")),
      menuItem("Test", tabName = "test", icon = icon("microchip"))
      )
    ),
  
  # Dashboard Body Section
  dashboardBody(
    # Background colour for Dashboard Body Section
    tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: lavender;
      }
    '))),
    
    tabItems(
      
      ## Home page
      tabItem("home",
              fluidPage(
                img(src = 'https://www.dirtea.com/images/7792_Schriftzug_Sortenkreis_210712-1.png', height = '100%', width = '100%', align = 'center'),
                h1('Yet to be Titled (I need a title guys and gals)', align = 'center'),
                h2('Marketing Analytics Final Project', align = 'center'),
                h2('Group 4 : Mindset Metrics', align = 'center'),
                h4('by Antoine BOSSAN, Serafina CHEN, Nishant DAVE, Duoer GU, Benjamin JULLIET, Sebastian LEE, and Nhu PHAM', align = 'center')
                # Titles: DirTea Sales Analytics,
                # Titles: DirTea Sales AnalYSIS,
              )
              
      ),
      
      ## Data page
      tabItem("data",
              fluidPage(
                
                # File input for RData file
                fileInput("file", "Choose RData file for analysis", accept = c(".RData")),
                
                # Display uploaded file information
                verbatimTextOutput("fileInfo"),
                
                # Output for displaying loaded data
                dataTableOutput("dataTable")
                
              )
              
      ),
      
      ## Dashboard page
      tabItem("dashboard",
               fluidPage(
                 h1("Dashboard"),
               )
               
        
      ),
      
      ## KPIs page
      tabItem("kpis",
              fluidPage(
                titlePanel("Key Performance Indicators"),
                sidebarLayout(
                  sidebarPanel(
                    
                  ),
                  mainPanel(
                    verbatimTextOutput("kpiOutput")
                  )
                )
              )
               
                       
      ),
      
      ## Mindset Metrics page
      tabItem("mindset",
              fluidPage(
                titlePanel("Mindset Metrics"),
                sidebarLayout(
                  sidebarPanel(
                    
                  ),
                  mainPanel(
                    plotOutput("mindsetPlot")
                  )
                )
              )
               
      ),
      
      ## Sales and Advertisement analysis page
      tabItem("salesandads",
               fluidPage(
                 titlePanel("Sales and Ads Analysis"),
                 sidebarLayout(
                   sidebarPanel(
                     
                     selectInput("adChannels", "Select Ad Channel to Correlate:", c("InstagramAds", "TikTokAds", "SEA", "PoSPromotions", "InfluencerColabs")),
                     
                     sliderInput("range", label = "Range of interest:", min = 1, max = 1100, value = c(1, 1100)),
                   
                     # From Nhu
                     dateRangeInput("dateRange", "Select Date Range:"),
                     checkboxGroupInput("adChannels", "Select Ad Channels:", 
                                        choices = c("InstagramAds", "TikTokAds", "SEA", "PoSPromotions", "InfluencerColabs")),
                     actionButton("update", "Update"),
                     radioButtons("plotType", "Choose Plot Type:",
                                  choices = c("Bar Chart", "Line Graph"))
                   
                   ),
                     
                   mainPanel(
                     
                     # Correlation plot
                     plotOutput("correlation_plot"),
                     
                     
                     # Sales Trend (Nhu)
                     
                     plotOutput("sales_plot"),
                     
                     # Ad Performance Plot (Nhu)
                     
                     plotOutput("adPerformance_plot")
                    )
                 
                 
                 )
                 
                 
               )
               
      ),
      
      ## Funnel page
      tabItem("funnel",
              fluidPage(
                titlePanel("Funnel Analysis"),
                sidebarLayout(
                  sidebarPanel(
                    
                  ),
                  mainPanel(
                    plotOutput("funnelPlot")
                  )
                )
              )
               
      ),
      
      ## Simulations page
      tabItem("simulations",
               fluidPage(
                 h1("Simulations")
               )
               
      ),
      
      ## Settings page
      tabItem("settings",
               fluidPage(
                 
                 titlePanel('Settings'),
                 
               )
               
      ),
      
      
      ## Help page
      tabItem("help",
              fluidPage(
                titlePanel("Help"),
                
                h1("Help")
              )
              
      ),
      
      # Test/Undefined page
      tabItem("undefined",
              fluidPage(
                titlePanel("Undefined"),
                sidebarLayout(
                  sidebarPanel(
                  ),
                  mainPanel(
                    
                  )
                )
              )
      )
      
      # tabItem("test",
      #         fluidPage(
      #           titlePanel("My Star Wars App"),
      #           sidebarLayout(
      #             sidebarPanel(),
      #             mainPanel(
      #               h6("Episode IV", align = "center"),
      #               h6("A NEW HOPE", align = "center"),
      #               h5("It is a period of civil war.", align = "center"),
      #               h4("Rebel spaceships, striking", align = "center"),
      #               h3("from a hidden base, have won", align = "center"),
      #               h2("their first victory against the", align = "center"),
      #               h1("evil Galactic Empire.")
      #             )
      #           )
      # 
      #         )
      # )
      
      
    )
  )
)


# Server Stuff
# server <- function(input, output){
#   output$correlation_plot <- renderPlot({
#     plot(MindSetDF[[input$adChannels]], MindSetDF$Sales, xlab = "", ylab = "Actual Sales")
#   })
#   
#   output$mindsettable <- renderDataTable(MindSetDF)
#   
#   
#   ## Test
#   observe({
#     file <- input$file
#     if (is.null(file)) {
#       return(NULL)
#     }
#     
#     # Check if the selected file has a .Rdata extension
#     if (grepl("\\.Rdata$", file$name)) {
#       load(file$datapath)
#       output$fileInfo <- renderText({
#         paste("File", file$name, "uploaded and loaded into the environment.")
#       })
#     } else {
#       output$fileInfo <- renderText({
#         "Please select a valid .Rdata file."
#       })
#     }
#   })
#   
# }

server <- function(input, output) {
  
  # Reactive value for storing the loaded data
  loadedData <- reactiveVal(NULL)
  
  # Display file information
  output$fileInfo <- renderPrint({
    file <- input$file
    if (is.null(file))
      return(NULL)
    file
  })
  
  ## Data Page
  # Load and preview data
  observe({
    req(input$file)
    
    # Load the RData file
    load(input$file$datapath)
    
    # Assuming your dataframe in RData is named MindSetDF
    MindSetDF <- tibble::rowid_to_column(MindSetDF, "Day")
    loadedData(MindSetDF)
  })
  
  # Data Table
  output$dataTable <- renderDataTable(loadedData())
  
  
  ## KPIs page
  # KPIs
  output$kpiOutput <- renderPrint({
    filtered <- loadedData()
    total_sales <- sum(filtered$Sales)
    avg_instagram_efficiency <- sum(filtered$InstagramAds) / total_sales
    avg_tiktok_efficiency <- sum(filtered$TikTokAds) / total_sales
    avg_SEA_efficiency <- sum(filtered$SEA) / total_sales
    conversion_rate <- total_sales / mean(filtered$Awareness)
  
  cat("Total Sales:", total_sales, "\n",
      "Average Instagram Efficiency:", avg_instagram_efficiency, "\n",
      "Average TikTok Efficiency:", avg_tiktok_efficiency, "\n",
      "Average SEA Efficiency:", avg_SEA_efficiency, "\n",
      "Conversion Rate from Awareness to Sales:", conversion_rate)
  # Code to calculate and display KPIs
  })
  
  
  ## Mindset page
  # Mindset Metrics Plot
  output$mindsetPlot <- renderPlot({ 
    df <- loadedData()
    ggplot(df, aes(x = Index)) +
      geom_line(aes(y = Awareness, color = "Awareness")) +
      geom_line(aes(y = Liking, color = "Liking")) +
      geom_line(aes(y = Consideration, color = "Consideration")) +
      labs(title = "Mindset Metrics Over Time", x = "Index", y = "Value") +
      scale_color_manual(values = c(Awareness = "blue", Liking = "green", Consideration = "red")) +
      theme_minimal()
  })
  
  
  ## Sales and Advertisement analysis page
  
  # Correlation plot
  output$correlation_plot <- renderPlot({
    df <- loadedData()
    plot(df[[input$adChannels]], df$Sales, xlab = "", ylab = "Actual Sales")
    })

  # Sales trend (Nhu)
  output$sales_plot <- renderPlot({
    df <- loadedData()
    # Create the plot using the Index as the x-axis
    ggplot(data = loadedData(), aes(x = Day, y = Sales)) +
      geom_line() +
      labs(title = "Sales Trends",
           x = "Days",
           y = "Sales") +
      xlim(c(input$range[1:2])) +
      theme_minimal()
  })
  
  # Ad Channels Performance Plot: to compare the performance of different ad channels in terms of sales and engagement
output$adPerformance_plot <- renderPlot({
  # Get the filtered data
  df <- loadedData()
  
  # Aggregate data by ad channels
  ad_data <- df %>%
    summarise(across(starts_with(c("InstagramAds", "TikTokAds", "SEA", "PoSPromotions", "InfluencerColabs")), sum))
  
  # Convert to long format for ggplot2
  ad_data_long <- pivot_longer(ad_data, cols = everything(), names_to = "AdChannel", values_to = "Sales")
  
  # Determine the plot type based on input$plotType
  ad_data_long$type <- ifelse(input$plotType == "Bar Chart", "bar", "line")
  
  ggplot(ad_data_long, aes(x = AdChannel, y = Sales, fill = AdChannel, color = AdChannel, group = AdChannel)) +
    geom_col(data = ad_data_long[ad_data_long$type == "bar", ], position = position_dodge()) +
    geom_line(data = ad_data_long[ad_data_long$type == "line", ], show.legend = FALSE) +
    geom_point(data = ad_data_long[ad_data_long$type == "line", ], show.legend = FALSE) +
    labs(title = "Ad Channels Performance",
         x = "Ad Channel",
         y = "Sales") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels to 45 degrees
  })

## Funnel page
# Funnel Analysis Plot
output$funnelPlot <- renderPlot({
  filtered <- loadedData()
  funnel_data <- data.frame(
    Stage = c("Awareness", "Consideration", "Liking", "Sales"),
    Value = c(mean(filtered$Awareness), mean(filtered$Consideration), mean(filtered$Liking), mean(filtered$Sales))
  )
  ggplot(funnel_data, aes(x = Stage, y = Value)) +
    geom_bar(stat = "identity", aes(fill = Stage)) +
    coord_flip() +
    labs(title = "Funnel Analysis", x = "", y = "Average Value") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
  # Plot code for Funnel Analysis
})

## Undefined

  
}




shinyApp(ui, server)

