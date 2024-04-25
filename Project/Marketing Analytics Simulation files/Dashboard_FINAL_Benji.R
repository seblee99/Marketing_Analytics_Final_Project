library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(ggplot2)
library(forecast)
library(nnet)

# KPI functions

# Function to calculate the liking to consideration ratio:
Awareness2Consideration <- function(data, start_row, end_row) {
  subset_data <- data[start_row:end_row, , drop = FALSE]
  avg_sales <- mean(subset_data$Consideration / subset_data$Awareness, na.rm = TRUE)
  return(avg_sales)
}


# Function to calculate the liking to consideration ratio:
Consideration2Liking <- function(data, start_row, end_row) {
  subset_data <- data[start_row:end_row, , drop = FALSE]
  avg_sales <- mean(subset_data$Liking / subset_data$Consideration, na.rm = TRUE)
  return(avg_sales)
}

# Function to calculate the average daily sales:
computeAverageSales <- function(data, start_row, end_row) {
  subset_data <- data[start_row:end_row, , drop = FALSE]
  avg_sales <- mean(subset_data$Sales, na.rm = TRUE)
  return(avg_sales)
}

# Function to calculate average awareness for a given row index range
computeAverageAwareness <- function(data, start_row, end_row) {
  subset_data <- data[start_row:end_row, , drop = FALSE]
  average_awareness <- mean(subset_data$Awareness, na.rm = TRUE)
  return(average_awareness)
}

# Function to calculate average liking for a given row index range
computeAverageLiking <- function(data, start_row, end_row) {
  subset_data <- data[start_row:end_row, , drop = FALSE]
  average_liking <- mean(subset_data$Liking, na.rm = TRUE)
  return(average_liking)
}

# Function to calculate average liking for a given row index range
computeAverageConsideration <- function(data, start_row, end_row) {
  subset_data <- data[start_row:end_row, , drop = FALSE]
  average_consideration <- mean(subset_data$Consideration, na.rm = TRUE)
  return(average_consideration)
}


# Function to compute ROAS as a percentage
computeROAS <- function(data, sales_col, spend_col, start_row, end_row) {
  
  # Calculate total advertising spendings by summing specific columns
  data$TotalAdvertisingSpend <- rowSums(data[, c('InstagramAds', 'TikTokAds', 'SEA', 'PoSPromotions', 'InfluencerColabs')], na.rm = TRUE)
  
  
  # Ensure the specified columns exist in the dataframe
  if (!(sales_col %in% colnames(data) && spend_col %in% colnames(data))) {
    stop("Columns not found in the dataframe.")
  }
  
  # Compute ROAS as a percentage
  data$ROAS_Percentage <- (data[[sales_col]] / data[[spend_col]])
  subset_data <- data[start_row:end_row, , drop = FALSE]
  
  average_ROAS <- mean(subset_data$ROAS_Percentage, na.rm = TRUE)
  return(average_ROAS)
}


# UI Stuff
ui <- dashboardPage(
  skin = "purple",
  
  # Dashboard Header Section
  dashboardHeader(title = tags$a(
    href = 'https://www.dirtea.com',
    tags$img(
      src = 'https://www.dirtea.com/images/Logo_Dirtea_negativ.png',
      height = '100%',
      width = '100%'
    )
  )),
  
  # Dashboard Sidebar Section
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Mindset Metrics", tabName = "mindset", icon = icon("brain")),
      menuItem(
        "Sales and Ads Analysis",
        tabName = "salesandads",
        icon = icon("rectangle-ad")
      ),
      menuItem(
        "Funnel Analysis",
        tabName = "funnel",
        icon = icon("filter")
      ),
      menuItem(
        "Simulations",
        tabName = "simulations",
        icon = icon("chart-line")
      ),
      menuItem(
        "Help",
        tabName = "help",
        icon = icon("circle-question")
      )
    )
  ),
  
  # Dashboard Body Section
  dashboardBody(
    # Background colour for Dashboard Body Section
    tags$head(tags$style(
      HTML('
      .content-wrapper {
        background-color: lavender;
      }
    ')
    )),
    
    ## Home page
    tabItems(
      tabItem("home",
              fluidPage(
                img(
                  src = 'https://www.dirtea.com/images/7792_Schriftzug_Sortenkreis_210712-1.png',
                  height = '100%',
                  width = '100%',
                  align = 'center'
                ),
                h1('DirTea Mind Metrics and Sales Analysis', align = 'center'),
                h2('Marketing Analytics Final Project', align = 'center'),
                h2('Group 4 : Mindset Metrics', align = 'center'),
                h4(
                  'by Antoine BOSSAN, Serafina CHEN, Nishant DAVE, Duoer GU, Benjamin JULLIET, Sebastian LEE, and Nhu PHAM',
                  align = 'center'
                )
              )),
      
      ## Data page
      tabItem(
        "data",
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
      tabItem(
        "dashboard",
        fluidPage(
          titlePanel("Key Performance Indicators"),
          
          # Slide bar
          sliderInput(
            "range_dashboard",
            label = "Chosen period :",
            min = 1,
            max = 1100,
            value = c(1, 1100)
          ),
          
          
          # KPI boxes
          fluidRow(
            # Awareness Box
            valueBoxOutput("Sales_Box"),
            
            
          ),
          fluidRow(
            # Awareness Box
            valueBoxOutput("Awareness_Box"),
            
            # Consideration Box
            valueBoxOutput("Consideration_Box"),
            
            # Liking Box
            valueBoxOutput("Liking_Box"),
            
            
          ),
          fluidRow(
            
            # A static infoBox
            #infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
            
            # ROAS Box
            valueBoxOutput("ROAS_Box"),
            
            #Awareness to Consideration
            valueBoxOutput("A2C_Box"),
            
            #Consideration to Liking
            valueBoxOutput("C2L_Box")
          ),
          
        h2(id = "dashboard", "KPIs :"),
        h3("Average Sales"),
        p("Average daily sales during the time period"),
        h3("Average Awareness, Consideration, Liking"),
        p("Average daily brand awareness, Consideration, Liking"),
        h3("Return on ads spend"),
        p("The amount of revenue that is earned for every dollar spent on a campaign"),
        h3("Awareness to Consideration Ratio"),
        p("The average daily Consideration / Awareness"),
        h3("Consideration to Liking Ratio"),
        p("The average daily Awarenes / Consideration")
        ),
        
      ),
      
      ## Mindset Metrics page
      tabItem("mindset",
              fluidPage(
                titlePanel("Mindset Metrics"),
                box(
                  title = "Potential",
                  status = 'info',
                  fluidRow(
                    
                    valueBoxOutput("potentialAwareness_box"),
                    valueBoxOutput("potentialConsideration_box"),
                    valueBoxOutput("potentialLiking_box")
                    
                  ),
                  width = 12
                ),
                
                box(
                  title = "Stickiness",
                  status = 'info',
                  fluidRow(
                    
                    valueBoxOutput("stickinessAwareness_box"),
                    valueBoxOutput("stickinessConsideration_box"),
                    valueBoxOutput("stickinessLiking_box")
                    
                  ),
                  width = 12
                ),
                
                box(
                  title = "Responsiveness - Awareness",
                  status = 'info',
                  fluidRow(

                    # valueBoxOutput("responseAwareInstagramAds_box"),
                    # valueBoxOutput("responseAwareTikTokAds_box"),
                    valueBoxOutput("responseAwareSEA_box"),
                    # valueBoxOutput("responseAwarePoSPromotions_box"),
                    valueBoxOutput("responseAwareColabs_box")

                  ),
                  width = 12
                ),

                box(
                  title = "Responsiveness - Consideration",
                  status = 'info',
                  fluidRow(

                    # valueBoxOutput("responseConsiderInstagramAds_box"),
                    # valueBoxOutput("responseConsiderTikTokAds_box"),
                    valueBoxOutput("responseConsiderSEA_box"),
                    # valueBoxOutput("responseConsiderPoSPromotions_box"),
                    # valueBoxOutput("responseConsiderColabs_box")

                  ),
                  width = 12
                ),

                box(
                  title = "Responsiveness - Liking",
                  status = 'info',
                  fluidRow(

                    # valueBoxOutput("responseLikingInstagramAds_box"),
                    # valueBoxOutput("responseLikingTikTokAds_box"),
                    valueBoxOutput("responseLikingSEA_box"),
                    # valueBoxOutput("responseLikingPoSPromotions_box"),
                    # valueBoxOutput("responseLikingColabs_box")

                  ),
                  width = 12
                ),

                box(
                  title = "Conversion",
                  status = 'info',
                  fluidRow(

                    valueBoxOutput("conversionAwareness_box"),
                    valueBoxOutput("conversionConsideration_box"),
                    valueBoxOutput("conversionLiking_box")

                  ),
                  width = 12
                )
                
              )), 
      
      ## Sales and Advertisement analysis page
      tabItem("salesandads",
              fluidPage(
                titlePanel("Sales and Ads Analysis"),
                
                  box(
                    status = 'primary',
                    sliderInput(
                      "range_sales",
                      label = "Range of interest:",
                      min = 1,
                      max = 1100,
                      value = c(1, 1100)
                    ), width = 6
                  ),
                # Sales Trend
                fluidRow(
                  box(
                    title = "Sales Trends",
                    status = 'primary',
                    plotOutput("sales_plot"), width = 12
                  )),
                
                # Ad expenses pie chart
                fluidRow(
                  box(
                    title = "Ad Expenses Distribution",
                    status = 'primary',
                    plotOutput("adExpense_plot"), width = 12
                  ))
                
                
              )),
    
      ## Funnel page 
      tabItem("funnel",
              fluidPage(
                titlePanel("Funnel Analysis"),
                box(
                mainPanel(plotOutput("funnelPlot")), width = 12)
              )),
    
      
      ### Simulations page
       tabItem("simulations",
               fluidPage(
                 titlePanel("Simulation"),
                 sidebarLayout(
                   sidebarPanel(
                     
                     sliderInput(
                       "instaBudget",
                       label = "Instagram Ads",
                       min = 0,
                       max = 25000,
                       value = 12000
                     ),
                     
                     sliderInput(
                       "tiktokBudget",
                       label = "Tiktok Ads",
                       min = 0,
                       max = 18000,
                       value = 12000
                     ),
                     
                     sliderInput(
                       "seaBudget",
                       label = "SEA",
                       min = 0,
                       max = 65000,
                       value = 43000
                     ),
                     
                     sliderInput(
                       "posBudget",
                       label = "PoS Promotions",
                       min = 0,
                       max = 5100,
                       value = 6000
                     ),
                    
                     sliderInput(
                       "colabBudget",
                       label = "Influencer Colabs",
                       min = 0,
                       max = 30000,
                       value = 24000
                     ),
                     
                     
                   ),
                   mainPanel(
                     box(
                       title = "Sales Forecast",
                       status = 'primary',
                       fluidRow(
                         infoBoxOutput("upperForecast"),
                         infoBoxOutput("midForecast"),
                         infoBoxOutput("lowerForecast")
                       ),
                       width = 12
                      )
                   )
                 )
       
               )),
      

      ### ______ SIMULAT END ______ ###
      ###*********************************###
      
      ## Help page
      tabItem("help",
              fluidPage(titlePanel("Help"),
                        sidebarLayout(
                          sidebarPanel(
                            h3("Sections"),
                            # Create links to sections using h1 headers
                            tags$ul(
                              tags$li(tags$a(href = "#start", "Initialization")),
                              tags$li(tags$a(href = "#data", "Data")),
                              tags$li(tags$a(href = "#dashboard", "Dashboard")),
                              tags$li(tags$a(href = "#mindset", "Mindset Metrics")),
                              tags$li(tags$a(href = "#salesandads", "Sales and Ads Analysis")),
                              tags$li(tags$a(href = "#funnel", "Funnel Analysis")),
                              tags$li(tags$a(href = "#simulations", "Simulations"))
                            )
                          ),
                          
                          mainPanel(
                            h1("Getting Started"),
                            # Initialization Section
                            h2(id = "start", "Initialization"),
                            h3("Go to Data Section and upload data to initialize the analysis."),
                            h4("The uploaded data must meet 3 criteria:"),
                            h5(
                              "1. The dataset is in .RData format and is available as 'MindSetDF' in the R environment,"
                            ),
                            h5("2. The index of the dataset represents the day"),
                            h5("3. The dataset contains the following named columns:"),
                            h5(
                              "'Sales', 'Awareness', 'Liking', 'Consideration', 'InstagramAds', 'TikTokAds', 'SEA', 'PoSPromotions', and 'InfluencerColabs'*"
                            ),
                            h6("*not necessarily in that order, but the variable names must match."),
                            h4("The supporting .rds files 'awareness_model', 'consideration_model', 'liking_model', and 'lm_model' must be present in the same folder this .R application is running in."),
                            h3("Now you are ready to analyse!"),
                            
                            # Data Section
                            h2(id = "data", "Data Section"),
                            p(
                              "In the Data Section, you are given the option to explore the uploaded dataset after the selected .RData has been uploaded to the dashboard. To explore and analyse further, please check out other sections!"
                            ),
                            
                            # Dashboard Section
                            h2(id = "dashboard", "Dashboard Section"),
                            p(
                              "This section contains Key Performance indicators that are relevant
                              to the strategice allocation of budget to the various marketing avenues
                              available the DirTea marketing team."
                            ),
                            
                            # Mindset Metrics Section
                            h2(id = "mindset", "Mindset Metrics"),
                            p("Mindset metrics provide a comprehensive view of a campaign or product's performance. In this section, potential and stickiness are calculated based on the data. Responsiveness and conversion values are taken from coefficients which are statistically significant from the model."),
                            
                            h3("Potential"),
                            p("Potential assesses the possible growth of the awareness, consideration and liking. The value is calculated by the difference between maximum value and mean and divided by the maximum."),
                            h3("Stickiness"),
                            p("Stickiness refers to the ability to retain customer interest over time. It can be calculated as the sum of the AR coefficients from AIC model."),
                            h3("Responsiveness"),
                            p("Responsiveness measures how quickly and effectively a target reacts to a marketing stimulus. We take the log-linear model and get responsiveness for each mindset metric."),
                            h3("Conversion"),
                            p("Conversion measures how much the attitudinal metric turns into sales. The multiplicative funnel model is applied to get the conversion coefficients."),
                            
                            # Sales and Ads Analysis Section
                            h2(id = "salesandads", "Sales and Ads Analysis Section"),
                            p("This section allows the user to examine sales trends directly as well as ads spendings for a given time period"), 
                            
                            # Funnel Analysis Section ### TO BE DELETED?
                            h2(id = "funnel", "Funnel Analysis"),
                            p("This section showecases the conversation rates of the funnel Awareness ⇒ Consideration ⇒  Liking"),
                            
                            # Simulations Section
                            h2(id = "simulations", "Simulation Section"),
                            p("The simulation section allows you to simulate sales in the future
                              based on a previsional budget. The predictions are monthly predictions
                              and the simulation expect monthly budgets per ad channel as input.
                              The simulation uses historical data
                              to forecast future consideration and liking. Based on that a linear 
                              regression model is used to do the predictions."),
                            
                          )
                        )))
    )
  )
)

# Server Stuff
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
    
    # Create a sequence of dates
    start_date <- as.Date("2020-01-01")  # Replace with your desired start date
    n_days <- nrow(MindSetDF)  # Assuming you want the same number of days as rows in your dataframe
    date_sequence <- seq(start_date, by = "1 day", length.out = n_days)
    
    # Add the 'Date' column to the dataframe
    MindSetDF$Date <- date_sequence
    MindSetDF$Date <- as.Date(MindSetDF$Date)
    
    # Load the data
    loadedData(MindSetDF)
  })
  
  # Data Table
  output$dataTable <- renderDataTable(loadedData())
  
  
  ## Dashboard Page
  # Create a reactive expression for average awareness calculation
  calculateAverageSales <- reactive({
    selected_range <- input$range_dashboard
    start_row <- selected_range[1]
    end_row <- selected_range[2]
    average_sales_result <- computeAverageSales(loadedData(), start_row, end_row)
    return(round(average_sales_result,2))
  })
  
  
  # Awareness box
  output$Sales_Box <- renderValueBox({
    valueBox(
      paste0(calculateAverageSales(),' €'),
      "Average Sales",
      icon = icon("coins"),
      color = "blue"
    )
  })
  
  # Create a reactive expression for average awareness calculation
  calculateAverageAwareness <- reactive({
    selected_range <- input$range_dashboard
    start_row <- selected_range[1]
    end_row <- selected_range[2]
    average_awareness_result <- computeAverageAwareness(loadedData(), start_row, end_row)
    return(round(average_awareness_result,2))
  })
  
  
  # Awareness box
  output$Awareness_Box <- renderValueBox({
    valueBox(
      paste0(calculateAverageAwareness()),
      "Average Awareness",
      icon = icon("chart-simple"),
      color = "purple"
    )
  })
  
  # Create a reactive expression for average consideration calculation
  calculateAverageConsideration <- reactive({
    selected_range <- input$range_dashboard
    start_row <- selected_range[1]
    end_row <- selected_range[2]
    average_consideration_result <- computeAverageConsideration(loadedData(), start_row, end_row)
    return(round(average_consideration_result,2))
  })
  
  
  # Consideration box
  output$Consideration_Box <- renderValueBox({
    valueBox(
      paste0(calculateAverageConsideration()),
      "Average Consideration",
      icon = icon("chart-simple"),
      color = "purple"
    )
  })
  
  # Create a reactive expression for average liking calculation
  calculateAverageLiking <- reactive({
    selected_range <- input$range_dashboard
    start_row <- selected_range[1]
    end_row <- selected_range[2]
    average_liking_result <- computeAverageLiking(loadedData(), start_row, end_row)
    return(round(average_liking_result,2))
  })
  
  
  # Average liking box
  output$Liking_Box <- renderValueBox({
    valueBox(
      paste0(calculateAverageLiking()),
      "Average Liking",
      icon = icon("chart-simple"),
      color = "purple"
    )
  })
  
  # Create a reactive expression for average ROAS calculation
  calculateROAS <- reactive({
    selected_range <- input$range_dashboard
    start_row <- selected_range[1]
    end_row <- selected_range[2]
    ROAS <- computeROAS(loadedData(), "Sales", "TotalAdvertisingSpend",start_row,end_row)
    return(round(ROAS,2))
  })
  
  
  # ROAS box
  output$ROAS_Box <- renderValueBox({
    valueBox(
      paste0(calculateROAS()),
      "Return on ads spend",
      icon = icon("rectangle-ad"),
      color = "green"
    )
  })
  
  # A2C reactive expression
  calculateA2C <- reactive({
    selected_range <- input$range_dashboard
    start_row <- selected_range[1]
    end_row <- selected_range[2]
    result <- Awareness2Consideration(loadedData(), start_row, end_row)
    return(round(result,2))
  })
  
  
  # A2C box
  output$A2C_Box <- renderValueBox({
    valueBox(
      paste0(calculateA2C()),
      "Awareness to Consideration ratio",
      icon = icon("instagram"),
      color = "yellow"
    )
  })
  
  # A2C reactive expression
  calculateC2L <- reactive({
    selected_range <- input$range_dashboard
    start_row <- selected_range[1]
    end_row <- selected_range[2]
    result <- Consideration2Liking(loadedData(), start_row, end_row)
    return(round(result,2))
  })
  
  
  # A2C box
  output$C2L_Box <- renderValueBox({
    valueBox(
      paste0(calculateC2L()),
      "Consideration to Liking ratio",
      icon = icon("thumbs-up"),
      color = "yellow"
    )
  })
  
  
  ## Mindset page
  ## Potential
  # Function to calculate potential for a given variable
  calculatePotential <- function(data, variable) {
    (100 - mean(data[[variable]])) / 100
  }
  
  # Function to create potential value box
  createPotentialValueBox <- function(variable, icon_name, icon_color) {
    df <- loadedData()
    
    potential_value <- calculatePotential(df, variable)
    
    valueBox(
      format(potential_value, digits = 3),
      variable,
      icon = icon(icon_name),
      color = icon_color
    )
  }
  
  # Usage of the functions
  output$potentialAwareness_box <- renderValueBox({
    createPotentialValueBox("Awareness", "podcast", "yellow")
  })
  
  output$potentialConsideration_box <- renderValueBox({
    createPotentialValueBox("Consideration", "person-circle-question", "purple")
  })
  
  output$potentialLiking_box <- renderValueBox({
    createPotentialValueBox("Liking", "thumbs-up", "green")
  })
  
  ## Stickiness
  # Function to calculate stickiness for a given variable
  calculateStickiness <- function(data, variable, max_order) {
    ar_model <- ar(data[[variable]], aic = TRUE, order.max = max_order)
    sum(ar_model$ar[1:min(max_order, length(ar_model$ar))])
  }
  
  # Function to create stickiness value box
  createStickinessValueBox <- function(variable, icon_name, icon_color) {
    df <- loadedData()
    
    stickiness <- calculateStickiness(df, variable, max_order = 8)
    
    valueBox(
      format(stickiness, digits = 3),
      variable,
      icon = icon(icon_name),
      color = icon_color
    )
  }
  
  # Usage of the functions
  output$stickinessAwareness_box <- renderValueBox({
    createStickinessValueBox("Awareness", "podcast", "yellow")
  })
  
  output$stickinessConsideration_box <- renderValueBox({
    createStickinessValueBox("Consideration", "person-circle-question", "purple")
  })
  
  output$stickinessLiking_box <- renderValueBox({
    createStickinessValueBox("Liking", "thumbs-up", "green")
  })
  
  ## Responsiveness
  # Commented out statistically insignificant estimates
  # Function to create a response awareness value box
  createResponseAwarenessValueBox <- function(response_variable, icon_name, icon_color) {
    df <- loadedData()
    
    df$lag_aware <- stats::lag(df$Awareness, -1)
    df$lag_aware[1] <- 0
    
    response_model <- lm(log(df$Awareness + 1) ~ log(lag_aware + 1)
                         + log(df$InstagramAds + 1) + log(df$TikTokAds + 1) + log(df$SEA + 1)
                         + log(df$PoSPromotions + 1) + log(df$InfluencerColabs + 1),
                         data = df)
    
    valueBox(
      format(summary(response_model)$coefficients[paste0('log(df$', response_variable, ' + 1)'), 'Estimate'], digits = 3),
      response_variable,
      icon = icon(icon_name),
      color = icon_color
    )
  }
  
  # Usage of the function
  # output$responseAwareInstagramAds_box <- renderValueBox({
  #   createResponseAwarenessValueBox("InstagramAds", "thumbs-up", "purple")
  # })
  # 
  # output$responseAwareTikTokAds_box <- renderValueBox({
  #   createResponseAwarenessValueBox("TikTokAds", "thumbs-up", "blue")
  # })

  output$responseAwareSEA_box <- renderValueBox({
    createResponseAwarenessValueBox("SEA", "google", "orange")
  })
  
  # output$responseAwarePoSPromotions_box <- renderValueBox({
  #   createResponseAwarenessValueBox("PoSPromotions", "thumbs-up", "orange")
  # })
  
  output$responseAwareColabs_box <- renderValueBox({
    createResponseAwarenessValueBox("InfluencerColabs", "instagram", "red")
  })
  
  
  # Function to create a response consideration value box
  createResponseConsiderationValueBox <- function(response_variable, icon_name, icon_color) {
    df <- loadedData()
    
    df$lag_consider <- stats::lag(df$Consideration, -1)
    df$lag_consider[1] <- 0
    
    response_model <- lm(log(df$Consideration + 1) ~ log(lag_consider + 1)
                         + log(df$InstagramAds + 1) + log(df$TikTokAds + 1) + log(df$SEA + 1)
                         + log(df$PoSPromotions + 1) + log(df$InfluencerColabs + 1),
                         data = df)
    
    valueBox(
      format(summary(response_model)$coefficients[paste0('log(df$', response_variable, ' + 1)'), 'Estimate'], digits = 3),
      response_variable,
      icon = icon(icon_name),
      color = icon_color
    )
  }
  
  # Usage of the function
  # output$responseConsiderInstagramAds_box <- renderValueBox({
  #   createResponseConsiderationValueBox("InstagramAds", "thumbs-up", "purple")
  # })
  # 
  # output$responseConsiderTikTokAds_box <- renderValueBox({
  #   createResponseConsiderationValueBox("TikTokAds", "thumbs-up", "blue")
  # })
  
  output$responseConsiderSEA_box <- renderValueBox({
    createResponseConsiderationValueBox("SEA", "google", "orange")
  })
  
  # output$responseConsiderPoSPromotions_box <- renderValueBox({
  #   createResponseConsiderationValueBox("PoSPromotions", "thumbs-up", "orange")
  # })
  # 
  # output$responseConsiderColabs_box <- renderValueBox({
  #   createResponseConsiderationValueBox("InfluencerColabs", "thumbs-up", "red")
  # })
  
  
  # Function to create a response liking value box
  createResponseLikingValueBox <- function(response_variable, icon_name, icon_color) {
    df <- loadedData()
    
    df$lag_liking <- stats::lag(df$Liking, -1)
    df$lag_liking[1] <- 0
    
    response_model <- lm(log(df$Liking + 1) ~ log(lag_liking + 1)
                         + log(df$InstagramAds + 1) + log(df$TikTokAds + 1) + log(df$SEA + 1)
                         + log(df$PoSPromotions + 1) + log(df$InfluencerColabs + 1),
                         data = df)
    
    valueBox(
      format(summary(response_model)$coefficients[paste0('log(df$', response_variable, ' + 1)'), 'Estimate'], digits = 3),
      response_variable,
      icon = icon(icon_name),
      color = icon_color
    )
  }
  
  # Usage of the function
  # output$responseLikingInstagramAds_box <- renderValueBox({
  #   createResponseLikingValueBox("InstagramAds", "thumbs-up", "purple")
  # })
  # 
  # output$responseLikingTikTokAds_box <- renderValueBox({
  #   createResponseLikingValueBox("TikTokAds", "thumbs-up", "blue")
  # })
  
  output$responseLikingSEA_box <- renderValueBox({
    createResponseLikingValueBox("SEA", "google", "orange")
  })
  
  # output$responseLikingPoSPromotions_box <- renderValueBox({
  #   createResponseLikingValueBox("PoSPromotions", "thumbs-up", "orange")
  # })
  # 
  # output$responseLikingColabs_box <- renderValueBox({
  #   createResponseLikingValueBox("InfluencerColabs", "thumbs-up", "red")
  # })
  
  
  ## Conversion
  # Function to calculate and create value box
  createConversionValueBox <- function(variable, icon_name, icon_color) {
    df <- loadedData()
    
    df$lag_sales <- stats::lag(df$Sales, -1)
    df$lag_sales[1] <- 0
    
    conversion <- lm(log(df$Sales+1)~log(lag_sales+1)+log(df$Awareness)+log(df$Consideration)+log(df$Liking), data = df)
    
    valueBox(
      format(summary(conversion)$coefficients[paste0('log(df$', variable, ')'), 'Estimate'], digits = 3),
      variable,
      icon = icon(icon_name),
      color = icon_color
    )
  }
  
  # Usage of the functions
  output$conversionAwareness_box <- renderValueBox({
    createConversionValueBox("Awareness", "podcast", "yellow")
  })
  
  output$conversionConsideration_box <- renderValueBox({
    createConversionValueBox("Consideration", "person-circle-question", "purple")
  })
  
  output$conversionLiking_box <- renderValueBox({
    createConversionValueBox("Liking", "thumbs-up", "green")
  })
  
  
  ## Sales and Advertisement analysis page

  # Sales trend
  output$sales_plot <- renderPlot({
    df <- loadedData()
    # Create the plot using the Index as the x-axis
    ggplot(data = loadedData(), aes(x = Day, y = Sales)) +
      geom_line() +
      labs(title = "",
           x = "Days",
           y = "Sales") +
      xlim(c(input$range_sales[1], input$range_sales[2])) +
      theme_minimal()
  })
  
  # Pie Chart
  output$adExpense_plot <- renderPlot({
    df <- loadedData()
    df_subset <- subset(df, Day >= input$range_sales[1] & Day <= input$range_sales[2])
    ad_expenses <- colSums(df_subset[, c("InstagramAds", "TikTokAds", "SEA", "PoSPromotions", "InfluencerColabs")])
    
    pie_data <- data.frame(
      AdChannel = names(ad_expenses),
      Expenses = ad_expenses,
      Percentage = ad_expenses / sum(ad_expenses) * 100,
      Legend = paste0(names(ad_expenses), " (", ad_expenses, "€)")
    )
    
    ggplot(pie_data, aes(x = "", y = Percentage, fill = Legend, label = sprintf("%.1f%%", Percentage))) +
      geom_bar(width = 1, stat = "identity") +
      geom_text(position = position_stack(vjust = 0.5), color = "white", size = 4, fontface = 'bold') +
      coord_polar("y") +
      labs(title = "",
           fill = "Ad Channel") +
      theme_void() +
      theme(legend.position = "right") +
      guides(fill = guide_legend(title = "Ad Channel", keywidth = 1, keyheight = 1))
    
  })
  
  ## Funnel page
  # Funnel Analysis Plot
  output$funnelPlot <- renderPlot({
    filtered <- loadedData()
    funnel_data <- data.frame(
      Stage = c("Awareness", "Consideration", "Liking"),
      Value = c(
        mean(filtered$Awareness),
        mean(filtered$Consideration),
        mean(filtered$Liking)
      )
    )
    ggplot(funnel_data, aes(x = Stage, y = Value)) +
      geom_bar(stat = "identity", aes(fill = Stage)) +
      coord_flip() +
      labs(title = "Funnel Analysis", x = "", y = "Average Value") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
    # Plot code for Funnel Analysis
  })
  
  ###*********************************###
  ### ______ BENJAMIN CODE START ______ ###
  
  # ## Simulations Page
  # # Function to forecast and create infobox
  simulate <- function() {
    df <- loadedData()
    
    
    # Load the lm model
    lm_model <- readRDS("lm_model.rds")
    
    # Load the arima models
    consideration_model <- readRDS("consideration_model.rds")
    liking_model <- readRDS("liking_model.rds")
    awareness_model <- readRDS("awareness_model.rds")
    
    # Choose budget
    
    sum_InstagramAds <- input$instaBudget
    sum_TikTokAds <- input$tiktokBudget
    sum_SEA <- input$seaBudget
    sum_PoSPromotions <- input$posBudget
    sum_InfluencerColabs <- input$colabBudget
    
    # Simulation horizon
    
    h <- 1
    
    # Forecast consideration, liking, and awareness for h periods with ARIMA models
    forecast_consideration <- forecast(consideration_model, h = h, level = c(80, 95))
    forecast_liking <- forecast(liking_model, h = h, level = c(80, 95))
    forecast_awareness <- forecast(awareness_model, h = h, level = c(80, 95))
    
    #   # Combine the forecasted values into the budget matrix
    budget_matrix <- cbind(
      forecast_consideration$mean,
      forecast_liking$mean,
      forecast_awareness$mean,
      sum_InstagramAds,
      sum_TikTokAds,
      sum_SEA,
      sum_PoSPromotions,
      sum_InfluencerColabs
    )
    print(budget_matrix)
    # Predict the value of Sales based on the projected budget for h periods using the lm model
    
    predicted_sales = predict(lm_model, newdata = data.frame(budget_matrix), interval = "prediction")
    predicted_sales <- as.data.frame(predicted_sales)
    return(predicted_sales)
    }
  
  
  
  
  createFitForecastInfoBox <- function(variable, icon_name, icon_color) {
     
    predicted_sales = simulate()
    print(predicted_sales)
     
     valueBox(
       format(paste0(round(predicted_sales$fit),' €'), digits = 3),
       variable,
       icon = icon(icon_name),
       color = icon_color
     )
  }
  
  createLwrForecastInfoBox <- function(variable, icon_name, icon_color) {
    
    predicted_sales = simulate()
    print(predicted_sales)
    
    valueBox(
      format(paste0(round(predicted_sales$lwr),' €'), digits = 3),
      variable,
      icon = icon(icon_name),
      color = icon_color
    )
  }
  
  createUprForecastInfoBox <- function(variable, icon_name, icon_color) {
    
    predicted_sales = simulate()
    print(predicted_sales)
    
    valueBox(
      format(paste0(round(predicted_sales$upr),' €'), digits = 3),
      variable,
      icon = icon(icon_name),
      color = icon_color
    )
  } 
  
  
  # Usage of the functions
  output$upperForecast <- renderValueBox({
     createUprForecastInfoBox("upr", "arrow-trend-up", "green")
   })
   
   output$midForecast <- renderValueBox({
     createFitForecastInfoBox("fit", "arrow-right-long", "yellow")
   })
   
  output$lowerForecast <- renderValueBox({
     createLwrForecastInfoBox("lwr", "arrow-trend-down", "red")
  })
  
  ### ______ BENJAMIN CODE END ______ ##
  ###*********************************###
  
}




shinyApp(ui, server)
