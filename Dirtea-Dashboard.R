library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyr)
library(tibble)
library(reshape2)

data <- get("MindSetDF")
data_for_heatmap <- as.data.frame(cor(mtcars))
data_for_bubble <- mtcars %>%
  rownames_to_column("car") %>%
  gather(key = "metric", value = "value", -car)

# Define UI
ui <- fluidPage(
  titlePanel("DirTea Sales Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Select Date Range:"),
      checkboxGroupInput("adChannels", "Select Ad Channels:", 
                         choices = c("InstagramAds", "TikTokAds", "SEA", "PoSPromotions", "InfluencerColabs")),
      actionButton("update", "Update"),
      radioButtons("plotType", "Choose Plot Type:",
                   choices = c("Bar Chart", "Line Graph"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Sales and Ads Analysis", plotOutput("salesPlot"), DTOutput("salesTable")),
        tabPanel("Mindset Metrics", plotOutput("mindsetPlot")),
        tabPanel("Funnel Analysis", plotOutput("funnelPlot")),
        tabPanel("KPIs", verbatimTextOutput("kpiOutput"))
      ),
      plotOutput("adPerformancePlot")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    # Start with the full dataset
    filtered <- data
    
    # Filter based on selected ad channels, if any
    if (!is.null(input$adChannels) && length(input$adChannels) > 0) {
      filtered <- filtered %>%
        filter_at(vars(one_of(input$adChannels)), any_vars(. > 0))
    }
    
    # Add an index column that represents the sequence of the data
    filtered <- filtered %>%
      mutate(Index = row_number())
    
    return(filtered)
  })
  
  # Sales and Ads Analysis Plot: Time series plots showing sales trends over the selected period
  output$salesPlot <- renderPlot({
    df <- filteredData()
    
    # Create the plot using the Index as the x-axis
    ggplot(df, aes(x = Index, y = Sales)) +
      geom_line() +  # Line plot for time series
      labs(title = "Sales Trends",
           x = "Sequence Index",
           y = "Sales") +
      theme_minimal()
  })
  # Ad Channels Performance Plot: to compare the performance of different ad channels in terms of sales and engagement
  output$adPerformancePlot <- renderPlot({
    # Get the filtered data
    df <- filteredData()
    
    # Aggregate data by ad channels
    ad_data <- df %>%
      summarise(InstagramSales = sum(InstagramAds),
                TikTokSales = sum(TikTokAds),
                SEASales = sum(SEA),
                PoSPromotionsSales = sum(PoSProdmotions),  # Make sure this column name is correct
                InfluencerColabsSales = sum(InfluencerColabs))
    
    # Convert to long format for ggplot2
    ad_data_long <- pivot_longer(ad_data, cols = everything(), names_to = "AdChannel", values_to = "Sales")
    
    # Determine the plot type based on input$plotType
    if(input$plotType == "Bar Chart") {
    ggplot(ad_data_long, aes(x = AdChannel, y = Sales, fill = AdChannel)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Ad Channels Performance",
           x = "Ad Channel",
           y = "Sales") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels to 45 degrees
  } else {
    ggplot(ad_data_long, aes(x = AdChannel, y = Sales, color = AdChannel, group = AdChannel)) +
      geom_line() +
      geom_point() +
      labs(title = "Ad Channels Performance",
           x = "Ad Channel",
           y = "Sales") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels to 45 degrees
  }
})
  # Render Sales Table
  output$salesTable <- renderDT({
    datatable(filteredData())
  })

  # Ad Channels Performance Plot
  output$adPerformancePlot <- renderPlot({
    # Get the filtered data
    df <- filteredData()
    
    # Aggregate data by ad channels
    ad_data <- df %>%
      summarise(InstagramSales = sum(InstagramAds),
                TikTokSales = sum(TikTokAds),
                SEASales = sum(SEA),
                PoSPromotionsSales = sum(PoSPromotions),
                InfluencerColabsSales = sum(InfluencerColabs))
    
    # Convert to long format for ggplot2
    ad_data_long <- pivot_longer(ad_data, cols = everything(), names_to = "AdChannel", values_to = "Sales")
    
    # Determine the plot type based on input$plotType
    if(input$plotType == "Bar Chart") {
      ggplot(ad_data_long, aes(x = AdChannel, y = Sales, fill = AdChannel)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        labs(title = "Ad Channels Performance",
             x = "Ad Channel",
             y = "Sales") +
        theme_minimal()
    } else {
      ggplot(ad_data_long, aes(x = AdChannel, y = Sales, color = AdChannel, group = AdChannel)) +
        geom_line() +
        geom_point() +
        labs(title = "Ad Channels Performance",
             x = "Ad Channel",
             y = "Sales") +
        theme_minimal()
    }
  })
  
  # Heatmap Plot: to visualize the correlation between ad spend in each channel and sales outcomes
  output$heatmapPlot <- renderPlot({
    ggplot(melt(data_for_heatmap), aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Bubble Chart Plot
  output$bubbleChartPlot <- renderPlot({
    ggplot(data_for_bubble, aes(x = car, y = metric, size = value)) +
      geom_point(aes(color = value), alpha = 0.7) +
      scale_size(range = c(1, 10)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Sales Table
  output$salesTable <- renderDT({
    datatable(filteredData())
  })
  
  # Mindset Metrics Plot
  output$mindsetPlot <- renderPlot({ ggplot(filteredData(), aes(x = Index)) +
      geom_line(aes(y = Awareness, color = "Awareness")) +
      geom_line(aes(y = Liking, color = "Liking")) +
      geom_line(aes(y = Consideration, color = "Consideration")) +
      labs(title = "Mindset Metrics Over Time", x = "Index", y = "Value") +
      scale_color_manual(values = c(Awareness = "blue", Liking = "green", Consideration = "red")) +
      theme_minimal()
    # Plot code for Mindset Metrics
  })
  
  # Funnel Analysis Plot
  output$funnelPlot <- renderPlot({filtered <- filteredData()
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
  
  # KPIs
  output$kpiOutput <- renderPrint({total_sales <- sum(filtered$Sales)
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
}

shinyApp(ui = ui, server = server)
