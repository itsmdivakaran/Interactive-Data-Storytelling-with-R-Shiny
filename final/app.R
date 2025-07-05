library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)

# Load and preprocess
data <- read.csv("supermarket_sales .csv")
data$Date <- dmy(data$Date)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  titlePanel("🛍️ Supermarket Sales Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      selectInput("branch", "Select Branch:", choices = c("All", unique(data$Branch))),
      selectInput("city", "Select City:", choices = c("All", unique(data$City))),
      selectInput(
        inputId = "product",
        label = "Select Product Line(s):",
        choices = unique(data$Product.line),
        selected = unique(data$Product.line),
        multiple = TRUE
      ),
      actionButton("reset", "Reset Filters", class = "btn btn-primary")
    ),
    
    mainPanel(
      id = "main",
      fluidRow(column(
        4, div(class = "card card-blue", h4("Total Sales"), textOutput("totalSales"))
      ), column(
        4, div(class = "card card-green", h4("Total Customers"), textOutput("totalCust"))
      ), column(
        4, div(class = "card card-yellow", h4("Average Rating"), textOutput("avgRating"))
      )),
      br(),
      
      tabsetPanel(
        tabPanel("Overview",
                 fluidRow(
                   column(12, plotlyOutput("salesDonut")),
                 ),
                 br(),
                 fluidRow(
                   column(12, plotlyOutput("salesLollipop"))
                 )
        ),
        tabPanel("Sales Trend", plotlyOutput("dailySalesPlot")),
        tabPanel(
          "Charts",
          fluidRow(column(6, plotlyOutput("salesByGender")), column(6, plotlyOutput("paymentPlot"))),
          plotlyOutput("ratingPlot")
        ),
        tabPanel("Data Table", DTOutput("salesTable"))
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    df <- data
    
    if (input$branch != "All") {
      df <- df %>% filter(Branch == input$branch)
    }
    
    if (input$city != "All") {
      df <- df %>% filter(City == input$city)
    }
    
    if (!is.null(input$product) && length(input$product) > 0) {
      df <- df %>% filter(Product.line %in% input$product)
    }
    
    df
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "branch", selected = "All")
    updateSelectInput(session, "city", selected = "All")
    updateSelectInput(session, "product", selected = "All")
  })
  
  output$totalSales <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0)
      return("₹ 0")
    paste0("₹ ", round(sum(df$Total), 2))
  })
  
  output$totalCust <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0)
      return("0")
    length(unique(df$Invoice.ID))
  })
  
  output$avgRating <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0)
      return("0.0")
    round(mean(df$Rating), 2)
  })
  
  safe_plotly <- function(p) {
    if (is.null(p$data) || nrow(p$data) == 0)
      return(NULL)
    ggplotly(p)
  }
  
  output$salesDonut <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    summary <- df %>%
      group_by(Product.line) %>%
      summarise(Total = sum(Total))
    
    plot_ly(
      data = summary,
      labels = ~Product.line,
      values = ~Total,
      type = 'pie',
      hole = 0.5,
      textinfo = 'label+percent',
      hoverinfo = 'label+value+percent'
    ) %>%
      layout(title = "Sales by Product Line (Donut)",
             showlegend = TRUE)
  })
  
  
  output$salesLollipop <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    plot <- df %>%
      group_by(Product.line) %>%
      summarise(Total = sum(Total)) %>%
      ggplot(aes(x = reorder(Product.line, Total), y = Total)) +
      geom_segment(aes(xend = Product.line, y = 0, yend = Total), color = "#2c3e50", size = 1) +
      geom_point(color = "#e74c3c", size = 4) +
      coord_flip() +
      labs(title = "Sales by Product Line (Lollipop)", x = NULL, y = "Total Sales") +
      theme_minimal()
    
    ggplotly(plot)
  })
  
  output$dailySalesPlot <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0)
      return(NULL)
    
    plot <- df %>%
      group_by(Date) %>%
      summarise(DailySales = sum(Total)) %>%
      ggplot(aes(x = Date, y = DailySales)) +
      geom_line(color = "#2c3e50", size = 1.2) +
      geom_point(color = "#2980b9", size = 2) +
      scale_y_continuous(limits = c(0, 10000), labels = scales::comma) +
      labs(title = "Daily Sales Trend", x = "Date", y = "Total Sales (₹)") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(plot, tooltip = c("x", "y"))
  })
  
  output$salesByGender <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    df_summary <- df %>%
      group_by(Gender) %>%
      summarise(Sales = sum(Total))
    
    plot_ly(
      data = df_summary,
      labels = ~Gender,
      values = ~Sales,
      type = 'pie',
      hole = 0.5,
      textinfo = 'label+percent',
      hoverinfo = 'label+value+percent',
      marker = list(
        colors = c('#3498db', '#f39c12')  # Optional custom colors
      )
    ) %>%
      layout(
        title = "Sales by Gender (Donut Chart)",
        showlegend = TRUE,
        margin = list(t = 50, b = 50, l = 50, r = 50)
      )
  })
  
  output$paymentPlot <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0)
      return(NULL)
    
    df_summary <- df %>%
      count(Payment) %>%
      mutate(perc = round(100 * n / sum(n), 1),
             label = paste0(Payment, ": ", perc, "%"))
    
    plot_ly(
      df_summary,
      labels = ~ label,
      values = ~ n,
      type = 'pie',
      textinfo = 'label+percent',
      insidetextorientation = 'radial'
    ) %>%
      layout(title = "Payment Method Distribution", showlegend = FALSE)
  })
  
  output$ratingPlot <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0)
      return(NULL)
    plot <- df %>%
      group_by(Product.line) %>%
      summarise(AverageRating = mean(Rating)) %>%
      ggplot(aes(
        x = reorder(Product.line, AverageRating),
        y = AverageRating,
        fill = Product.line
      )) +
      geom_col() +
      coord_flip() +
      labs(title = "Average Rating by Product Line", x = NULL, y = "Rating") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(plot)
  })
  
  output$salesTable <- renderDT({
    datatable(filtered_data(),
              options = list(pageLength = 10),
              filter = "top")
  })
}

shinyApp(ui, server)