library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)


df <- iris
# Find subset of columns that are suitable for scatter plot
df_num <- df |> select(where(is.numeric))

ui <- fluidPage(
  titlePanel(
    title = div(
      img(src = "coa-sealjpg.jpg", height = "60px", style = "margin-right:15px;"),
      "Introduction to Developing Interactive Shiny Applications in R"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      varSelectInput("xvar", "Select Variable", df_num, selected = "Sepal.Length"),
      varSelectInput("yvar", "Select Variable", df_num, selected = "Petal.Length"),
      checkboxGroupInput(
        "species", "Filter by species",
        choices = unique(df$Species), 
        selected = unique(df$Species)
      )),
    mainPanel("main panel",
              fluidRow(
                splitLayout(
                  cellWidths = c("50%", "50%"), 
                  plotOutput("box"),
                  plotOutput("scatter")
                )
              )
))
)

server <- function(input, output, session) {
  subsetted <- reactive({
    req(input$species)
    df |> filter(Species %in% input$species)
  })
  
  output$box <- renderPlot({
    p1 <- ggplot(subsetted(), aes(x = Species, y = !!input$yvar, 
                                 fill = Species)) +
      geom_boxplot() 
    
    
    p1 <- p1 + 
      labs(
        title = paste("Boxplot of", input$yvar, "by Species"),
        x = "Species",
        y = input$yvar
      ) +
      theme_minimal()
    
    p1
  }, res = 100)
  
  output$scatter <- renderPlot({
    p2 <- ggplot(subsetted(), aes(x = !!input$xvar, y = !!input$yvar, color = Species)) +
      list(
        theme_minimal(),
      geom_point() 
      )
    
    
    p2 <- p2 + 
      labs(
        title = paste("Scatter Plot  of",input$xvar, "and",  input$yvar, "by Species"),
        x = "Species",
        y = input$yvar
      ) +
      theme_minimal()
    
    p2
  }, res = 100)
}

shinyApp(ui, server)
