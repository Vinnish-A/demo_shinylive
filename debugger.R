library(shiny)
library(glmnet)

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(sidebarPanel(
    sliderInput(
      "Sepal.Length",
      "Sepal.Length",
      min = iris$Sepal.Length |> range() |> _[1] |> floor(),
      max = iris$Sepal.Length |> range() |> _[2] |> ceiling(),
      value = iris$Sepal.Length[1]
    ), 
    sliderInput(
      "Sepal.Width",
      "Sepal.Width",
      min = iris$Sepal.Width |> range() |> _[1] |> floor(),
      max = iris$Sepal.Width |> range() |> _[2] |> ceiling(),
      value = iris$Sepal.Width[1], step = 0.1
    ), 
    sliderInput(
      "Petal.Length",
      "Petal.Length",
      min = iris$Petal.Length |> range() |> _[1] |> floor(),
      max = iris$Petal.Length |> range() |> _[2] |> ceiling(),
      value = iris$Petal.Length[1]
    ), 
    sliderInput(
      "Petal.Width",
      "Petal.Width",
      min = iris$Petal.Width |> range() |> _[1] |> floor(),
      max = iris$Petal.Width |> range() |> _[2] |> ceiling(),
      value = iris$Petal.Width[1]
    ), 
    actionButton("go", "GO!")
  ), # Show a plot of the generated distribution
  mainPanel(textOutput("distPlot"))))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$go, {
    output$distPlot <- renderText({
      # generate bins based on input$bins from ui.R
      library(glmnet)
      
      data = iris[iris$Species != 'virginica', ]
      data$Species = ifelse(data$Species == 'setosa', 0, 1)
      X = data[, 1:4] |> as.matrix()
      Y = data[, 5] |> as.matrix()
      
      model = glmnet(
        x = X,
        y = Y,
        alpha = 1,
        lambda = 0.001,
        family = binomial()
      )
      coef(model)
      
      newX = c(input$Sepal.Length, input$Sepal.Width, input$Petal.Length, input$Petal.Width)
      names(newX) = NULL
      
      res = predict(model, newX, type = 'response')[, 1]
      res = ifelse(res < 0.5, 'It\'s setosa!', 'It\'s versicolor')
      return(res)
      
    })
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)