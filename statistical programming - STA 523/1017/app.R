library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(" "),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        
        numericInput(inputId = "loan_n", label = "Number of loans:", 
                     value = 1000, min = 0),
        
        numericInput(inputId = "loan_amt", label = "Loan amount:",
                     value = 200000, min = 0, step = 10000),
        
        numericInput(inputId = "loss_per_for", label = "Loss per foreclousre:",
                     value = -225000, max = 0),
        
        sliderInput(inputId = "def", label = "Default rate:",
                    value = 0.02, min = 0, max = 0.10, step = 0.005),
        
        sliderInput(inputId = "rate", label = "Interest Rate:",
                    value = 0.025, min = 0, max = 0.10, 0.005),
        
        br(),
        h3("Plot options:"),
        br(),
        checkboxInput(inputId = "line", label = "Highlight Profit Boundary", value = F),

        selectInput(inputId = "col", label = "Fill color:",
                    choices = c("Green", "Yellow", "Purple"),
                    selected = "Green")
        
    ),

    mainPanel(
           
        
        plotOutput(outputId = "hist")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    #profits <- reactive(
    #    
    #)
    
    output$hist <- renderPlot({
    
    
    profits <- replicate(10000, {
        profit_per_loan <- input$loan_amt * input$rate
        result <- sample(c(profit_per_loan, input$loss_per_fore),
                            size= input$loan_n, replace = T, 
                         prob = c(1-input$def, input$def))
                         sum(result)}
    )
    
    df <- tibble(profit = profits/100000) 
    
    #fill_color <- switch(input$col,
    #                     "Green", "green")
    
    g <- ggplot(mapping = aes(x = profit)) +
        geom_histogram(bins = 20, fill = input$col, color = "black") +
        labs(x = "Profit in milloins", y = "frequency") +
        theme_bw(base_size = 20)
    
    if(input$line){
        g + geom_vline(xintercept = 0, color = "red", size =2)
    } else{
        g
    } 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
