library(shiny)

server <- function(input, output) {

    output$distPlot <- renderPlot({
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}


ui1 <- fluidPage(
  
  # Application title
  titlePanel("Wynagrodzenie cośtam"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

app_ui <- navbarPage("Nazwa aplikacji",
                     tabPanel("Analiza 1", ui1),
                     tabPanel("Analiza 2", ui2, icon = icon("database")),
                     theme = bs_theme(bootswatch = "cosmo"),
                     footer = shiny::HTML("
                        <footer class='text-center text-sm-start' style='width:100%;'>
                        <hr>
                        <p class='text-center' style='font-size:12px;'>
                          © 2021 Copyright:
                          <a class='text-dark' href='https://www.mi2.ai/'>MI2</a>
                        </p>
                        </footer>
                      "),
                     header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"))
                     
)




# Run the application 
shinyApp(ui = app_ui, server = server)
