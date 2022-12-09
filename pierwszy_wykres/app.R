library(shiny)

library(dplyr)
library(ggplot2)



df <- read.csv("data/prepared_data.csv") # load pre-prepared data

server <- function(input, output) {
  
  output$earningsPlot <- renderPlot({
    plt <- df
    
    if(input$school != "*") {
      plt <- plt %>% filter(WOJ_NAME == input$school)
    } 
    
    plt <- plt %>% 
    group_by(P_ROKDYP, P_DZIEDZINA) %>% 
      summarise(srednie_wynagrodzenie = mean(P_E_ZAR, na.rm=TRUE)) 
    plt %>% 
      filter(P_DZIEDZINA %in% input$dziedzina)  %>% 
      ggplot(aes(x = P_ROKDYP, y=srednie_wynagrodzenie, color = P_DZIEDZINA)) +
      geom_line() +
      labs(
        title = "Średnie wynagrodzenie absolwentów studiów z danej dziedziny",
        subtitle = ifelse(input$school == "*", "w całej Polsce", paste("w województwie", input$school)), 
        x = "Rok uzyskania dyplomu",
        y = "Średnie wynagrodzenie (w PLN)",
        color = "Dziedzina studiów"
      ) + theme_bw() +
      ylim(min(plt$srednie_wynagrodzenie, na.rm=TRUE), max(plt$srednie_wynagrodzenie, na.rm = TRUE))
      #ylim(2400, 5000)
  })
}


ui1 <- fluidPage(
  
  # Application title
  titlePanel("Średnie wynagrodzenie absolwentów uczelni wyższych w zależności od regionu"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "school",
        label = "Wybór regionu:",
        choices = c("Cała polska" = "*", unique(pull(df, WOJ_NAME)))
      ),
      checkboxGroupInput("dziedzina", label = "Wybierz dziedziny studiów do pokazania na wykresie",
                         choices = unique(pull(df, P_DZIEDZINA)),
                         selected = unique(pull(df, P_DZIEDZINA))),),
    
    # Show a plot of the generated distribution
    mainPanel(
      shinycssloaders::withSpinner(
        plotOutput("earningsPlot"), 
        type = 1, 
        color = "#00ff00", 
        size = 2 # getOption("spinner.size", default = 1)
      )
    )
  )
)



shinyApp(ui = ui1, server = server)
