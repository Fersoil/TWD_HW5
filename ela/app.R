library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)


df <- read.csv("prepared_data.csv") # load pre-prepared data

server <- function(input, output) {
  
  # generuj wykres średniego wynagrodzenia
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
        size = 1 # getOption("spinner.size", default = 1)
      )
    )
  )
)




ui2 <- fluidPage(
  titlePanel("druga analizka") 
  # TODO
)

app_ui <- navbarPage("Badanie losów studentów",
                     tabPanel("Wynagrodzenia", ui1),
                     tabPanel("Analiza 2", ui2),
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
