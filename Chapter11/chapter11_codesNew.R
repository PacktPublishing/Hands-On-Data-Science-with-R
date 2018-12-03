#install.packages("shiny)

##Example
library(shiny)
runExample()
runExample('01_hello')
##End Example



##Template
#The UI object
ui <- fluidPage()

#The server function
server <- function(input, output) {}

#Run the app
shinyApp (ui = ui, server = server)
##End Template



##First App
ui <- fluidPage(numericInput(inputId = "df", label = "Numbers between 1 and 50:", 
                             value = 1, min = 1, max = 50, step = 1),
                plotOutput(outputId = "norm"))

server <- function(input, output) {}

#Run the app
shinyApp (ui = ui, server = server)
##End First App



##Second App
#the UI object
ui <- fluidPage(numericInput(inputId = "a_number", 
                             label = "Numbers between 1 and 50:", 
                             value = 1, min = 1, max = 50, step = 1),
                plotOutput(outputId = "a_plot"))

#the server function
server <- function(input, output) {
  output$a_plot <- renderPlot({plot(seq(0,input$a_number))})
}

#Run the app
shinyApp (ui = ui, server = server)
##End Second App



##t distribution app
#the Users Interface
ui <- fluidPage(
  titlePanel("Difference between normal and t distributions!"),
                sidebarLayout(
                  sidebarPanel(
                    numericInput(inputId = "freedom",
                                            label = "Degrees of Freedom:",
                                            value = 1, min = 1, max = 50,
                                            step = 1)), 
                  mainPanel(plotOutput(outputId = "norm")     
                  )
                )
)

#the server function
server <- function(input, output) {
  output$norm <- renderPlot({
    x <- seq(-5,5,.1)
    par(lwd = 2)
    plot(x, dnorm(x), type = 'l', ylab = 'density',
         main = 'Prob. Density Distributions')
    lines(x, dt(x, input$freedom), col = '#e66101', lty = 2)
    legend('topright', 
           legend = c('normal','t-student'), 
           col = c('#000000','#e66101'), lty = 1:2)    
  })
}
#Run the app
shinyApp (ui = ui, server = server)
##End t distribution app



##Table example
#the UI object
ui <- fluidPage(numericInput(inputId = "number",
                             label = "A number between 1 and 50:",
                             value = 1, min = 1, max = 50, step = 1),
                textInput(inputId = "text", label = "A text box", 
                          value = ""),
                tableOutput(outputId = "table_a"),
                tableOutput(outputId = "table_b"))

#the server function
server <- function(input, output) {
  the_number <- reactive({input$number})
  output$table_a <- renderTable(list(input$text, the_number()))
  output$table_b <- renderTable(list(the_number(), 
                                     isolate({input$text})))
}

#Run the app
shinyApp (ui = ui, server = server)
##End Table example



##Reactive
#the UI
ui <- fluidPage(numericInput(inputId = "freedom",label = "Degrees of Freedom:",
                             value = 1, min = 1, max = 50, step = 1),
                textInput(inputId = "title", label = "Chart title:",
                          value = "prob. density distributions"),
                actionButton(inputId = "upgrade", label = "Plot it!"),
                plotOutput(outputId = "plot_a"))

#the server function
server <- function(input, output) {
  up <- eventReactive(input$upgrade, {
    x <- seq(-5,5,.1)
    par(lwd = 2)
    plot(x, dnorm(x), type = 'l', ylab = 'density',
         main = input$title)
    lines(x, dt(x, input$freedom), col = '#e66101', lty = 2)#input$freedom
    legend('topright', legend = c('normal','t-student'), 
           col = c('#000000','#e66101'), lty = 1:2)})
  to_print <- observeEvent(input$upgrade, {print(list(input$title, 
                                                      input$freedom))})
  output$plot_a <- renderPlot(up())
} 
#Run the app
shinyApp (ui = ui, server = server)
##End reactive



##Final section
library(shiny)
library(ggplot2)
library(dplyr)
#install.packages("Ecdat")

#you must insert the codes to call the dataset here, just the ones we used above
dt <- Ecdat::Computers
dt <- dt %>% group_by(premium, ram, screen, hd)
dt <- dt %>% summarise(AveragePrice = round(mean(price), digits = 2), 
                       AverageSpeed = round(mean(speed), digits = 0))

#UI
ui <- fluidPage(
  titlePanel("Let's learn them all!", windowTitle = "Practicing"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput(outputId = "plot")), 
        tabPanel("Going further!")
      ))))
#server function			
server <- function(input, output) {
  output$plot <- renderPlot({})
}
#calling shiny
shinyApp (ui = ui, server = server)



#the UI object
ui <- fluidPage(
  titlePanel("Let's learn them all!", windowTitle = "Practicing"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "x_var",
                  label = "Choose the variable X:",
                  choices = c("Average Speed" = "AverageSpeed", "HD" = "hd")),
      selectInput(inputId = "y_var",
                  label = "Choose the variable Y:",
                  choices = c("Avarage Price" = "AveragePrice", "Average Speed" = "AverageSpeed")),
      radioButtons(inputId = "premium",
                   label = "Was the manufacturer a \"premium\" firm:",
                   choices = c("Yes" = "yes", "No" = "no"))
    ),
    mainPanel(
      tags$p("Hey there! I've learned a lot about shiny package, you should learn too, do it ",
             tags$a(href = "https://shiny.rstudio.com/", "here!")),
      tabsetPanel(
        tabPanel("Plot!", plotOutput(outputId = "plot")), 
        tabPanel("Going further!", tags$h2("Tips"),
                 tags$p("There are many things to learn, to go deep into shiny, check these ",
                        tags$a(href = "https://shiny.rstudio.com/articles/", "articles."))
        ))))
)

#the server function
server <- function(input, output) {
  output$plot <- renderPlot({
    data <- dt[dt$premium == input$premium,]
    ggplot(data = data, aes(x = get(input$x_var), y = get(input$y_var))) +
      geom_point(alpha = .5, stroke = 1.5) +
      geom_smooth(method = 'lm', se = F, show.legend = F) +
      xlab(input$x_var) +
      ylab(input$y_var) +
      theme_minimal()
  })
}

#calling shiny
shinyApp (ui = ui, server = server)
##End Final section