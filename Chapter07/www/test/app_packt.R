#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(data.table)
library(DT)
library(forecast)
library(ggplot2)
library(plotly)
library(xts)
library(prophet)
library(tidyverse)

colorp <- c("#00526d","#de6e6e","#006d00","#6d4736","#8f29a6")

# Helper Functions
fm <- function(x){c(as.integer(substring(x,1,4)), as.integer(substring(x,6,8)))}


##### Forecasting Data

productdb <- readRDS("productdb.rds")
products <- (unique(productdb$BNFNAME))
productslower <- tolower(products)
productdb2 <- productdb[,.(SUMQ=sum(QUANTITY)), by=c("BNFNAME","CHEMSUB")][order(-SUMQ)]
setkey(productdb2,CHEMSUB)

getTop5 <- function(x){
  CHEM <- productdb2[BNFNAME==x]$CHEMSUB[1]
  productdb2[CHEMSUB==CHEM]$BNFNAME[1:5]
}


ui <- dashboardPage (skin="green",
                     dashboardHeader(title = "ORBIXS Analytics"),
                     dashboardSidebar(
                       useShinyjs(),
                       uiOutput("selectedapp"),
                       conditionalPanel(condition="input.selectedapp=='forecast'",sidebarMenu(
                         uiOutput("products"),
                         uiOutput("forecastmodel"),
                         uiOutput("forecastmetric"),
                         uiOutput("decompose")
                       ))
                       
                     ),
                     dashboardBody(
                       tags$head(
                         tags$link(rel = "stylesheet", type = "text/css", href = "diagnostics.css"),
                         tags$link(rel = "stylesheet", type = "text/css", href = "//fonts.googleapis.com/css?family=Fanwood+Text"),
                         tags$link(rel = "stylesheet", type = "text/css", href = "//fonts.googleapis.com/css?family=Varela"),
                         tags$link(rel = "stylesheet", type = "text/css", href = "fonts.css")
                       ),
                       
                       
                       fluidRow(
                         tabsetPanel(id="inTabSet",
                                     tabPanel(title = "Placeholder", value="miscTab"
                                     ),
                                     tabPanel(title = "Forecasting", value="forecastTab",fluidRow(plotlyOutput("top5plot")),hr(),
                                              fluidRow(column(5,fluidRow(plotOutput("autoplotforecast", height="325px")),fluidRow(plotOutput("autoplot", height="325px"))),
                                                       column(7,dataTableOutput("forecastdata"))),hr(),fluidRow(dataTableOutput("productdata")))))),
                     title = "Predictive Analytics"
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  observeEvent(input$selectedapp, {
    if(input$selectedapp=="forecast") {
      updateTabsetPanel(session = session, "inTabSet", selected = "forecastTab")
    }
  })
  
  output$selectedapp <- renderUI({
    selectizeInput("selectedapp", "App Selection", choices = c("Forecasting"="forecast"), selected="forecast")
  })
  
  
  ###################################### Forecasting Code ################################################
  
  output$products <- renderUI({
    selectizeInput("products", "Product Name", choices = NULL, multiple = FALSE)
  })
  
  updateSelectizeInput(session, 'products', choices = products, server = TRUE, selected = products[1])
  
  output$forecastmodel <- renderUI({
    selectizeInput("forecastmodel", "Forecasting Model", choices = c("Auto"="auto","Holt-Winters"="hw","TBATS"="tbats","Auto ARIMA"="autoarima","Markov Chain Monte-Carlo"="mcmc"), multiple = FALSE, selected="Auto")
  })
  
  output$forecastmetric <- renderUI({
    selectizeInput("forecastmetric", "Forecasting Metric", choices = c("Sales Revenue"="rev","Quantity"="trx"), multiple = FALSE, selected="trx")
  })
  
  output$decompose <- renderUI(
    prettyCheckbox(inputId = "decompose", 
                   label = "Decompose ETS", value = TRUE,
                   icon = icon("check"), status = "success", 
                   animation = "rotate")
  )
  
  getProdData <- eventReactive(c(input$products,input$forecastmetric), {
    # print (input$products)
    # print("HERE1")
    dt1 <- productdb[BNFNAME==input$products]
    if(input$forecastmetric=="trx") {
      dt1$Metric <- dt1$QUANTITY
    } else if(input$forecastmetric=="rev") {
      dt1$Metric <- dt1$ACTCOST
    }
    dt1
  })
  
  # IMPORTANT: DO NOT CHANGE NAME "productdata"; IT IS LINKED TO A CSS PROPERTY
  output$productdata <- renderDataTable({
    dt1 <- getProdData()
    datatable(dt1, width="500px", options = list(pageLength=30,dom = 't',
                                                 columnDefs = list(list(className = 'dt-left', targets = "_all"))),rownames = FALSE,
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left; color:#de6e6e;',
                'Table 2: ', htmltools::h5('Sales Data from NHS Records')
              )
    )
  })
  
  createTS <- eventReactive(c(input$products,input$forecastmetric), {
    dt1 <- getProdData()
    tsd <- ts(dt1$Metric, start=fm(dt1$month[1]), end=fm(dt1$month[length(dt1$month)]), frequency=12)
    list(tsd=tsd,data=dt1)
  })
  
  getSalesData <- eventReactive(c(input$products,input$forecastmetric),{
    top5 <- getTop5(input$products)
    if(input$forecastmetric=="trx") {
      res <- data.frame(productdb[BNFNAME %in% top5][,.(month,BNFNAME,QUANTITY)])
      setnames(res, c("Month","Product","Metric"))
    } else if(input$forecastmetric=="rev") {
      res <- data.frame(productdb[BNFNAME %in% top5][,.(month,BNFNAME,ACTCOST)])
      setnames(res, c("Month","Product","Metric"))
    }
  })
  
  getforecast <- eventReactive(c(input$forecastmodel,input$forecastmetric),{
    m <- input$forecastmodel
    tsdata <- createTS()
    tsd <- tsdata$tsd
    data <- tsdata$data
    extra=""
    # "Auto","Holt-Winters","TBATS","Auto ARIMA"
    if (m=="hw"){
      res <- forecast(HoltWinters(tsd))
    } else if (m=="autoarima"){
      res <- forecast(auto.arima(tsd))
    } else if (m=="tbats"){
      res <- forecast(tbats(tsd))
    } else if (m=="auto"){
      res <- forecast(tsd)
    } else if (m=="mcmc"){
      dtx <- tsdata$data[,.(ds=as.Date(paste0(month,"-01"),format="%Y-%m-%d"),y=Metric)]
      m <- prophet(dtx)
      nperiods <- 22
      future <- make_future_dataframe(m, periods = nperiods, freq = 'month')
      origres <- predict(m, future, mcmc.samples = 1000)
      res <- data.frame(origres %>% rename (month=ds) %>% slice((n()-nperiods):n()))
      row.names(res) <- substr(res$month,1,7)
      res$month <- NULL
      res <- res[,c(sort(names(res)[names(res) %like% "yhat*"]),names(res)[!names(res) %like% "yhat*"])]
      setnames(res,gsub("yhat","fcst",names(res)))
      # origres[origres<0] <- 0
      extra = list(m=m,origres=origres)
    }
    # res[res<0] <-0
    list(res=res,extra=extra)
  })
  
  output$autoplot <- renderPlot({
    tsdata <- createTS()
    tsd1 <- tsdata$tsd
    pl1 <- autoplot(tsd1)
    pl1 + theme(legend.position = "bottom") + 
      ggtitle(paste0("Chart 2: Time-Series Data (Actuals)", pl1$labels$title)) + 
      theme(plot.title = element_text(color="#de6e6e", size=12, face = "bold")) +
      ylab("Quantity")
  })
  
  output$autoplotforecast <- renderPlot({
    tsd1 <- getforecast()
    tsdres <- tsd1$res
    tsdextra <- tsd1$extra
    if(input$forecastmodel=="mcmc"){
      plot(tsdextra$m, tsdextra$origres ) + theme_linedraw()
    } else {
      pl <- autoplot(tsdres)
      pl + theme(legend.position = "bottom") +
        ggtitle(paste0("Chart 1: ", pl$labels$title)) + 
        theme(plot.title = element_text(color="#de6e6e", size=12, face = "bold")) +
        ylab(input$forecastmetric)}
  })
  
  
  output$top5plot <- renderPlotly({
    data=getSalesData()
    dc <- dcast(data, Month ~ Product)
    dc$Month <- factor(dc$Month, levels = dc$Month)
    dc$Month <- as.yearmon(dc$Month)
    Quantity <- dc[,2]
    hovertext0 <- paste0(dc$Month,":<b> ",((names(dc))[2])," ",Quantity)
    pplot <- plot_ly(data = dc, x = ~Month, y=~Quantity, type="scatter", mode="lines", name=((names(dc))[2]), line = list(color = colorp[1], width=2.5), hoverinfo = "text", text = hovertext0) 
    pplot <- pplot %>%
      layout(legend = list(x = 0.1, y = 1.2, orientation = 'h'),
             plot_bgcolor='rgb(254, 247, 234)',paper_bgcolor='rgb(254, 247, 234)',hovermode = 'compare',
             annotations = list(
               list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
                    x = 0.01, y = 1.05, showarrow = F,
                    text = "<b>Top 5 Products in the same category</b>",
                    font = list(size = 14))
             ))
    
    colorp <- c("#00526d","#de6e6e","#006d00","#6d4736","#8f29a6")
    
    for (i in c(3:ncol(dc))){
      # print (i)
      yval <- dc[,i]
      nameval <- names(dc)[i]
      hovertext <- paste0(dc$Month,":<b> ",nameval," ",yval)
      pplot = pplot %>% add_trace(y = yval, name=nameval, line = list(color = colorp[i-1], width=2.5), hoverinfo = "text", text = hovertext)
    }
    
    pplot 
    
  }    
  )
  
  output$forecastdata <- renderDataTable({
    currency=''
    tsd2 <- getforecast()
    result <- data.frame(tsd2$res)
    result <- data.table(Period=rownames(result),round(result,2))
    if(input$forecastmodel=="mcmc") {
      dtr <- data.table(result)
      result <- dtr
    }
    if(input$forecastmetric=="rev") {
      # RES <<-result
      # print("HEREX")
      # result <- result[,-1,with=T]*1.4
      result <- cbind(Period=result$Period,result[,.SD*1.4,.SDcols=names(result)[!names(result) %like% "Period"]])
      if (!input$forecastmodel=="mcmc") currency='$ ' # Suppressing $ sign for mcmc revenue forecasts due to rendering issue
    }
    
    datatable(result, options = list(pageLength=30, scrollX = TRUE, dom = 't',
                                     columnDefs = list(list(className = 'dt-right', targets = "_all"))),rownames = FALSE,
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left; color:#de6e6e',
                'Table 1: ', htmltools::h5('Forecast Values with 80/95 % Confidence Intervals')
              )
    ) %>% formatCurrency(2:ncol(result),currency)
  })
  
  
  ###################################### END - Forecasting Code ################################################
  
  
}


# Run the application
shinyApp(ui = ui, server = server)

# {IND:eval parse ".Q.qp ",x;R:$[IND;eval parse ".Q.ind[",x,";til 35875]";eval parse "select[35875] from ",x];:R}ATC4
