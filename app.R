# load the shinydashboard package
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)

recommendation <- read.csv('recommendation.csv',stringsAsFactors = F,header=T)

header <- dashboardHeader(title = "CMP Dashboard") 

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
             href = "https://www.salesforce.com")
  )
)

frow1 <- fluidRow(
  valueBoxOutput("profAcc")
  ,valueBoxOutput("marketShare")
  ,valueBoxOutput("profitMargin")
)

frow2 <- fluidRow(
  
  # first box for sales by quarter and region bar
  box(
    title = "Revenue per Account"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("salesQuartBar", height = "300px")
  )
  
  # second box for sales by year and region bar
  ,box(
    title = "Revenue per Product"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("salesYearBar", height = "300px")
  ) 
  
)

# share of sales by model
frow3 <- fluidRow(
  
  # first box for a line graph showing the share per model
  box(
    title = "Revenue per Region"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("shareLine", height = "300px")
  )
  
  # second box for the sales per model bar
  ,box(
    title = "Revenue by Product"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("shareBar", height = "300px")
    
  )
  
)

# data table to view the raw data
frow4 <- fluidRow(
  tabBox(
    title = "Account Data Table"
    ,width = 10
    ,id = "dataTabBox"
    ,tabPanel(
      title = "Revenue1"
      ,dataTableOutput("salesbymodel")
    )
    ,tabPanel(
      title = "Revenue2"
      ,dataTableOutput("salesbyquarter")
    )
    ,tabPanel(
      title = "Revenue3"
      ,dataTableOutput("prioryearsales")
    )
  )
)




# combine the three fluid rows to make the body
body <- dashboardBody(frow1, frow2, frow3, frow4)

ui <- dashboardPage(header, sidebar, body, skin='red')

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  total.revenue <- sum(recommendation$Revenue)
  prof.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  prof.prod <- recommendation %>% group_by(Product) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  
  

  output$profAcc <- renderValueBox({
    valueBox(
      formatC(prof.account$value, format="d", big.mark=',')
      ,paste('Top Account:',prof.account$Account)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
    
  })
  
  
  
  output$marketShare <- renderValueBox({
    
    valueBox(
      formatC(total.revenue, format="d", big.mark=',')
      ,'Total Expected Revenue'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")
  
  })
  
  
  
  output$profitMargin <- renderValueBox({
    
      valueBox(
        formatC(prof.prod$value, format="d", big.mark=',')
        ,paste('Top Product:',prof.prod$Product)
        ,icon = icon("menu-hamburger",lib='glyphicon')
        ,color = "yellow")
      
  })
  
  
  output$salesQuartBar <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=Product, y=Revenue, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
      xlab("Product") + theme(legend.position="bottom" 
                             ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Revenue by Product") + labs(fill = "Region")
  })
  
  
  output$salesYearBar <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=Account, y=Revenue, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
      xlab("Account") + theme(legend.position="bottom" 
                             ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Revenue by Region") + labs(fill = "Region")
  })
  
  
  
  output$shareLine <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=Account, y=Revenue, fill=factor(Product))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
      xlab("Account") + theme(legend.position="bottom" 
                             ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Revenue by Account") + labs(fill = "Product")
    
  })
  
  
  output$shareBar <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=Region, y=Revenue, fill=factor(Product))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
      xlab("Region") + theme(legend.position="bottom" 
                             ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Revenue by Region") + labs(fill = "Product")
    
  })
  
  
  output$salesbymodel <- renderDataTable(recommendation %>% group_by(Account) %>% summarize('Expected Revenue' = sum(Revenue)))
  output$salesbyquarter <- renderDataTable(recommendation %>% group_by(Region) %>% summarize('Expected Revenue' = sum(Revenue)))
  output$prioryearsales <- renderDataTable(recommendation %>% group_by(Product) %>% summarize('Expected Revenue' = sum(Revenue)))
  
  
  
  output$NameBox <- renderInfoBox({
    infoBox(
      title = "Date"
      ,value = Sys.Date()
      ,color = "purple"
      ,icon = icon("tachometer")
    )
  })
  
  
  output$NameBox <- renderInfoBox({
    infoBox(
      title = "Date"
      ,value = Sys.Date()
      ,color = "purple"
      ,icon = icon("tachometer")
    )
  })
  
}


shinyApp(ui, server)