#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
setwd("/Users/user/Desktop/Classes/STAT 425/Projects/")
library(shiny)
load("NFL_Data.Rdata")

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  titlePanel('Downloading Data'),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  choices = c(levels(NFL_Data$Team),levels(as.factor(NFL_Data$Season)))),
      downloadButton('downloadData', 'Download')
    ),
    mainPanel(
      tableOutput('table')
    )
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  datasetInput <- reactive({
   switch (input$dataset,
           "Oakland" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[1],],
           "Minnesota" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[2],],
           "Denver" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[3],],
           "Kansas City" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[4],],
           "Pittsburgh" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[5],],
           "NY Giants" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[6],],
           "Seattle" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[7],],
           "San Francisco" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[8],],
           "Indianapolis" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[9],],
           "Philadelphia" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[10],],
           "Buffalo" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[11],],
           "Green Bay" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[12],],
           "Los Angeles" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[13],],
           "Atlanta" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[14],],
           "Miami" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[15],],
           "San Diego" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[16],],
           "Tennessee" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[17],],
           "Cincinnati" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[18],],
           "New Orleans" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[19],],
           "Washington" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[20],],
           "New England" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[21],],
           "NY Jets" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[22],],
           "Cleveland" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[23],],
           "Tampa Bay" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[24],],
           "Jacksonville" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[25],],
           "Baltimore" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[26],],
           "Arizona" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[27],],
           "Detroit" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[28],],
           "Chicago" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[29],],
           "Dallas" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[30],],
           "Carolina" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[31],],
           "Houston" = NFL_Data[NFL_Data$Team==levels(NFL_Data$Team)[32],],
           "2002" = NFL_Data[NFL_Data$Season==2002,],
           "2003" = NFL_Data[NFL_Data$Season==2003,],
           "2004" = NFL_Data[NFL_Data$Season==2004,],
           "2005" = NFL_Data[NFL_Data$Season==2005,],
           "2006" = NFL_Data[NFL_Data$Season==2006,],
           "2007" = NFL_Data[NFL_Data$Season==2007,],
           "2008" = NFL_Data[NFL_Data$Season==2008,],
           "2009" = NFL_Data[NFL_Data$Season==2009,],
           "2010" = NFL_Data[NFL_Data$Season==2010,],
           "2011" = NFL_Data[NFL_Data$Season==2011,],
           "2012" = NFL_Data[NFL_Data$Season==2012,],
           "2013" = NFL_Data[NFL_Data$Season==2013,],
           "2014" = NFL_Data[NFL_Data$Season==2014,],
           "2015" = NFL_Data[NFL_Data$Season==2015,])
  })
 
   

  
  output$table <- renderTable({
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$dataset, '.csv', sep='') 
    },
    content = function(file) {
      write.table(datasetInput(), file,sep=";")
    }
  )
})

# Run the application 
shinyApp(ui = ui, server = server)

