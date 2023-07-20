#install.packages("DT")

library(DT)
library(shiny)
library(ggplot2)  

ui <- fluidPage(
  title = "Explore the data for your own analysis",
  
  # This one is linked by the id 'download'
  downloadButton('download',"Download the data"),
  fluidRow(column(7,dataTableOutput('dto'))),
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "county",
        "Select a County",
        c("All", sort(unique(
          as.character(oregon_data$`County Name`)
        ))), selected="Baker County" ,  multiple = T,
      ),
      conditionalPanel(
        'input.dataset === "oregon_data"',
        checkboxGroupInput("show_vars", "Columns in the dataset to show:",
                           names(oregon_data), selected = "County Name") #  names(oregon_data))
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("oregon_data", DT::dataTableOutput("mytable1"))
      )
    )
  )
)

server <- function(input, output) {
  
  # choose columns to display
  #oregondata = oregon_data[1:133, ]
  
  
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(oregon_data %>% select(input$show_vars) %>% filter(`County Name`%in% input$county))               
    
    # original filtering without piping -- ignore -> oregon_data[,input$show_vars , drop = FALSE])
    
  })
  
  # Reactive expression with the data
  thedata <- reactive(oregon_data %>% select(input$show_vars) %>% filter(`County Name`==c(input$county)))
  output$download <- downloadHandler(
    filename = function(){"thename.csv"}, 
    content = function(fname){
      write.csv(thedata(), fname)
    }
  )
}
shinyApp(ui, server)
