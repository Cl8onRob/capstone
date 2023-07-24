#County graphic and table where you select the County you want to look at.

library(tidyverse)
library(tigris)
library(sf)
library(dplyr)
library(tibble)
library(shinythemes)
library(bslib)
library(shinyWidgets)
library(plotly)
library(shiny)
library(DT)
library(ggplot2)

federal_data <- read_csv("./1.0-communities.csv")

oregon_data <-
  federal_data |>
  filter(`State/Territory` == "Oregon")

tracts_sf <- tracts(state = "OR", year = 2010)

sf_w_data <- 
  left_join(oregon_data, tracts_sf, by = c("Census tract 2010 ID" = "GEOID10")) |>
  st_as_sf()
#select data that has numeric values only
oregon_num_only=oregon_data %>% select(`County Name`,`Census tract 2010 ID`,`State/Territory`) %>%  mutate(select_if(oregon_data, is.numeric))
# if values are NA make them 0
oregon_num_only=replace(oregon_num_only,is.na(oregon_num_only),0)
#group the information for the counties
oregon_num_counties=oregon_num_only %>% group_by(`County Name`) %>% summarise_if(is.numeric,mean,na.rm=T)
#create a row for Oregon state that averages all of counties data
oregon_num_counties=oregon_num_counties %>% 
  ungroup %>% 
  summarise(`County Name`=c(`County Name`, 'Oregon'),
            across(where(is.numeric),~c(., mean(.))))
#pivot so we can look at ratios
county_compare=oregon_num_counties %>% 
  select(-matches("percentile")) %>% 
  select(-c(`Percentage of tract that is disadvantaged by area`, 
            `Share of neighbors that are identified as disadvantaged`,
            `Number of Tribal areas within Census tract for Alaska`,
            `Percent of residents who are not currently enrolled in higher ed`))%>% 
  pivot_longer(!`County Name`,names_to="Index", values_to="value")

Oregon_stats=county_compare %>% filter(`County Name`=="Oregon") %>% mutate(Oregon_values=value) %>% select(Index,Oregon_values)
county_compare=left_join(county_compare,Oregon_stats, by="Index") %>% mutate(ratio=value/Oregon_values)

# Define UI for application 
ui <- fluidPage(
  theme=bs_theme(version=5, bootswatch="zephyr"),

    # Application title
    titlePanel("County Snapshot"),
    
    downloadButton('download', "Download the data"),
    fluidRow(column(7,dataTableOutput('dto'))),
    # Sidebar with a selection menu for counties
    sidebarLayout(
        sidebarPanel(width=2,
          selectInput(
            "county",
            "Select a County",
            c("All", sort(unique(
              as.character(oregon_data$`County Name`)
            ))), selected = "Baker County", multiple=T,
            ),
              tags$div(style="height: 1000px; overflow-y:auto;",
                       checkboxGroupInput("show_vars", "Columns in the dataset to show:",
                                 names(oregon_data),selected= "County Name" ))
          )
        ,
            
        # Show a plot of the generated data
        mainPanel(width=10,
          tabsetPanel(
           id ='dataset',
           tabPanel("oregon_data", plotOutput("distPlot",height="700"),DT::dataTableOutput("mytable1"))
          )
        )  
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  #will use the selections to create data table
    output$distPlot <- renderPlot({
        
        
      f=county_compare %>% 
          filter(`County Name` %in% c(input$county)) %>% 
          ggplot(aes(x=reorder(Index,ratio), y= ratio, fill=`County Name`))+
          geom_col(position = "dodge2")+
          coord_flip()+
          theme(legend.position = "right",text=element_text(size=20))+
          labs(x="variables", y="ratio for County vs State")
        
      f 
        
        
    })
    
      # choose columns to display
      #oregondata = oregon_data[1:133, ]
     
      
      
      output$mytable1 <- DT::renderDataTable({
        DT::datatable(oregon_data %>% select(input$show_vars) %>% filter(`County Name`%in% input$county))               
        
        # original filtering without piping -- ignore -> oregon_data[,input$show_vars , drop = FALSE])
        
      })
      
      
      # Reactive expression with the data
      thedata <- reactive(oregon_data %>% select(input$show_vars) %>% filter(`County Name`== c(input$county)))
      output$download <- downloadHandler(
        filename = function(){"thename.csv"}, 
        content = function(fname){
          write.csv(thedata(), fname)
        }
      )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
