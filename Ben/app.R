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
  pivot_longer(!`County Name`,names_to="Index", values_to="value")

Oregon_stats=county_compare %>% filter(`County Name`=="Oregon") %>% mutate(Oregon_values=value) %>% select(Index,Oregon_values)
county_compare=left_join(county_compare,Oregon_stats, by="Index") %>% mutate(ratio=value/Oregon_values)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme=bs_theme(version=5, bootswatch="zephyr"),

    # Application title
    titlePanel("County Snapshot"),

    # Sidebar with a selection menu for counties
    sidebarLayout(
        sidebarPanel(
          selectizeInput(
            inputId= "checkboxes",
            label="Select Counties",
            choices=unique(county_compare$`County Name`),
            selected="Union County",
            multiple=TRUE,
            options=list(
              plugins=list("remove_button"),
              delimiter= ",",
              create=FALSE,
              persist=FALSE,
              highlight=FALSE,
              selectOnTab=TRUE,
              searchfield= list("placeholder"="Search...")
              
            )
          )
        ),
            
        # Show a plot of the generated data
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #will use the selections to create data table
    output$distPlot <- renderPlot({
        
      thisCounty=input$checkboxes
        
      f=county_compare %>% 
          filter(`County Name` %in% c(thisCounty)) %>% 
          ggplot(aes(x=reorder(Index,ratio), y= ratio, fill=`County Name`))+
          geom_col(position = "dodge2")+
          coord_flip()+
          theme(legend.position = "none")+
          labs(x="variables", y="ratio for County vs State")
        
      f 
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
