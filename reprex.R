
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(tmap)
library(plotly)

##------------------------------------------------------------------------------------
## Please use this dataset as an example
volume <- sort(rep(c("Volume1", "Volume2", "Volume3"), 4))
dataset <- sort(rep(paste0("dataset",1:6), 2))
variables <- rep(paste0("var",1:2), 6)

df <- data.frame(volume, dataset, variables)

counties <- unique(V4_T2.26$County)

##------------------------------------------------------------------------------------


# Define UI for the  application 
ui <- fluidPage(
  
  navbarPage(
    title = span(style = "color:green; font-weight:bolder; font-style: italic;", h1("Mini App")),

    tabPanel(span(style = "color:red; font-weight:bolder; font-style:italic;",  h1("Analysis")),
             fluidRow(
               # side bar panel
               column(2,
                      selectInput("county", tags$h4(tags$b(tags$i("Select County"))), c("", counties),selected = "")
               )
               ,
               # main panel
               column(5, dataTableOutput("countytable")),
               column(5, plotOutput("countygraph")))
    )
    
    
  ))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
 ## A list of the datasets for each volume 
  dfs <- reactive({
    DataCatalogue %>% 
      mutate(Dataset = as.character(Dataset)) %>% 
      filter(Volume == input$volume) %>% 
      select(Dataset) %>% pull()
  })
  
  
  ## Drop down to select dataset (works)
  output$dats2 <- renderUI({
    selectizeInput('dat', tags$h4(tags$b(tags$i("Select Dataset"))), choices = c("select" = "", dfs()))
  })
  

 ## Drop down to select variable  (Insert code here)
  

  ## Sub County data
  
  countytab <- reactive({
    ## To fill later
    
  })
  output$countytable <-  renderDataTable({
    countytab()
  })
  
  ## Sub County plot
  output$countygraph <-  renderPlot({
    ## To fill later
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)





