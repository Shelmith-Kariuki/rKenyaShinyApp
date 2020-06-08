#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rKenyaCensus)
library(DT)
library(tmap)
library(plotly)

disvars <-  c(names(V4_T2.26))
dats <- c("V4_T2.26") #unique(DataCatalogue$Dataset)
volumes <- "V4" #unique(DataCatalogue$Volume)
counties <- unique(V4_T2.26$County)

# Define UI for the  application 
ui <- fluidPage(
    
    navbarPage(
            title = span(style = "color:green; font-weight:bolder; font-style: italic;", h1("Kenya 2019 Census Mini App")),
        tabPanel(span(style = "color:red; font-weight:bolder; font-style:italic;",  h1("About")),
                 fluidRow(
                     column(10, offset = 1,
                    h4(HTML(paste("<p align='justify'> The data used to generate this app has been obtained from the <a href='https://github.com/Shelmith-Kariuki/rKenyaCensus'>rKenyaCensus</a> package.",
                                "This is an R package that contains the 2019 Kenya Population and Housing Census results. 
    The results were released by the","<a href='https://twitter.com/KNBStats'>Kenya National Bureau of Statistics</a>", "in February 2020, 
        and published in four different pdf files (Volume 1 - Volume 4). To learn more about this package, please go through", 
                                "<a href='https://shelkariuki.netlify.app/post/rkenyacensus/'>this blog post</a></p>")))),
                    column(8, offset = 2,
                     img(src='featured.jpg', height="100%", width="100%", align = "center")))),
        
        tabPanel(span(style = "color:red; font-weight:bolder; font-style:italic;",  h1("Analysis")),
                 fluidRow(
                     # side bar panel
                     column(2,
                            selectInput("volume", tags$h4(tags$b(tags$i("Select Volume"))), volumes),
                            selectInput("dats2", tags$h4(tags$b(tags$i("Select Dataset"))), dats),
                            #uiOutput("vars2"))
                            selectInput("variable", tags$h4(tags$b(tags$i("Select Variable"))), disvars))
                 ,
                    # main panel
                    column(5, dataTableOutput("maintable")),
                    column(5, tmapOutput("mainmap"))),
                 br(), br(),
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

    
dfs <- reactive({
        DataCatalogue %>% 
        mutate(Dataset = as.character(Dataset)) %>% 
        filter(Volume == input$volume) %>% 
        select(Dataset) %>% pull()
})



observeEvent(dfs(), {
    choices <- dfs()[dfs() %in% dats]
    updateSelectInput(session, "dats2", choices = choices) 
})



var <- reactive({
    get(input$dats2)
})
# 
observeEvent(var(), {
    data <- var()
    choices <- names(data)
    updateSelectInput(session, "variable", choices = choices)
})



    
output$maintable <-  renderDataTable({
    var()
})
#   
output$mainmap <-  renderTmap({
    
    observeEvent(var(), {
        data <- var()
        data <- data %>% ungroup() %>% mutate(County = as.character(County))
        dat2 <- geo_join(KenyaCounties_SHP, data, by="County")
   

            tm_shape(dat2) +
                tm_fill(disvars[4],palette= RColorBrewer::brewer.pal(6,"BuGn"), zindex = 401) +
                #tm_polygons(disvars[1], zindex = 401)+
                tm_borders()+
                tm_layout(legend.position = c("left", "bottom")) })

        })
        # update map
        observe({
            var <- input$variable
            tmapProxy("mainmap", session, {
                tm_remove_layer(401) +
                    tm_shape(dat2) +
                    tm_fill(var,palette= RColorBrewer::brewer.pal(6,"BuGn")) +
                    #tm_polygons(disvars[1], zindex = 401)+
                    tm_borders()+
                    tm_layout(legend.position = c("left", "bottom"))
            })
        })
        
#   ## Sub County data
#         
#     countytab <- reactive({
#         V4_T2.26 %>% ungroup() %>% 
#             filter(County == input$county) %>% 
#             filter(AdminArea != "County") %>% 
#             select_("SubCounty",input$variable) 
#         
#     })
#         output$countytable <-  renderDataTable({
#             countytab()
#         })
#         
#  ## Sub County plot
#         output$countygraph <-  renderPlot({
#         ggplot(data = countytab(), aes_string(x = "SubCounty", y = input$variable))+
#             geom_bar(stat = "identity", fill = "darkgreen")+
#              theme(legend.position = "right",
#                                 legend.title = element_blank(),
#                                 axis.line = element_blank(),
#                                 plot.title = element_text(family="Source Sans Pro Semibold", 
#                                                           size = rel(1.5), hjust = 0.5),
#                                 plot.subtitle = element_text(size = rel(1.5), hjust = 0.5),
#                                 plot.caption = element_text(size = rel(1.5)),
#                                 legend.text = element_text(family = "Source Sans Pro Semibold", size = rel(1.2)),
#                                 panel.background = element_blank())+
#                 coord_flip()
# })
 }

# Run the application 
shinyApp(ui = ui, server = server)




     
    