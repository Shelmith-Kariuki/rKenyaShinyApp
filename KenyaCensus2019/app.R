#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("global.R")

# Define UI for the  application 
ui <- fluidPage(title = "2019 Kenya Population and Housing Census",
  tagList(
    tags$head(tags$script(type="text/javascript", src = "code.js")),
    navbarPage(
            title = 
            span(h1("Kenya 2019 Census Data"),style = "color:black; font-weight:bolder; font-style: italic;"),
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
        tabPanel(span(style = "color:red; font-weight:bolder; font-style:italic;",  h1("Datasets")),
                 fluidRow(
                     # side bar panel
                     column(2,
                            selectInput("volume1", tags$h4(tags$b(tags$i("Select Volume"))), volumes, selected = "DataCatalogue"),
                            selectInput("dats1", tags$h4(tags$b(tags$i("Select Dataset"))), dats)),
                     # main panel
                     column(10, 
                            span(textOutput("volumedescription"), style="color:black;font-weight:bolder;font-size: large; font-style:italic;"),br(),
                            span(textOutput("datasetdescription"), style="color:black;font-weight:bolder;font-size: large; font-style:italic;"),br(),br(),br(),
                            DT::dataTableOutput("dataset"),
                            downloadButton("download1", "Download as csv", class = "btn-lg btn-success"),
                            downloadButton("download2", "Download as xlsx", class = "btn-lg btn-success"))
                 )),
        
        tabPanel(span(style = "color:red; font-weight:bolder; font-style:italic;",  h1("ICT Analysis")),
                 tabsetPanel( 
                     tabPanel(span(style = "color:red; font-weight:bolder; font-style:italic;",  h1("Dataset")),
                              br(),br(),
                              span(textOutput("icttext"), style="color:black;font-weight:bolder;font-size: large; font-style:italic;"),br(),br(),br(),
                              span(textOutput("mpo"), style="color:black;font-weight:bolder;font-size: large; font-style:italic;"),
                              span(textOutput("uoi"), style="color:black;font-weight:bolder;font-size: large; font-style:italic;"),
                              span(textOutput("dltp"), style="color:black;font-weight:bolder;font-size: large; font-style:italic;"),br(),br(),br(),
                     DT::dataTableOutput("ICT_df"),
                     downloadButton("download3", "Download as csv", class = "btn-lg btn-success"),
                     downloadButton("download4", "Download as xlsx", class = "btn-lg btn-success")),
                     tabPanel(span(style = "color:red; font-weight:bolder; font-style:italic;",  h1("Analysis")),
                 
                 fluidRow(
                     # side bar panel
                     column(2,
                            selectInput("var", tags$h4(tags$b(tags$i("Select Variable"))), ict_vars)),
                    # main panel
                    column(4, 
                           DT::dataTableOutput("maintable"),
                           downloadButton("download7", "Download as xlsx", class = "btn-lg btn-success")),
                    column(6, 
                           leafletOutput("mainmap", height=480))),
                 br(), br(),br(), br(),
                 fluidRow(
                     # side bar panel
                     column(2,
                            selectInput("county", tags$h4(tags$b(tags$i("Select County"))), c("", counties),selected = "MOMBASA")
                     )
                     ,
                     # main panel
                     column(4, DT::dataTableOutput("countytable"),br(),br(),
                            downloadButton("download8", "Download as xlsx", class = "btn-lg btn-success")),
                     column(6, plotOutput("countygraph"),br(), 
                            downloadButton('downloadPlot', 'Download Plot', class = "btn-lg btn-success")))
    ))),
    tags$style(
        '.navbar { background-color: green;}}')
    

)))


# Define server logic required to draw a histogram
server <- function(input, output, session) {

## Data Catalogue
  
output$datacatalogue1 <- renderDataTable({
  DataCatalogue
})

## volume description
output$volumedescription <- renderText({
   desc<- DataCatalogue %>% 
        filter(Volume == input$volume1) %>% 
        distinct(Volume, VolumeDecription) %>% 
        select(VolumeDecription) %>% 
        pull()
   paste0(input$volume1, " : ", desc)
})
    
## create a list of datasets for each volume   
dfs <- reactive({
        DataCatalogue %>%
        mutate(Dataset = as.character(Dataset)) %>%
        filter(Volume == input$volume1) %>%
        select(Dataset) %>% pull()
})


##  Update the dataset menu to only contain datasets for the volume selected
observeEvent(dfs(), {
    choices <- dfs()[dfs() %in% dats]
    updateSelectInput(session, "dats1", choices = choices)
})

## Output showing the datasets to be downloaded

datashown <- reactive({
    get(input$dats1)
})

## Dataset description

output$datasetdescription <- renderText({
    desc<- DataCatalogue %>% 
        filter(Dataset == input$dats1) %>% 
        select(Dataset.Description) %>% 
        pull()
    paste0(input$dats1, " : ", desc)  
})

output$dataset <- DT::renderDataTable({
    datashown()
})

## Download the dataset as .csv

output$download1 <- downloadHandler(
 
    filename = function() {
        paste0(input$dats1, ".csv")
    },
    content = function(file) {
        write.csv(datashown(), file)
    }
)

## Download the dataset as .xlsx

output$download2 <- downloadHandler(
    
    filename = function() {
        paste0(input$dats1, ".xlsx")
    },
    content = function(file) {
        write.csv(datashown(), file)
    }
)

##---------------------ICT Analysis----------------------


output$icttext <- renderText({

    paste("This dataset is a merger between V4_T2.32 (Distribution of Population Age 3 years and Above Owning a Mobile Phone by Area of
Residence, Sex, County and Sub County) and V4_T2.33 (Distribution of Population Age 3 Years and Above Using Internet and
Computer/Laptop/Tablet by Area of Residence, Sex, County and Sub-County) ")
})

output$mpo <- renderText({
  paste("MPO: Mobile Phone Ownership")
})

output$uoi <- renderText({
  paste("UoI: Use of Internet")
})

output$dltp <- renderText({
  paste("UoDLT: Use of Desktop, Laptop or Tablet")
})


phone_df <- reactive({
    ICT_df
})

output$ICT_df <- DT::renderDataTable({
    phone_df()
})

## Download the dataset as .csv


output$download3 <- downloadHandler(
    
    filename = function() {
        paste0("ICT_df.csv")
    },
    content = function(file) {
        write.csv(phone_df(), file)
    }
)

## Download the dataset as .xlsx

output$download4 <- downloadHandler(
    
    filename = function() {
        paste0("ICT_df.xlsx")
    },
    content = function(file) {
        write.csv(phone_df(), file)
    }
)

## Main table
maintable_df <- reactive({
    ICT_df %>% 
        filter(AdminArea== "County") %>% 
        select_("County", input$var)
})
output$maintable <-  DT::renderDataTable({
    maintable_df()
})

## download county data    
output$download7 <- downloadHandler(
    
    filename = function() {
        paste0(input$var, ".xlsx")
    },
    content = function(file) {
        write.csv(maintable_df(), file)
    }
)

## Tmap

#output$mainmap <-  renderTmap({



        # tm_shape(dat2) +
        #     tm_fill(ict_vars[1],palette= RColorBrewer::brewer.pal(6,"BuGn"), zindex = 401) +
        #         #tm_polygons(disvars[1], zindex = 401)+
        #         tm_borders()+
        #         tm_layout(legend.position = c("left", "bottom")) })
        # 
        # # update map
        # observe({
        #     var <- input$var
        #     tmapProxy("mainmap", session, {
        #         tm_remove_layer(401) +
        #             tm_shape(dat2) +
        #             tm_fill(var,palette= RColorBrewer::brewer.pal(6,"BuGn")) +
        #             #tm_polygons(disvars[1], zindex = 401)+
        #             tm_borders()+
        #             tm_layout(legend.position = c("left", "bottom"))
        #     })
        # })

# output$mainmap <- renderLeaflet({
# ##-------------------------------------------
# 
#   data <- ICT_df
#   data <- data %>% ungroup() %>% mutate(County = as.character(County))
# 
#   KenyaCounties_SHP2 <- spTransform(KenyaCounties_SHP,
#                                     CRS("+init=epsg:4326"))
# 
#   map <- sp::merge(KenyaCounties_SHP2, data, by="County",duplicateGeoms = TRUE)
  #map@data <- dat2

#   leaflet(map) %>%
#     addTiles() 
#   ##-------------------------------------------
#   
# })
# ##-------------------------------------------
# # map$variableplot <- as.numeric(
# #   map@data[, input$var]
# # )
# ##-------------------------------------------
# selectedvardata <- reactive({
#   map@data <- map@data %>% 
#     select_("County", input$var) 
#   colnames(map@data)[2] <- "variableplot"
# })
# 
# 
# 
# ##-------------------------------------------
# 
# observe({
#   
# # pal <- colorNumeric(
# #     palette = "YlOrRd",
# #     domain = selectedvardata()$variableplot
# #   )
#   
#   
# # labels <- sprintf(
# #     "<strong>%s</strong><br/>%g",
# #     selectedvardata()$County, selectedvardata()$variableplot
# #   ) %>% lapply(htmltools::HTML)
# #   
#   
#   
#   leafletProxy("map", data = selectedvardata()) %>% 
#     addPolygons(color = "red", weight = 1, dashArray = "3", #fillColor = ~pal(variableplot) ,
#                 #popup = paste(dat2$County, dat2[,input$var], sep=" : "),
#                 highlight = highlightOptions(
#                   weight = 4,
#                   color = "green",
#                   dashArray = "",
#                   bringToFront = TRUE),
#                 #label = labels,
#                 labelOptions = labelOptions(
#                   style = list("font-weight" = "normal", padding = "3px 8px"),
#                   textsize = "15px",
#                   direction = "auto")) %>% 
#     addLegend(position = c("bottomright"),#pal = pal,
#               values = ~variableplot,title = "")
# })  
##-------------------------------------------

output$mainmap <- renderLeaflet({
  ##-------------------------------------------
  
  data <- ICT_df
  data <- data %>% ungroup() %>% 
    mutate(County = as.character(County))%>% 
      filter(AdminArea== "County") 
  
  Kenya_df <- st_as_sf(rKenyaCensus::KenyaCounties_SHP)
  
  KenyaCounties_SHP2 <- Kenya_df %>% st_transform(crs = 4326)

  map <- full_join(KenyaCounties_SHP2, data, by="County")
  
  options(scipen = 999999)
  
  # ADD this to create variableplot
  map$variableplot <- as.numeric(
    map[, input$var][[1]]
  )

  pal <- colorBin(
    palette = "YlOrRd",
    domain = map$variableplot
  )


  labels <- sprintf(
    "<strong>%s</strong><br/>%g",
    map$County, map$variableplot
  ) %>% lapply(htmltools::HTML)
  

  leaflet(map) %>%
    addTiles() %>%
    addPolygons(color = "red", weight = 1, dashArray = "4", fillColor = ~pal(variableplot) ,
                fillOpacity = 1,
                #popup = paste(dat2$County, dat2[,input$var], sep=" : "),
                highlight = highlightOptions(
                  weight = 4,
                  color = "green",
                  dashArray = "",
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
    setView(37.76404, 0.1667792, zoom = 5.7) %>% 
    addLegend(position = c("bottomright"),pal = pal, values = ~variableplot,
              title = "")
})

        
        
 ## Sub County data

    countytab <- reactive({
        ICT_df %>% ungroup() %>%
            filter(County == input$county) %>%
            filter(AdminArea != "County") %>%
            select_("SubCounty",input$var)

    })
        output$countytable <-  DT::renderDataTable({
            countytab()
        })


        
        ## Download the subcounty dataset as .xlsx
        
        output$download8 <- downloadHandler(
            filename = function() {
                paste0(input$county,"_",input$var, ".xlsx")
            },
            content = function(file) {
                write.csv(countytab(), file)
            }
        )
## Sub County plot

plotInput <- reactive({ 
      p <-  ggplot(data = countytab(), aes_string(x = "SubCounty", y = input$var))+
                geom_bar(stat = "identity", fill = "darkgreen")+
                geom_text(aes_string(label = input$var), vjust = 0.5, hjust = -0.25 , size = 4) +
                theme(legend.position = "bottom",
                      legend.title = element_blank(),
                      axis.line = element_line(size = 1.5),
                      plot.title = element_text(family="Source Sans Pro Semibold", 
                                                size = rel(1.5), hjust = 0.5),
                      plot.subtitle = element_text(family="Source Sans Pro Semibold", size = rel(1.4), hjust = 0.5),
                      plot.caption = element_text(size = rel(1.3)),
                      axis.text = element_text(family = "Source Sans Pro Semibold", size = rel(1.2)),
                      #axis.text.x = element_text(vjust = 0.5, hjust = 0.4, angle = 30 ),
                      axis.title = element_text(family = "Source Sans Pro Semibold", size = rel(1.4)),
                      legend.text = element_text(family = "Source Sans Pro Semibold", size = rel(1.2)),
                      panel.background = element_rect(fill = NA))+
                labs(title = paste0("Distribution of ",input$var), subtitle = paste0("\nCounty: ", input$county),
                     x = "Sub Counties",caption = "@Shel_Kariuki") + coord_flip()
      if( !input$var %in% grep("_Perc", input$var, value = T, ignore.case = T)){
        p <- p + ylim(c(0, max(countytab()[,2])+10000 ))
      }
      return(p)
})

output$countygraph <-  renderPlot({
     plotInput()
      
})
        

output$downloadPlot <- downloadHandler(
    filename = function() { paste0(input$county,"_",input$var, '.png', sep='') },
    content = function(file) {
        ggsave(file,width = 8, height = 6, plot = plotInput(), device = "png")
    })
}
# Run the application 
shinyApp(ui = ui, server = server)




     
    