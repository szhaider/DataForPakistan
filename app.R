
library(tidyverse)
library(vroom)
library(glue)
library(janitor)
library(sf)
library(leaflet)
library(leaflet.minicharts, warn.conflicts = FALSE)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(utf8)
library(scales)
library(bslib)
library(shinycssloaders)
library(htmltools)
library(htmlwidgets)
library(kableExtra)
library(knitr)
library(DT)
library(shinyjs) 
library(glue)
library(mapview)
library(webshot)
library(plotly)
library(ggrepel)
library(ggridges)
library(ggthemes)
library(shinyWidgets)
library(ggrepel)
library(zip)
library(leafsync)

theme_set(theme_light())

#All the data preparation steps are in data_preparation file
#Reading in data and also 
#Dropping the variables which are misfit here since we have no source for them in the glossary of Pakistan_indicators
# thier source in N/A
# Keeping only HIES/PSLM and MICS indicators in this

data <- readRDS("data/pak_ind.RDS") %>% 
  filter(source != "N/A")

#Adding context variable for color reversing 

data <- 
  data %>%
  mutate(positive = context=="positive") %>% 
  mutate(negative = context== "negative") 


#Shape file   
pak_shp <- st_read("data/pakistan_indicators.shp", stringsAsFactors=FALSE)
pak_shp1 <- read_sf("data/pakistan_indicators.shp")

pak_shp2 <- st_transform(pak_shp1, crs ="+proj=longlat +ellps=WGS84 +datum=WGS84")

coord <- sf::st_coordinates(pak_shp2) #Geting lon and lat out of shp file for circles

coord <- unique(coord) # 1 for each district

coord <- as_tibble(coord) %>% 
  rename("long" = X, "lat" = Y) 

pak_shp <- pak_shp %>% 
  clean_names() %>% 
  select(year, province, district, geometry) %>% 
  mutate_if(is.character, utf8_encode) %>% 
  st_as_sf() %>% 
  arrange(year, province, district)

#Taking 2019 and 2017 out for scatterplots since not available for both indicators
yrs <- data %>% 
  distinct(year) %>% 
  arrange(desc(year)) %>% 
  slice(-1,-3)

#Taking out islamabad for scatterplots
prvs <- data %>%  
  distinct(province) %>% 
  slice(-2) %>% 
  arrange(desc(province))

district_unique <- data %>% 
  mutate(district = fct_reorder(factor(district),
                                value, 
                                .na_rm = F,
                                .desc=T)) %>% 
  filter(!is.na(value),
         indicator != "Population Census") %>% 
  select(district) %>% 
  distinct() 

#User Interface

ui <- function(request){
  navbarPage("Pakistan Indicators",
             header = tagList(
             ),
             tabPanel("INTERACTIVE MAPS", 
                      bootstrapPage(theme = shinytheme("flatly")),
                      tags$style(type = 'text/css', '#map {height: calc(100vh - 80px) !important;}', style= 'padding-top:0px;'),
                      leafletOutput("map") %>% 
                        withSpinner(),
                      absolutePanel(id = "controls", class = "panel panel-default", fixed= TRUE,
                                    draggable = TRUE, bottom = "auto", right = "auto", left = 70, top = 110,
                                    width = 230, height = "auto",
                                    style = "background-color: white;
                                                   opacity: 0.85;  
                                                   padding: 20px 20px 20px 20px;
                                                   margin: auto;
                                                   border-radius: 5pt;
                                                   box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                                   padding-bottom: 2mm;
                                                   padding-top: 1mm;",
                                    selectInput("family",
                                                "Select Domain:",
                                                choices = unique(data$domain)),
                                    selectInput("stat",
                                                "Select Indicator: ", 
                                                choices = unique(data$indicator)),
                                    selectInput("time",
                                                "Select Year:",
                                                choices = unique(data$year_1),
                                                # selected = median(data$year),
                                                width = "100%",
                                                multiple = FALSE),
                                    br(),
                                    verbatimTextOutput("source1"),
                                    tags$head(tags$style("#source1{color:black; font-size:12px; font-style:italic; 
              overflow-y:scroll; max-height: 120px; background: white;}")),
                                    verbatimTextOutput("source2"),
                                    tags$head(tags$style("#source2{color:black; font-size:12px; font-style:italic; 
              overflow-y:scroll; max-height: 120px; background: white;}"))
                      )
             ),
             tabPanel("COMPARISON MAPS",
                      mainPanel(
                        width = 12,
                        fluidRow(
                          column(width = 6,
                                 offset = 0,
                                 style = 
                                   'padding-bottom:0px; 
                                       padding-left:0px; 
                                       padding-right:0px; 
                                       margin-left:-10px; 
                                       position: relative;',
                                 tags$style(type = 'text/css', '#double_map_1 {height: calc(98vh - 50px) !important;}'),
                                 leafletOutput("double_map_1", width = "100%", height = "400px"),
                                 absolutePanel(id = "controls", class = "panel panel-default", fixed= FALSE,
                                               draggable = TRUE, bottom = "auto", right = "auto", left = 10, top = 10,
                                               width = 240, height = "auto",
                                               style = "background-color: white;
                                                   opacity: 0.85;
                                                   padding: 20px 20px 20px 20px;
                                                   margin: auto;
                                                   border-radius: 5pt;
                                                   box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                                   padding-bottom: 2mm;
                                                   padding-top: 1mm;",
                                               br(),
                                               selectInput("family_c1", "Select Domain for Map1:",
                                                           choices = unique(data$domain)),
                                               selectInput("stat_c1",
                                                           "Choose Indicator for Map1: ",
                                                           choices = unique(data$indicator)),
                                               
                                               selectInput("time_c1", "Select Year for Map1",
                                                           choices = unique(data$year_1))
                                 )
                          ),
                          column(width = 6,
                                 offset = 0,
                                 style = 
                                   'padding-bottom:0px; 
                                       padding-left:2px; 
                                       padding-right:2px; 
                                       margin-left:0px;
                                       margin-right:5px;
                                       position: relative;',
                                 tags$style(type = 'text/css', '#double_map_2 {height: calc(98vh - 50px) !important;}'),
                                 leafletOutput("double_map_2", width = "100%", height = "400px"),
                                 absolutePanel(id = "controls", class = "panel panel-default", fixed= FALSE,
                                               draggable = TRUE, bottom = "auto", right = "auto", left = 10, top = 10,
                                               width = 240, height = "auto",
                                               style = "background-color: white;
                                                   opacity: 0.85;
                                                   padding: 20px 20px 20px 20px;
                                                   margin: auto;
                                                   border-radius: 5pt;
                                                   box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                                   padding-bottom: 2mm;
                                                   padding-top: 1mm;",
                                               br(),
                                               selectInput("family_c2", 
                                                           "Select Domain for Map2:",
                                                           choices = unique(data$domain)),
                                               selectInput("stat_c2",
                                                           "Choose Indicator for Map2: ",
                                                           choices = unique(data$indicator)),
                                               selectInput("time_c2", 
                                                           "Select Year for Map2",
                                                           choices = unique(data$year_1))
                                               
                                 )
                          )
                        )
                      )
             ),
             tabPanel("GRAPHS",
                      sidebarLayout( 
                        sidebarPanel(width = 3,
                                     style = "background-color: white;
                               opacity: 0.85;  
                               padding: 20px 20px 20px 20px;
                               margin: auto;
                               border-radius: 5pt;
                               box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                               padding-bottom: 2mm;
                               padding-top: 1mm;",
                                     
                                     selectInput("family2", "Choose Domain:",
                                                 choices = unique(data$domain)),
                                     
                                     selectInput("stat2",
                                                 "Choose Indicator: ", 
                                                 choices = unique(data$indicator)),
                                     
                                     selectInput("time2", "Choose Year:",
                                                 choices = unique(data$year_1)),
                                     
                                     selectInput("prov2", "Select Province: ",
                                                 choices = data %>% distinct(province) %>% filter(province != "Federal Capital Territory")
                                     ),
                                     verbatimTextOutput("source_graph"),
                                     tags$head(tags$style("#source_graph{color:black; font-size:12px; font-style:italic; 
overflow-y:scroll; max-height: 120px; background: white;}")),
                                     br(),
                                     downloadButton("downloadplot1", "Download Plot", class = "btn-success")
                        ),
                        mainPanel(
                          h4("District-wise Comparison"),
                          plotOutput("plot1",
                                     height = "450px", width = 900) %>% 
                            withSpinner()
                        ))),
             tabPanel("TIME-SERIES",
                      sidebarLayout( 
                        sidebarPanel(width = 3,
                                     style = "background-color: white;
                                     opacity: 0.85;  
                                     padding: 20px 20px 20px 20px;
                                     margin: auto;
                                     border-radius: 5pt;
                                     box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                     padding-bottom: 2mm;
                                     padding-top: 1mm;",
                                     selectInput("family_t", "Choose Domain:",
                                                 choices = unique(data$domain)),
                                     selectInput("stat_t",
                                                 "Choose Indicator: ", 
                                                 choices = unique(data$indicator)),
                                     selectizeInput("dist_t", "Select District: ", 
                                                    choices = district_unique,
                                                    selected = c("Awaran", "Chagai",
                                                                 "Gwadar", "Khuzdar",
                                                                 "Quetta"),
                                                    multiple = TRUE),
                                     verbatimTextOutput("source_timeseries"),
                                     tags$head(tags$style("#source_timeseries{color:black; font-size:12px; font-style:italic; 
overflow-y:scroll; max-height: 120px; background: white;}"))
                        ),
                        mainPanel(
                          h3("Time Trend of Selected Indicators"),
                          plotlyOutput("plot2",
                                       height = "400px", width = 900) %>% 
                            withSpinner() ,
                        ))),
             tabPanel("SCATTER PLOTS",
                      sidebarLayout( 
                        sidebarPanel(width = 3,
                                     style = "background-color: white;
                                               opacity: 0.85;  
                                               padding: 20px 20px 20px 20px;
                                               margin: auto;
                                               border-radius: 5pt;
                                               box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                               padding-bottom: 2mm;
                                               padding-top: 1mm;",
                                     selectInput("time_s", "Choose Year:",
                                                 choices = yrs),
                                     selectInput("stat_s1",
                                                 "Choose Indicator 1: ", 
                                                 choices = unique(data$indicator)),
                                     selectInput("stat_s2",
                                                 "Choose Indicator 2: ", 
                                                 choices = unique(data$indicator)),
                                     actionButton("prov_s", "Provincial Disaggregation",  class = "btn-success"),
                                     br(),
                                     br(),
                                     verbatimTextOutput("source_compgraph_1"),
                                     tags$head(tags$style("#source_compgraph_1{color:black; font-size:12px; font-style:italic; 
overflow-y:scroll; max-height: 120px; background: white;}")),
                                     verbatimTextOutput("source_compgraph_2"),
                                     tags$head(tags$style("#source_compgraph_2{color:black; font-size:12px; font-style:italic; 
overflow-y:scroll; max-height: 120px; background: white;}")),
                        ),
                        mainPanel(
                          h3("Scatterplot of Selected Indicators"),
                          plotOutput("plot3", click = "plot_click_s",
                                     
                                     height = "450px", width = 900) %>% 
                            withSpinner(),
                          br(),
                          downloadButton("downloadplot3", "Download Plot", class = "btn-success")
                        )
                      )),
             tabPanel("TABLE",
                      sidebarLayout( 
                        sidebarPanel(width = 3,
                                     style = "background-color: white;
                                 opacity: 0.90;  
                                 padding: 20px 20px 20px 20px;
                                 margin: auto;
                                 border-radius: 5pt;
                                 box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                 padding-bottom: 2mm;
                                 padding-top: 1mm;",
                                     selectInput("family1", "Choose Domain:",
                                                 choices = unique(data$domain)),
                                     selectInput("stat1",
                                                 "Choose Indicator: ", 
                                                 choices = unique(data$indicator)),
                                     selectInput("time1", "Choose Year:", 
                                                 choices = unique(data$year_1)),
                                     selectInput("prov1", "Select Province: ",
                                                 choices = unique(data$province)),
                                     verbatimTextOutput("table_note"),
                                     tags$head(tags$style("#table_note{color:black; font-size:12px; font-style:italic; 
overflow-y:scroll; max-height: 120px; background: white;}")),
                                     br(),
                                     actionButton("national",
                                                  "Click for National Level Table",
                                                  class = "btn-success")
                        ),
                        
                        mainPanel(
                          column(width = 12,
                                 dataTableOutput("table1", width = 900) %>% 
                                   withSpinner(),
                                 downloadButton("downloadtable",
                                                "Download Table", class = "btn-success")
                          ))
                      )),
             tabPanel("ABOUT",
                      tabsetPanel(
                        tabPanel("DOCUMENTATION",
                                 mainPanel(
                                   br(),
                                   h4(strong("This App covers District level maps of Pakistan for various Social and Welfare Indicators")),
                                   br(),
                                   h4(strong("The App may be used to assess the changes over time in various Districts of Pakistan")), 
                                   tags$hr(),
                                   fluidRow(downloadButton("bulkdownload", "Download Complete Dataset",
                                                           class = "btn-success"),
                                            downloadButton("shapefile", "Download Pakistan Shapefile",
                                                           class = "btn-success"),
                                            downloadButton("glossary", "Download Data Glossay", class= "btn-success"),
                                            hr(),
                                            h4(strong("Micro Data Web Links")), 
                                            br(),
                                            tags$a(href= "https://microdata.worldbank.org/index.php/catalog?sort_by=rank&sort_order=desc&sk=pakistan", "Data Link for Pakistan", target="_blank"), br(),
                                            div(tags$em(span("World Bank's Micro Data Repository")), style="color:red"),
                                            tags$hr(),
                                            h4(strong("Data Repository")),
                                            br(),
                                            tags$a(href= "https://github.com/haider-zeeshan51214", "Repo Link", target="_blank"), br(),
                                            div(tags$em(span("Will be on Github repo")), style="color:red"),
                                            
                                   ),
                                   tags$hr(),
                                 )
                        ),
                        # tabPanel("LAB",
                        #          mainPanel(
                        #            br(),
                        #            h4(strong("Here, links of the further dashboards of the same family will be added"))
                        #          )),
                        tabPanel("ABOUT",
                                 mainPanel(
                                   tags$br(),tags$br(), tags$h4(strong("Authors/Developers")),
                                   "Moritz Meyer, Senior Economist, Poverty & Equity GP - World Bank",tags$br(),
                                   "Zeeshan Haider, Consultant, Poverty & Equity GP - World Bank",tags$br(),
                                   tags$br(),tags$h4(strong("Contacts")),
                                   "mmeyer3@worldbank.org",tags$br(),
                                   "shaider7@worldbank.org"
                                 ))
                        
                      )
             )
  )
}

#Server
server <- function(input, output, session) {
  
  #Main Map  
  #selecting domain  
  d <- reactive({
    req(input$family)
    data %>% 
      filter(domain == input$family)
  })
  
  #Updating indicators based on seelcted domain    
  observeEvent(d(),{
    req(input$family)
    choices <- unique(d()$indicator)
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "stat", 
      choices = choices)
  })
  
  #Selected indicator  
  d0 <- reactive({
    req(input$stat)
    data %>% 
      filter(indicator == input$stat)
  })
  
  #Updating years based on selected indicator  
  observeEvent(d0(), {                                 
    req(input$stat)
    updated_years_m <- d0() %>% 
      filter(!is.na(value))
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "time", 
      choices =sort(unique(updated_years_m$year_1), decreasing = T)  
    )
    
  })
  
  #making a reactive function for dataset to be used based on User's selection  
  d1 <- reactive({
    data %>% 
      filter(indicator == input$stat,
             year_1 == input$time)
    #%>% 
    #    arrange(district)
  }) %>%  #putting preselected inputs in cache
    bindCache(input$stat,
              input$time)
  
  #Labelling for the Map       
  labels <- reactive({
    paste0(glue::glue("<b>District</b>: { pak_shp$district } </br>"), glue::glue("<b>Indicator: </b>"), " ", glue::glue("{ round(d1()$value, 2) }"), " ", glue("{d1()$units}"), sep = "") %>% 
      lapply(htmltools::HTML)                                                                             
  })
  
  #Color scheme definition       
  pal_new <- reactive({
    req(unique(d1()$context) %in% c("negative", "positive"))
    if (unique(d1()$context) == "negative"){
      c('#FFEDA0', '#FED976', '#FEB24C', '#FD8D3C', '#FC4E2A','#E31A1C')
    } else {
      c('#E31A1C', '#FC4E2A','#FD8D3C', '#FEB24C', '#FED976', '#FFEDA0')
    }
    
  })
  
  
  #Main Map reactive function  
  #Rendering main Map  
  #Lealfet
  output$map <- renderLeaflet({
    # message("rendering map")
    leaflet(options = leafletOptions(zoomSnap = 0.20, zoomDelta = 0.20)) %>%
      addProviderTiles(providers$CartoDB, group = "CARTO") %>%
      setView(lng=69.5, lat = 30.5, zoom = 5.3)
  })
  
  pal <- 
    reactive(colorBin(palette = pal_new()  , bins= 6, 
                      na.color = "grey", 
                      domain= d1()$value))
  
  observe({
    req(input$stat)
    leafletProxy("map", 
                 data= st_transform(subset(pak_shp, 
                                           year==d1()$year), 
                                    crs=4326)) %>% 
      clearShapes() %>% 
      addPolygons(label= labels(),
                  labelOptions = labelOptions(
                    style = list("font-weight"= "normal",   
                                 padding= "3px 8px",
                                 "color"= "#cc4c02"), 
                    textsize= "15px",
                    direction = "auto"
                  ),
                  fillColor =  ~pal()(d1()$value),
                  fillOpacity = 0.7,
                  stroke = TRUE,
                  color= "white",
                  weight = 1,
                  opacity = 0.5,
                  fill = TRUE,
                  dashArray = NULL,
                  smoothFactor = 0.5,
                  highlightOptions = highlightOptions(weight= 5,
                                                      fillOpacity = 1,
                                                      opacity= 1,
                                                      bringToFront = TRUE), 
                  group = "Polygons") %>%
      addMeasure() %>% 
      addScaleBar("bottomright")
    
    leafletProxy("map", data= st_transform(subset(pak_shp, year==d1()$year), 
                                           crs=4326)) %>%
      clearControls() %>%
      addLegend("bottomright",
                pal= pal(),
                values= ~d1()$value,
                title = "Legend",
                opacity= 1,
                labFormat = labelFormat(
                  between = "  :  ",
                  digits = 2))

  })

  #Comparison Maps
  
  #Selected Domian for map1       
  d_bd1 <- reactive({
    data %>% 
      filter(domain == input$family_c1)
  })
  
  #Selected domain for map 2    
  d_bd2 <- reactive({
    data %>% 
      filter(domain == input$family_c2)
  })
  
  #Updating indicators for selected domain (Map1)    
  observeEvent(d_bd2(),{
    choices <- unique(d_bd2()$indicator)
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "stat_c2", 
      choices = choices)
  })
  
  #Updating indicators for selected domain (Map2)    
  
  observeEvent(d_bd1(),{
    choices <- unique(d_bd1()$indicator)
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "stat_c1", 
      choices = choices)
  })
  
  #Selecting Indicator for map 1    
  d_ba1 <- reactive({
    data %>% 
      filter(indicator == input$stat_c1)
  })
  
  #Selecting Indicator for map 2    
  d_bb1 <- reactive({
    data %>% 
      filter(indicator == input$stat_c2)
  })
  
  #Updating years for map 1    
  observeEvent(d_ba1(), {                                
    updated_years_c1 <- d_ba1() %>% 
      filter(!is.na(value))
    updateSelectizeInput(
      session = getDefaultReactiveDomain(),
      "time_c1", 
      choices = sort(unique(updated_years_c1$year_1), decreasing = T)
    )
  })
  #Updating years for map 2    
  observeEvent(d_bb1(), {                                 
    updated_years_c2 <- d_bb1() %>% 
      filter(!is.na(value))
    updateSelectizeInput(
      session = getDefaultReactiveDomain(),
      "time_c2", 
      choices = sort(unique(updated_years_c2$year_1), decreasing = T)
    )
  })
  
  #User selection for Map 1 Dataset        
  d_c1 <- reactive({
    data %>% 
      filter(indicator == input$stat_c1,
             year_1 == input$time_c1)
  })
  
  # Double map 1 Lealfet
  output$double_map_1 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.20, zoomDelta = 0.20)) %>%
      addProviderTiles(providers$CartoDB, group = "CARTO") %>%
      setView(lng=68.5, lat = 30.5, zoom = 5) 
  })
  
  #To render leafelt map before proxy observer updates
  outputOptions(output, "double_map_1", suspendWhenHidden = FALSE)
  
  #LAbels for map 1        
  labels1 <- reactive({
    paste0(glue::glue("<b>District</b>: { pak_shp$district } </br>"), glue::glue("<b>Indicator 1: </b>"), " ", glue::glue("{ round(d_c1()$value, 2) }"), " ", glue("{d_c1()$units}"), sep = "") %>% 
      lapply(htmltools::HTML)                                                                             
  })
  
  #Color Scheme for map1    
  pal_new_map1 <- reactive({
    req(unique(d_c1()$context) %in% c("negative", "positive"))
    if (unique(d_c1()$context) == "negative"){
      c('#FFEDA0', '#FED976', '#FEB24C', '#FD8D3C', '#FC4E2A','#E31A1C')
    } else {
      c('#E31A1C', '#FC4E2A','#FD8D3C', '#FEB24C', '#FED976', '#FFEDA0')
    }
    
  })
  
  pal1 <- reactive(colorBin(palette = pal_new_map1()  , 
                            bins= 6, na.color = "grey", 
                            domain= d_c1()$value))
  
  observe({
    req(input$stat_c1)
    leafletProxy("double_map_1", data=st_transform(subset(pak_shp, year==d_c1()$year), 
                                                   crs=4326)) %>% 
      clearShapes() %>% 
      addPolygons(label= labels1(),
                  labelOptions = labelOptions(
                    style = list("font-weight"= "normal",    
                                 padding= "3px 8px",
                                 "color"= "#cc4c02"), 
                    textsize= "15px",
                    direction = "auto" ),
                  fillColor = ~pal1()(d_c1()$value),
                  fillOpacity = 0.7,
                  stroke = TRUE,
                  color= "white",
                  weight = 0.8,
                  opacity = 0.5,
                  fill = TRUE,
                  smoothFactor = 0.5,
                  highlightOptions = highlightOptions(weight= 5,
                                                      fillOpacity = 1,
                                                      opacity= 1,
                                                      bringToFront = TRUE),
                  
                  group = "Polygons") %>%
      addLayersControl(baseGroups = c("ESRI", "ST Terrain", "Toner Lite", "OSM","ESRI IMG"),
                       overlayGroups = c("Polygons"),
                       options = layersControlOptions(collapsed = TRUE)) 
    
    leafletProxy("double_map_1", 
                 data= st_transform(subset(pak_shp, year==d_c1()$year), 
                                    crs=4326)) %>%
      clearControls() %>%
      addLegend("bottomright",
                pal= pal1(),
                values= ~d_c1()$value,
                title = "Legend",
                opacity= 1,
                labFormat = labelFormat(
                  between = "  :  ",
                  digits = 2))
 
  })
  
  #User's data selection for map 2    
  d_c2 <- reactive({
    data %>% 
      filter(indicator == input$stat_c2,
             year_1 == input$time_c2)
  })
  
  #Map 2 reactive function   
  
  #Lealfet
  output$double_map_2 <- renderLeaflet({
    # message("rendering local map")
    leaflet(options = leafletOptions(zoomSnap = 0.20, zoomDelta = 0.20)) %>%
      addProviderTiles(providers$CartoDB, group = "CARTO") %>%
      setView(lng=69, lat = 30.5, zoom = 5)  %>% 
      syncWith("combined_map")
  })
  
  #To render leafelt map before proxy observer updates
  outputOptions(output, "double_map_2", suspendWhenHidden = FALSE)
  
  #Labels for Map 2    
  labels2 <- reactive({
    paste0(glue::glue("<b>District</b>: { pak_shp$district } </br>"), glue::glue("<b>Indicator 2: </b>"), " ", glue::glue("{ round(d_c2()$value, 2) }")," ", glue("{d_c2()$units}"), sep = "") %>% 
      lapply(htmltools::HTML)                                                                             
  })
  
  #Color scheme for map 2  
  pal_new_map2 <- reactive({
    req(unique(d_c2()$context) %in% c("negative", "positive"))
    if (unique(d_c2()$context) == "negative"){
      c('#FFEDA0', '#FED976', '#FEB24C', '#FD8D3C', '#FC4E2A','#E31A1C')
    } else {
      c('#E31A1C', '#FC4E2A','#FD8D3C', '#FEB24C', '#FED976', '#FFEDA0')
    }
    
  })  
  # map_2 <- reactive({
  pal2 <- reactive(colorBin(palette = pal_new_map2()  , 
                            bins= 6, 
                            na.color = "grey", 
                            domain= d_c2()$value))
  
  observe({
    req(input$stat_c2)
    leafletProxy("double_map_2", data= st_transform(subset(pak_shp, 
                                                           year==d_c2()$year), 
                                                    crs=4326)) %>% 
      clearShapes() %>% 
      addPolygons(label= labels2(),
                  labelOptions = labelOptions(
                    style = list("font-weight"= "normal",   
                                 padding= "3px 8px",
                                 "color"= "#cc4c02"), 
                    textsize= "15px",
                    direction = "auto"
                  ),
                  fillColor = ~pal2()(d_c2()$value),
                  fillOpacity = 0.7,
                  stroke = TRUE,
                  color= "white",
                  weight = 0.8,
                  opacity = 0.5,
                  fill= TRUE,
                  smoothFactor = 0.5,
                  highlightOptions = highlightOptions(weight= 5,
                                                      fillOpacity = 1,
                                                      opacity= 1,
                                                      bringToFront = TRUE), 
                  group = "Polygons")

    leafletProxy("double_map_2",
                 data= st_transform(subset(pak_shp, year==d_c2()$year), 
                                    crs=4326)) %>%
      clearControls() %>%
      addLegend("bottomright",
                pal= pal2(),
                values= d_c2()$value,
                title = "Legend",
                opacity= 1,
                labFormat = labelFormat(
                  between = "  :  ",
                  digits = 2))
    
  })
  
  #Rendering synced maps in comparison Tab
  
  output$double <- renderUI({
    leafsync::sync(map_1(), map_2(),ncol= 2, no.initial.sync = TRUE)
  })
  
  #Graphs Tab
  
  #Selecting domain     
  d_g <- reactive({
    data %>% 
      filter(domain == input$family2)
  })
  
  #Updating indicators based on domain
  observeEvent(d_g(),{
    choices <- unique(d_g()$indicator)
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "stat2", 
      choices = choices)
  })
  
  #selecting indicator
  d_g1 <- reactive({
    data %>% 
      filter(indicator == input$stat2)
  })
  
  #Updating years based on indicators
  observeEvent(d_g1(), {                                 
    updated_years <- d_g1() %>% 
      filter(!is.na(value))
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "time2", 
      choices = sort(unique(updated_years$year_1), decreasing = T)
    )
  })
  
  #Selecting year  
  d_g2 <- reactive({
    d_g1() %>% 
      filter(year_1 == input$time2) 
  })
  
  #Updating province based on selected year  
  observeEvent(d_g2(), {                                 
    updated_prov_plot <- d_g2() %>% 
      filter(!is.na(value),
             province != "Federal Capital Territory")
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "prov2", 
      choices = unique(updated_prov_plot$province)
    )
  })
  
  #Graph reactive
  suppressWarnings(
  gr_1 <- function(){ 
    data %>% 
      filter(province == input$prov2,
             indicator == input$stat2,
             year_1 == input$time2) %>%
      filter(province != "Federal Capital Territory") %>% 
      filter(!str_detect(district, "FR"),
             !str_detect(district, "Agency")) %>% 
      mutate(district = fct_reorder(factor(district), 
                                    value, 
                                    .na_rm=F)) %>% 
      ggplot(aes(value, district)) +
      geom_col(na.rm= T)+
      geom_text(aes(label = round(value,1)),
                na.rm = T,
                alpha= 0.9, size=3.2,
                vjust=1, hjust = 1,
                nudge_y= 0.4,
                color="white")+
      labs(y= "", x = input$stat2)+
      labs(title = "")
    
  }
  )
  
  #Rendering graph   
  output$plot1 <- renderPlot({ 
    print(gr_1())
  })
  
  #Download grpah
  output$downloadplot1 <- downloadHandler(
    filename = function(){
      paste0("plot_", glue("{ input$stat2 }", "_", "{ input$time2 }"), ".png")
    },
    content = function(file){
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, gr_1(), device = device)
    }
    
  )
  
  # Plot 2 Time trend of indicators in selected districts
  #selecting domain
  d_tt0 <- reactive({
    data %>% 
      filter(domain == input$family_t)
  })
  #Updating indicators
  observeEvent(d_tt0(),{
    choices1 <- unique(d_tt0()$indicator)
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "stat_t", 
      choices = choices1)
  })    
  
  #Selecting indicator
  d_tt1 <- reactive({
    d_tt0() %>% 
      filter(indicator == input$stat_t)
  })
  
  #Updating provinces based on the indicator selection
  observeEvent(d_tt1(),{
    updated_prov_t <-  d_tt1() %>% 
      select(-domain,-indicator_1, -units, -source, -definition, -positive, -negative, -context) %>%
      filter(!is.na(value),
             province != "Federal Capital Territory") %>% 
      distinct(province, year) %>% 
      group_by(province) %>% 
      filter(n()>=2) %>% 
      select(-year)
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "prov_t", 
      choices = updated_prov_t)
  })     
  
  #Time series chart function
  suppressWarnings(
  gr_2 <- function(){
    req(input$dist_t)
    data %>% 
      filter(domain == input$family_t,
             indicator == input$stat_t,
             district %in% input$dist_t) %>%
      filter(province != "Federal Capital Territory") %>% 
      filter(!str_detect(district, "FR"),
             !str_detect(district, "Agency"),
             indicator!= "Population Census",
             !is.na(value)) %>% 
      ggplot(aes(year, value, color = district)) +
      geom_line(linewidth= 0.75) +
      geom_point(size=1.25) +
      expand_limits(y=0)+
      labs(x= "Years", y= input$stat_t,
           color= "District")
  }
  )
  
  #Rendering chart time series
  output$plot2 <- renderPlotly({
    gr_2()
  })
  
  #Scatter Plot
  
  # User first choose year, and only those indicators show up in both indicator filters, for which data is available
  # So that empty plots in few cases won't show up
  
  #Selecting Year
  
  d_s0 <- reactive({
    data %>% 
      filter(year == input$time_s) %>% 
      arrange(desc(year))
  })
  
  #Updatinf indicator 1
  observeEvent(d_s0(),{
    updated_indicator1 <- d_s0() %>% 
      filter(!is.na(value))
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "stat_s1",
      choices = unique(updated_indicator1$indicator)
    )
  })
  
  #Updating Indicator 2
  
  observeEvent(d_s0(),{
    updated_indicator2 <- d_s0() %>% 
      filter(!is.na(value))
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "stat_s2",
      choices = unique(updated_indicator2$indicator)
    )
  })
  
  #Scatter plot function
  suppressWarnings(
  gr_3 <- function(){
    data %>% 
      select(-domain, -source, -definition, -units, -indicator_1,-year_1, -positive, -negative, -context) %>% 
      filter(
        # province == input$prov_s,
        year == input$time_s,
        province != "Federal Capital Territory",
        !str_detect(district, "FR"),
        !str_detect(district, "Agency")) %>% 
      filter(indicator %in%  c(input$stat_s1,input$stat_s2)) %>%
      pivot_wider(names_from =  indicator,values_from =   value) %>% 
      ggplot(aes(.data[[input$stat_s1]], .data[[input$stat_s2]]))+
      geom_point(na.rm=TRUE, size= 1) +
      geom_smooth(na.rm=TRUE, method = "lm") +
      scale_x_continuous(labels = scales::number_format() )+
      scale_y_continuous(labels = scales::number_format())+
      labs(title = glue("Year", " ", "{ input$time_s }:", " ", "Districts for Selected Indicators"),
           subtitle = "Linear Model",
           x=input$stat_s1,
           y=input$stat_s2,
           color="Province")
  }
  )
  
  #Rendering scatter plot
  output$plot3 <- renderPlot({
    print(gr_3())
  })

  
  #Download scatter plot
  output$downloadplot3 <- downloadHandler(
    filename = function(){
      paste0("plot_", glue("{ input$stat_s1 }","_", "{ input$stat_s2 }", "_", "{ input$time_s }"), ".png")
    },
    content = function(file){
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, gr_3(), device = device)
    }
    
  )
  
  #When province disaggregatio tab is clicked
  
  observeEvent(input$prov_s,{
    gr_3p <- function(){
      data %>% 
        select(-domain, -source, -definition, -units, -indicator_1,-year_1, -negative, -positive, -context) %>% 
        filter(
          # province == input$prov_s,
          year == input$time_s,
          province != "Federal Capital Territory",
          !str_detect(district, "FR"),
          !str_detect(district, "Agency")) %>% 
        filter(indicator %in%  c(input$stat_s1,input$stat_s2)) %>%
        pivot_wider(names_from =  indicator, values_from =  value) %>% 
        ggplot(aes(.data[[input$stat_s1]], .data[[input$stat_s2]], 
                   color = province))+
        geom_point(na.rm=TRUE, size= 1) +
        # geom_text_repel(aes(label = district), na.rm = TRUE, max.overlaps = 5) +
        geom_smooth(na.rm = TRUE, method = "lm") +
        scale_x_continuous(labels = scales::number_format() )+
        scale_y_continuous(labels = scales::number_format())+
        labs(title = glue("Year", " ", "{ input$time_s }:", " ", "Districts for Selected Indicators"),
             subtitle = "Linear Model",
             x=input$stat_s1,
             y=input$stat_s2,
             color= "Province")
    }
    
    #Rendering new plot afterwards    
    output$plot3 <- renderPlot({
      print(gr_3p())
    })
    
    #Download new scatterplot after provincial disaggregation  
    output$downloadplot3 <- downloadHandler(
      filename = function(){
        paste0("plot_", glue("{ input$stat_s1 }","_", "{ input$stat_s2 }", "_", "{ input$time_s }"), ".png")
      },
      content = function(file){
        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, gr_3p(), device = device)
      }
      
    )
    
  })
  
  
  #Table
  #Selected domain
  d_t <- reactive({
    data %>% 
      filter(domain == input$family1)
  })
  
  #Updating indicators based on that
  observeEvent(d_t(),{
    choices <- unique(d_t()$indicator)
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "stat1", 
      choices = choices)
  })
  
  #selecting indicator
  d_t1 <- reactive({
    data %>% 
      filter(indicator == input$stat1)
  })
  
  #Updating years based on the previous selection
  observeEvent(d_t1(), {                                 
    updated_years_t <- d_t1() %>% 
      filter(!is.na(value))
    updateSelectizeInput(
      session = getDefaultReactiveDomain(),
      "time1", 
      choices = sort(unique(updated_years_t$year_1), decreasing = T)
    )
  })
  
  #making reactive dataset 
  d2 <- reactive({
    d_t1() %>% 
      filter(indicator == input$stat1,
             year_1 == input$time1,
             province == input$prov1)
  })
  
  #making reactive dataset
  d2_2 <- reactive({
    d_t1() %>% 
      filter(indicator == input$stat1,
             year_1 == input$time1)  
  })
  
  #Table specification
  table1 <- reactive({
    d2() %>% 
      select(-domain, -source, -definition, -units, -indicator_1, -year_1, -positive, -negative, -context) %>% 
      arrange(province, district) %>% 
      rename(Province = province,
             District = district,
             Indicator = indicator,
             Value = value)
  }) 
  
  #Rendering table
  output$table1 <- renderDataTable({
    DT::datatable(table1(), 
                  options= list(pageLength=10))
  })
  
  #For national level table, for 1 go download
  observeEvent(input$national,{
    table2 <- reactive({
      d2_2() %>% 
        select(-domain, -source, -definition, -units, -indicator_1, -year_1, -positive, -negative, -context) %>% 
        arrange(province, district) %>% 
        rename(Province = province,
               District = district,
               Indicator = indicator,
               Value = value)
    })
    #rendering national level table  
    output$table1 <- renderDataTable({
      table2()
    })
    #Download national level table  
    output$downloadtable <- downloadHandler(
      filename = function(){
        paste0("table_", glue("{ input$stat1 }", "_", "{ input$time1 }"), ".csv")
      },
      content = function(file){
        write.csv(table2(), file)
      }
      
    )
  })
  
  #download main table
  output$downloadtable <- downloadHandler(
    filename = function(){
      paste0("table_", glue("{ input$stat1 }", "_", "{ input$time1 }"), ".csv")
    },
    content = function(file){
      write.csv(table1(), file)
    }
    
  )
  
  #For Source  on Main Map
  src <- reactive({
    data %>% 
      filter(indicator == input$stat) %>% 
      select(source)
  })   
  
  src1 <- reactive({
    data %>% 
      filter(indicator == input$stat) %>% 
      select(definition)
  })
  output$source2 <- renderText({
    paste0("Source: ",unique(src()$source),"\n", "Definition: ",unique(src1()$definition))
  })
  
  #For sources on comparison maps
  src_c1 <- reactive({
    data %>% 
      filter(indicator == input$stat_c1) %>% 
      select(source)
  })   
  src_c2 <- reactive({
    data %>% 
      filter(indicator == input$stat_c1) %>% 
      select(definition)
  })   
  
  output$source_c1 <- renderText({
    paste0("MAP 1", "\n", "Source 1: ",unique(src_c1()$source),"\n", "Definition 1: ",unique(src_c2()$definition))
  })
  
  src_c3 <- reactive({
    data %>% 
      filter(indicator == input$stat_c2) %>% 
      select(source)
  })   
  src_c4 <- reactive({
    data %>% 
      filter(indicator == input$stat_c2) %>% 
      select(definition)
  })   
  
  output$source_c2 <- renderText({
    paste0("MAP 2", "\n", "Source 2: ",unique(src_c3()$source),"\n", "Definition 2: ",unique(src_c4()$definition))
  })
  
  #For Source on the table 
  src_t1 <- reactive({
    data %>% 
      filter(indicator == input$stat1) %>% 
      select(source)
  })   
  src_t2 <- reactive({
    data %>% 
      filter(indicator == input$stat1) %>% 
      select(definition)
  })     
  
  
  output$table_note <- renderText({
    paste0("Source: ",unique(src_t1()$source),"\n", "Definition: ",unique(src_t2()$definition))
  })
  
  #Source on the  Graph
  src_g1 <- reactive({
    data %>% 
      filter(indicator == input$stat2) %>% 
      select(source)
  })   
  src_g2 <- reactive({
    data %>% 
      filter(indicator == input$stat2) %>% 
      select(definition)
  })     
  
  output$source_graph <- renderText({
    paste0("Source: ",unique(src_g1()$source),"\n", "Definition: ",unique(src_g2()$definition))
  })  
  
  #source for the timeseries graph
  src_gt1 <- reactive({
    data %>% 
      filter(indicator == input$stat_t) %>% 
      select(source)
  })   
  src_gt2 <- reactive({
    data %>% 
      filter(indicator == input$stat_t) %>% 
      select(definition)
  })  
  output$source_timeseries <- renderText({
    paste0("Source: ",unique(src_gt1()$source),"\n", "Definition: ",unique(src_gt2()$definition))
    
  })
  
  #Source for comparison graphs
  src_gs1 <- reactive({
    data %>% 
      filter(indicator == input$stat_s1) %>% 
      select(source)
  })   
  src_gs2 <- reactive({
    data %>% 
      filter(indicator == input$stat_s1) %>% 
      select(definition)
  })  
  output$source_compgraph_1 <- renderText({
    paste0("Indicator 1:", "\n", "Source: ",unique(src_gs1()$source),"\n", "Definition: ",unique(src_gs2()$definition))
    
  })
  
  src_gs3 <- reactive({
    data %>% 
      filter(indicator == input$stat_s2) %>% 
      select(source)
  })   
  src_gs4 <- reactive({
    data %>% 
      filter(indicator == input$stat_s2) %>% 
      select(definition)
  })  
  output$source_compgraph_2 <- renderText({
    paste0("Indicator 2", "\n", "Source: ",unique(src_gs3()$source),"\n", "Definition: ",unique(src_gs4()$definition))
    
  })
  
  
  # *****************************************************************    
  #to bulk download input dataset in csv
  #First converting data back to original wider format. Moreover, unnecessary variables have been removed
  
  data_bulk <- reactive({
    # req(input$bulkdownload)
    data %>% 
      pivot_wider(names_from = indicator, values_from = value, province:year)
  })
  
  output$bulkdownload <- downloadHandler(
    filename = function(){
      paste0("pakistan_indicator_dataset", ".csv")
    },
    content = function(file){
      write.csv(data_bulk(), file)
    }
    
  )
  # *********************************************
  #to Download shapefile in zip
  
  output$shapefile <- downloadHandler(
    filename = "Pakistanshape.zip",
    content = function(file){
      if(length(Sys.glob("pakistan_shape.*"))>0){
        file.remove(Sys.glob("pakistan_shape.*"))
      }
      st_write(pak_shp, dsn = "pakistan_shape.shp", layer= "pakistan_shape" ,driver= "ESRI Shapefile", overwrite_layer = T)

      zip(zipfile = 'Pakistanshape.zip', files= Sys.glob("pakistan_shape.*"))
      file.copy("Pakistanshape.zip", file)
      if(length(Sys.glob("pakistan_shape.*"))>0){
        file.remove(Sys.glob("pakistan_shape.*"))
      }
    }
  )
  
  #Download dataset glossary in pdf
  output$glossary <- downloadHandler(
    filename = "Glossary.pdf",
    content = function(file){
      file.copy("www/definitions.pdf", file)
    }
  )
  
  
}

#run app
shinyApp(ui, server, enableBookmarking= "url")

