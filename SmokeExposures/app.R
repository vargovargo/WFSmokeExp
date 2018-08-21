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
library(tidyverse)
library(leaflet)
library(sf)

dateList <- format(seq(as.Date("2018/01/29"),as.Date("2018/08/15"), by = "day"),"%Y%m%d")

foo2 <- readRDS("Jan29ToAug152018_wide.rds")

vuln <- readRDS("vuln.rds")

CAtracts <-  st_read(dsn = "tractsSM.GeoJSON", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
    mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY)))



ui <- dashboardPage(
    dashboardHeader(title = "Califronia Wildfire Smoke Exposure Tracking"),
    dashboardSidebar(sidebarMenu(
        menuItem(
            "Dashboard",
            tabName = "dashboard",
            icon = icon("dashboard")
        ),
        menuItem("Raw Data", tabName = "RawData", icon = icon("th"))
    )),
    dashboardBody(tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                    box(title = "Smoke Category",
                        collapsible = T,
                        collapsed = T,
                        radioButtons(
                            inputId = "Smoke",
                            label = "Smoke Density",
                            choices = c("Heavy", "Medium", "Light")
                        )
                    ),
                    box(
                        title = "Time Window",
                        collapsible = T,
                        collapsed = T,
                        dateRangeInput(
                            inputId = "Time",
                            label = "Select Window",
                            min = dateList[1],
                            max = dateList[length(dateList)],
                            start = "20180701",
                            end = "20180813",
                            separator = " to ")
                    ),
                    box(
                        title = "CA Wildfire Smoke Expousres",
                        collapsible = TRUE,
                        collapsed = F,
                        leafletOutput("HMLdaysMap", height = 700)
                    ),
                box(title = "Person-Days of Exposure by Race and Age Vulnerabilities",
                    plotOutput('VulnPlot')
                )
                
                
                )
                ),
        
        
        # second Tab Content
        tabItem(tabName = "RawData",
                HTML("<h5>This data comes from the Nation Oceanic and Atmosphereic Administration's Office of Satellite and Product Operations. </h5>
<ul>
<li>The initial HMS product for the current day is created and updated by a satellite analyst roughly between 8am and 10am Eastern Time. After 10am, the analysis is fine-tuned as time permits as additional satellite data becomes available. Areas of smoke are analyzed and added to the analysis during daylight hours as visible satellite imagery becomes available. The product is finalized and 'completed' for the archive the following morning - generally by around 800am.</li>
<li>The fire sizes depicted in the product are primarily determined by the field of view of the satellite instrument, or the resolution of the analysis tool. They should not be used to estimate specific fire perimeters.</li>
<li>The ability to detect fires and smoke can be compromised by many factors, including cloud cover, tree canopy, terrain, the size of the fire or smoke plume, the time of the day, etc. The satellite sensors used to detect fires are sensitive to heat sources and reflected sunlight. Analysts do their best to distinguish between fires and other heat sources or highly reflective surfaces, such as factories, mines, gas flares, solar panels, clouds, etc. but some false detects do get included in the analysis.</li></ul> ")
                
                ))
    ))


server <- function(input, output) { 

    
    
    # output$HMLdaysMap <- renderLeaflet({
    #     
    #     pal_l <- colorBin(palette =  "Reds",
    #                       bins = 7,
    #                       domain = na.exclude(foo2$light))
    #     
    #     pal_m <- colorBin(palette =  "Reds",
    #                       bins = 7,
    #                       domain = na.exclude(foo2$medium))
    #     
    #     pal_h <- colorBin(palette =  "Reds",
    #                       bins = 7,
    #                       domain = na.exclude(foo2$heavy))
    #     
    #     
    #     mapTemp <- CAtracts %>% inner_join(foo2)
    #     
    #     
    #     mapTemp %>%
    #         leaflet()  %>%
    #         addProviderTiles(providers$CartoDB.Positron) %>%
    #         addPolygons(
    #             color = "#444444",
    #             weight = 0.3,
    #             smoothFactor = 0.1,
    #             fillOpacity = 0.6,
    #             fillColor = ~ pal_l(light),
    #             highlightOptions = highlightOptions(
    #                 color = "white",
    #                 weight = 2,
    #                 bringToFront = TRUE
    #             ),
    #             popup = paste0(
    #                 "This tract has experienced ",
    #                 mapTemp$light,
    #                 " days (",
    #                 round(100 * mapTemp$light / 199, 1),
    #                 "%) of Light Smoke Exposure."
    #             ),
    #             group = "Light Smoke"
    #         ) %>%
    #         addPolygons(
    #             color = "#444444",
    #             weight = 0.3,
    #             smoothFactor = 0.1,
    #             fillOpacity = 0.6,
    #             fillColor = ~ pal_m(medium),
    #             highlightOptions = highlightOptions(
    #                 color = "white",
    #                 weight = 2,
    #                 bringToFront = TRUE
    #             ),
    #             popup = paste0(
    #                 "This tract has experienced ",
    #                 mapTemp$medium,
    #                 " days (",
    #                 round(100 * mapTemp$medium / 199, 1),
    #                 "%) of Medium Smoke Exposure."
    #             ),
    #             group = "Medium Smoke"
    #         ) %>%
    #         addPolygons(
    #             color = "#444444",
    #             weight = 0.3,
    #             smoothFactor = 0.1,
    #             fillOpacity = 0.6,
    #             fillColor = ~ pal_h(heavy),
    #             highlightOptions = highlightOptions(
    #                 color = "white",
    #                 weight = 2,
    #                 bringToFront = TRUE
    #             ),
    #             popup = paste0(
    #                 "This tract has experienced ",
    #                 mapTemp$heavy,
    #                 " days (",
    #                 round(100 * mapTemp$heavy / 199, 1),
    #                 "%) of Heavy Smoke Exposure."
    #             ),
    #             group = "Heavy Smoke"
    #         ) %>%
    #         addLayersControl(
    #             baseGroups = c("Heavy Smoke", "Medium Smoke", "Light Smoke"),
    #             options = layersControlOptions(collapsed = TRUE)
    #         )
    #     
    #     
    #     
    # })
    
    
    ################### mapping #######################
    output$HMLdaysMap <- renderLeaflet({
        mapTemp <- CAtracts %>% inner_join(foo2)

        if(input$Smoke == "Light"){

            pal_l <- colorBin(palette =  "Reds",
                              bins = 7,
                              domain = na.exclude(foo2$light))
            
            
            mapTemp %>%
                leaflet()  %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(
                    color = "#444444",
                    weight = 0.3,
                    smoothFactor = 0.1,
                    fillOpacity = 0.6,
                    fillColor = ~ pal_l(light),
                    highlightOptions = highlightOptions(
                        color = "white",
                        weight = 2,
                        bringToFront = TRUE
                    ),
                    popup = paste0(
                        "This tract has experienced ",
                        mapTemp$light,
                        " days (",
                        round(100 * mapTemp$light / 199, 1),
                        "%) of Light Smoke Exposure."
                    ),
                    group = "Light Smoke"
                )
            

        } 
        else if(input$Smoke == "Medium"){

            pal_m <- colorBin(palette =  "Reds",
                              bins = 7,
                              domain = na.exclude(foo2$medium))

            mapTemp %>%
                leaflet()  %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(
                    color = "#444444",
                    weight = 0.3,
                    smoothFactor = 0.1,
                    fillOpacity = 0.6,
                    fillColor = ~ pal_m(medium),
                    highlightOptions = highlightOptions(
                        color = "white",
                        weight = 2,
                        bringToFront = TRUE
                    ),
                    popup = paste0(
                        "This tract has experienced ",
                        mapTemp$medium,
                        " days (",
                        round(100 * mapTemp$medium / 199, 1),
                        "%) of Medium Smoke Exposure."
                    ),
                    group = "Medium Smoke"
                )
        }
        else{

            pal_h <- colorBin(palette =  "Reds",
                              bins = 7,
                              domain = na.exclude(foo2$heavy))
            
            mapTemp %>%
                leaflet()  %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(
                    color = "#444444",
                    weight = 0.3,
                    smoothFactor = 0.1,
                    fillOpacity = 0.6,
                    fillColor = ~ pal_h(heavy),
                    highlightOptions = highlightOptions(
                        color = "white",
                        weight = 2,
                        bringToFront = TRUE
                    ),
                    popup = paste0(
                        "This tract has experienced ",
                        mapTemp$heavy,
                        " days (",
                        round(100 * mapTemp$heavy / 199, 1),
                        "%) of Heavy Smoke Exposure."
                    ),
                    group = "Heavy Smoke"
                    
                )
            
        }

    })
    
    
   output$plotData <- reactive({
       
       vuln %>%
           inner_join({mutate(foo2, ct10 = as.numeric(ct10))}) %>% 
           mutate(lightPD = population * light,
                  mediumPD = population * medium,
                  heavyPD = population * heavy) %>%
           filter(cohort != "total", race != "Total") %>%
           group_by(cohort, race) %>%
           summarize(Heavy = sum(heavyPD), 
                     Medium = sum(mediumPD),
                     Light = sum(lightPD)) %>% ungroup() %>%
           gather(Heavy, Medium, Light, key = SmokeDensity, value = PersonDays)
       
   })
   
   
   output$VulnPlot <- renderPlot({
       
       plot %>% ggplot(aes(x=race, y= PersonDays, fill = cohort)) + geom_bar(stat="identity") + facet_grid(.~SmokeDensity~., scales="free_y")
       
   })
    
    
    
    
    
    
    }

shinyApp(ui, server)