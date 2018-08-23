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

defaultDates <- format(seq(as.Date("2018/01/29"),as.Date("2018/08/15"), by = "day"),"%Y%m%d")

# AllData <- readRDS(file = "Jan29ToAug152018.rds") %>% mutate(date = as.Date(paste0(year, month, day), format = "%Y%m%d"))

SmokeByDay <- readRDS("SmokeByDay.RDS")

vuln <- readRDS("vuln.rds")

# SmokeDaysByTract <- readRDS("Jan29ToAug152018_wide.rds")

CAtracts <-  st_read(dsn = "tractsSM.GeoJSON", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
    mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY)))

ui <- dashboardPage(
    dashboardHeader(title = "Wildfire Smoke"),
    dashboardSidebar(sidebarMenu(
      menuItem("Background", tabName = "back", icon = icon("th")),
        menuItem(
            "Wildfire Smoke",
            tabName = "smokeDash",
            icon = icon("dashboard")
        )

    )),
    dashboardBody(tabItems(
        tabItem(tabName = "smokeDash",
                
                wellPanel(plotOutput("TimePlot")),
                
                fluidRow(
                    box(
                        title = "California Wildfire Smoke Expousres",
                        collapsible = TRUE,
                        collapsed = F,
                        leafletOutput("HMLdaysMap", height = 700)
                    ),
                    box(
                      title = "Time Window",
                          collapsible = T,
                          collapsed = F,
                          radioButtons(inputId = "radioTime",
                                       label = "Choose Time to examine in the Map",
                                       choices = c("last 2 weeks", "last 30 days","last 2 months") 
                                      )
                      
                    ),
                    
                    # box(
                    #     title = "Time Window",
                    #     collapsible = T,
                    #     collapsed = F,
                    #     dateRangeInput(
                    #         inputId = "Time",
                    #         label = "Select Window",
                    #         min = as.Date(defaultDates[1],"%Y%m%d"),
                    #         max = as.Date(defaultDates[length(defaultDates)],"%Y%m%d"),
                    #         start = as.Date("20180601","%Y%m%d"),
                    #         end = as.Date("20180813","%Y%m%d"),
                    #         separator = " to ", 
                    #         format = )
                    # ),

                    # box(title = "Smoke Category (view it on the map)",
                    #     collapsible = T,
                    #     collapsed = F,
                    #     radioButtons(
                    #         inputId = "Smoke",
                    #         label = "Smoke Density",
                    #         choices = c("Heavy", "Medium", "Light")
                    #     )
                    # ),
                box(title = "Person-Days of Exposure by Race and Age Vulnerabilities",
                    plotOutput('VulnPlot')
                )
                
                
              )
        ),
                
                
        
        
        # second Tab Content
        tabItem(tabName = "back",
                HTML("<h4>The Wildfire Smoke data on this dashboard comes from the Nation Oceanic and Atmospheric Administration's Office of Satellite and Product Operations. </h4>
<h6>The NOAA Smoke and Fire Products can be found <a href='https://www.ospo.noaa.gov/Products/land/hms.html' target='_blank'> at their website</a>. </h6> 
<ul>
<li>The initial HMS product for the current day is created and updated by a satellite analyst roughly between 8am and 10am Eastern Time. After 10am, the analysis is fine-tuned as time permits as additional satellite data becomes available. Areas of smoke are analyzed and added to the analysis during daylight hours as visible satellite imagery becomes available. The product is finalized and 'completed' for the archive the following morning - generally by around 800am.</li>
<li>The fire sizes depicted in the product are primarily determined by the field of view of the satellite instrument, or the resolution of the analysis tool. They should not be used to estimate specific fire perimeters.</li>
<li>The ability to detect fires and smoke can be compromised by many factors, including cloud cover, tree canopy, terrain, the size of the fire or smoke plume, the time of the day, etc. The satellite sensors used to detect fires are sensitive to heat sources and reflected sunlight. Analysts do their best to distinguish between fires and other heat sources or highly reflective surfaces, such as factories, mines, gas flares, solar panels, clouds, etc. but some false detects do get included in the analysis.</li></ul> 
                     
                     <h5>View the latest Fire and Smoke in Goolge Earth </h5>
                     <ul>
                     <li><a href='https://www.ospo.noaa.gov/data/land/fire/fire.kml'>Fire</a></li>
                     <li><a href='https://www.ospo.noaa.gov/data/land/fire/smoke.kml'>Smoke</a></li>
                     </ul><p>Click on the Wildfire Smoke item in the Sidebar to see more.</p>")
                # , 
                # textOutput("text"),
                # tableOutput("table")
                # 
                ))
    ))


server <- function(input, output) { 

    # dateList <- reactive({
    #    format(seq(as.Date(input$Time[1]),as.Date(input$Time[2]), by = "day"),"%Y%m%d")
    # 
    # })
    # 
    
    # SmokeDaysByTract <- reactive({
    #     AllData %>%
    #         filter(
    #             as.Date(date) >= as.character(input$Time[1]) &
    #                 as.Date(date) <= as.character(input$Time[2])
    #         ) %>%
    #     mutate(smoke = factor(ifelse(smoke == "Smoke (Light)", "light", 
    #                                  ifelse(smoke == "Smoke (Heavy)","heavy", "medium")), levels=c("light","medium","heavy"))) %>%   
    #     group_by(smoke, ct10) %>%
    #     summarize(NumberOfDays = length(unique(date))) %>%
    #     ungroup() %>% 
    #     spread(key = smoke,value = NumberOfDays) 
    # })
  

    
  SmokeDaysByTract <- reactive({
    if(input$radioTime == "last 2 weeks"){readRDS("last2weeks.RDS")}
    
    else if(input$radioTime == "last 30 days"){readRDS("last30days.RDS")}
    
    else {readRDS("last2months.RDS")}
    
  })
  
  
  numDays <- reactive({
    if(input$radioTime == "last 2 weeks"){14}
    
    else if(input$radioTime == "last 30 days"){30}
    
    else {60}
    
  })
  
    
    # SmokeByDay <- 
    #   # reactive ({
    #   AllData %>% 
    #   # filter(
    #   #   as.Date(date) >= as.character(input$Time[1]) &
    #   #     as.Date(date) <= as.character(input$Time[2])
    #   # ) %>%
    #   mutate(smoke = factor(ifelse(smoke == "Smoke (Light)", "light", 
    #                                ifelse(smoke == "Smoke (Heavy)","heavy", "medium")), levels=c("light","medium","heavy"))) %>%
    #   select(smoke, ct10, date) %>% inner_join({
    #     vuln %>% 
    #       mutate(ct10 = paste0("0",as.character(as.numeric(ct10)))) %>%
    #       filter(race == "Total") %>%
    #       spread(key = cohort, value = population) %>%
    #       select(-race, )
    #   }) %>%
    #   group_by(ct10, date, smoke) %>%
    #   summarize(Persons = mean(total, na.rm=T)) %>%
    #   group_by(smoke, date) %>%
    #   summarize(Persons = sum(Persons, na.rm=T))
    # # })
   
    mapTemp <- reactive({
        CAtracts %>% inner_join(SmokeDaysByTract())
    })
    
    
 ############ MAP ###############################   
  #################### alternative mapping with all three layers in the single map ######################
    output$HMLdaysMap <- renderLeaflet({

        pal_l <- colorBin(palette =  "Reds",
                          bins = 7,
                          domain = na.exclude(SmokeDaysByTract()$light))

        pal_m <- colorBin(palette =  "Reds",
                          bins = 7,
                          domain = na.exclude(SmokeDaysByTract()$medium))

        pal_h <- colorBin(palette =  "Reds",
                          bins = 7,
                          domain = na.exclude(SmokeDaysByTract()$heavy))


        mapTemp() %>%
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
                    mapTemp()$light,
                    " days (",
                    round(100 * mapTemp()$light / numDays(), 1),
                    "%) of Light Smoke Exposure."
                ),
                group = "Light Smoke"
            ) %>%
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
                    mapTemp()$medium,
                    " days (",
                    round(100 * mapTemp()$medium / numDays(), 1),
                    "%) of Medium Smoke Exposure."
                ),
                group = "Medium Smoke"
            ) %>%
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
                    mapTemp()$heavy,
                    " days (",
                    round(100 * mapTemp()$heavy / numDays(), 1),
                    "%) of Heavy Smoke Exposure."
                ),
                group = "Heavy Smoke"
            ) %>%
            addLayersControl(
                baseGroups = c("Heavy Smoke", "Medium Smoke", "Light Smoke"),
                options = layersControlOptions(collapsed = TRUE)
            )



    })
    
    
    ################### mapping #######################
    # output$HMLdaysMap <- renderLeaflet({
    #     
    # 
    #     if(input$Smoke == "Light"){
    # 
    #         pal_l <- colorBin(palette =  "Reds",
    #                           bins = 7,
    #                           domain = na.exclude(SmokeDaysByTract()$light))
    #         
    #         
    #         mapTemp() %>%
    #             leaflet()  %>%
    #             addProviderTiles(providers$CartoDB.Positron) %>%
    #             addPolygons(
    #                 color = "#444444",
    #                 weight = 0.3,
    #                 smoothFactor = 0.1,
    #                 fillOpacity = 0.6,
    #                 fillColor = ~ pal_l(light),
    #                 highlightOptions = highlightOptions(
    #                     color = "white",
    #                     weight = 2,
    #                     bringToFront = TRUE
    #                 ),
    #                 popup = paste0(
    #                     "This tract has experienced ",
    #                     mapTemp()$light,
    #                     " days (",
    #                     round(100 * mapTemp()$light/length(dateList()), 1),
    #                     "%) of Light Smoke Exposure."
    #                 ),
    #                 group = "Light Smoke"
    #             )
    #         
    # 
    #     } 
    #     else if(input$Smoke == "Medium"){
    # 
    #         pal_m <- colorBin(palette =  "Reds",
    #                           bins = 7,
    #                           domain = na.exclude(SmokeDaysByTract()$medium))
    # 
    #         mapTemp() %>%
    #             leaflet()  %>%
    #             addProviderTiles(providers$CartoDB.Positron) %>%
    #             addPolygons(
    #                 color = "#444444",
    #                 weight = 0.3,
    #                 smoothFactor = 0.1,
    #                 fillOpacity = 0.6,
    #                 fillColor = ~ pal_m(medium),
    #                 highlightOptions = highlightOptions(
    #                     color = "white",
    #                     weight = 2,
    #                     bringToFront = TRUE
    #                 ),
    #                 popup = paste0(
    #                     "This tract has experienced ",
    #                     mapTemp()$medium,
    #                     " days (",
    #                     round(100 * mapTemp()$medium / length(dateList()), 1),
    #                     "%) of Medium Smoke Exposure."
    #                 ),
    #                 group = "Medium Smoke"
    #             )
    #     }
    #     else{
    # 
    #         pal_h <- colorBin(palette =  "Reds",
    #                           bins = 7,
    #                           domain = na.exclude(SmokeDaysByTract()$heavy))
    #         
    #         mapTemp() %>%
    #             leaflet()  %>%
    #             addProviderTiles(providers$CartoDB.Positron) %>%
    #             addPolygons(
    #                 color = "#444444",
    #                 weight = 0.3,
    #                 smoothFactor = 0.1,
    #                 fillOpacity = 0.6,
    #                 fillColor = ~ pal_h(heavy),
    #                 highlightOptions = highlightOptions(
    #                     color = "white",
    #                     weight = 2,
    #                     bringToFront = TRUE
    #                 ),
    #                 popup = paste0(
    #                     "This tract has experienced ",
    #                     mapTemp()$heavy,
    #                     " days (",
    #                     round(100 * mapTemp()$heavy / length(dateList()), 1),
    #                     "%) of Heavy Smoke Exposure."
    #                 ),
    #                 group = "Heavy Smoke"
    #                 
    #             )
    #         
    #     }
    # 
    # })

    
############### PLOT ############################   
    
    plotData <- reactive({
       
      vuln %>%
        mutate(ct10 = paste0("0",as.character(as.numeric(ct10)))) %>%
        inner_join(SmokeDaysByTract()) %>% 
        mutate(lightPD = population * light,
               mediumPD = population * medium,
               heavyPD = population * heavy) %>%
        filter(cohort != "total" & cohort != "other" &  race != "Total") %>%
        group_by(cohort, race) %>%
        summarize(Heavy = sum(heavyPD, na.rm = T),
                  Medium = sum(mediumPD, na.rm = T),
                  Light = sum(lightPD, na.rm = T)) %>% ungroup() %>%
        gather(Heavy, Medium, Light, key = SmokeDensity, value = PersonDays) %>%
        mutate(SmokeDensity = factor(SmokeDensity, levels = c("Light","Medium","Heavy")))
        
       
   })
   
   
   output$VulnPlot <- renderPlot({
       
       plotData() %>% ggplot(aes(x=cohort, y= PersonDays/1000000, fill = SmokeDensity)) + geom_bar(stat="identity", position ="dodge") + facet_wrap(~ race , scales="free_y") + ylab("Million Person-Days")
       
   })
   
   
   
   
   ##################### TIme Series plot #################################
   
   output$TimePlot <- renderPlot({
   
   SmokeByDay %>% ggplot() + 
       geom_area(aes(x = date, y= Persons/1000000, fill = smoke), position = "dodge") + 
       ylab("Millions of People") + xlab("Date") +  ggtitle("2018 Wildfire Smoke Exposure for California") + 
       geom_hline(yintercept = 37.25, color = "red", linetype="dashed") + 
       geom_label(aes(x = as.Date("2018-05-01"),y = 37.25, label="Total CA Population")) + 
       geom_vline(xintercept = as.Date("2018-07-27"), color = "gray20", linetype="twodash") + 
       geom_label(aes(x = as.Date("2018-07-27"),y = 35, label="Mendocino Complex Start", hjust="right")) +
       geom_vline(xintercept = as.Date("2018-07-23"), color = "gray20", linetype="dotdash") + 
       geom_label(aes(x = as.Date("2018-07-23"),y = 32, label="Carr Fire Start", hjust="right")) +
       geom_vline(xintercept = as.Date("2018-07-13"), color = "gray20", linetype="longdash") + 
       geom_label(aes(x = as.Date("2018-07-13"),y = 29, label="Ferguson Fire Start", hjust="right"))
   })
   
   
   ################# random outputs to check some things #############
    # 
    # output$table <- renderTable(SmokeDaysByTract())
    # 
    # output$text <- renderText(paste0("number of days in the period you selected: ",length(dateList())))
    # 
    
    }

shinyApp(ui, server)