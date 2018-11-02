
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(plotly)
library(sf)
library(data.table)
# library(dygraphs)
# library(xts)
library(ggiraph)

# setwd("~/GitHub/WFSmokeExp/SmokeExposures/")
smoke <- fread("CAsmokeFile.csv")
smoke <- copy(smoke)
setkey(smoke, date, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)


drought <- fread("PDSItracts2010.csv")
drought <- copy(drought)
setkey(drought, date, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)

ca <- st_read("counties.geoJSON") %>% mutate(countyFIPS = as.integer(substr(COUNTYFI_1, 3,5)))

beginning <- as.character(min(unique(smoke$date))) # determine first day
end <- as.character(max(unique(smoke$date))) # determine last day


pop <- smoke[, .(popmean = mean(POPULATION, na.rm = T)), by = .(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)]
setkey(pop, STATEFP, COUNTYFP)

# CApop <- smoke[,.(BGpop = mean(POPULATION, na.rm = T)), keyby = .(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)] %>% .[,.(sum(BGpop))]

# CApop$V1 = 37.25396 million

tableList <- fread("CenPop2010_Mean_BG06.txt")

allBGs <- tableList[, geoIDer := paste0(STATEFP,"_",COUNTYFP,"_",TRACTCE,"_",BLKGRPCE)]
allBGs <- copy(allBGs)
setkey(allBGs, STATEFP, COUNTYFP, geoIDer)

popCentroidsBG <- st_as_sf(tableList, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

ui <- dashboardPage(
  dashboardHeader(title = "CA Climate Exposures Dashboard",titleWidth = 500),
  dashboardSidebar(sidebarMenu(
    menuItem("Background", tabName = "back", icon = icon("info")),
    menuItem(
      "Extreme Heat",
      tabName = "extremeHeat",
      icon = icon("thermometer-three-quarters")
    ),
    menuItem(
      "Wildfire Smoke",
      tabName = "smokeDash",
      icon = icon("fire")
    ),
    menuItem(
      "Drought",
      tabName = "droughtDash",
      icon = icon("tint")
    ),
    menuItem(
      "Policies / Actions",
      tabName = "actions",
      icon = icon("exclamation-circle")
    )
    
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "extremeHeat",
            HTML("<h4>Under Construction</h4>")),
    tabItem(
      tabName = "smokeDash",
      fluidRow(
        box(
          title = "Select a Time and Location",
          collapsible = T,
          collapsed = F,
          width = 3,
          dateRangeInput(
            width = "100%",
            inputId = "Time",
            label = "Select Time Period to View",
            min = as.Date(beginning, "%Y%m%d"),
            max = as.Date(end, "%Y%m%d"),
            start = as.Date("20180701", "%Y%m%d"),
            end = as.Date("20180820", "%Y%m%d"),
            separator = " to ",
            format =
          ),
          ggiraphOutput('cmap', height = "300px"),
          footer = "Displaying person-days of HEAVY smoke exposure"
        )
        ,
        box(
          title = textOutput("plotTitle"),
          solidHeader = T,
          collapsible = T,
          collapsed = F,
          width = 9,
        # dygraphOutput("dygraph"),
          plotlyOutput("VulnPlot")

        )
      ),
      fluidRow(
        valueBoxOutput("lightDaysBox",width = 4),
        valueBoxOutput("mediumDaysBox",width = 4),
        valueBoxOutput("heavyDaysBox",width = 4)
        
      ),  fluidRow(
        valueBoxOutput("lightPopBox",width = 4),
        valueBoxOutput("mediumPopBox",width = 4),
        valueBoxOutput("heavyPopBox",width = 4)
        
      )
      
      
      # fluidRow(
      #   box(
      #     title = "Table of Plotted Data",
      #     footer = "Table of Plotted Data",
      #     status = "info",
      #     collapsible = T,
      #     collapsed = T,
      #     width = "100%",
      #     dataTableOutput('table')
      #   )
      # ),
        
    ),
    
    
    
    
    # background Tab Content
    tabItem(
      tabName = "back",
      
      HTML(
        "<h2>This dashboard brings together information from the Nation Oceanic and Atmospheric Administration (NOAA) on wildfire smoke intensity and coverage (left) and information about California populations -- including populations most sensitive to smoke exposures -- (right), in order to get a more complete picture of wildfire smoke exposures in the state.</h2>
        <h4>Recent Supporting Evidence on Population Vulnerability to Wildfire Smoke Exposures</h4>
        <ul>
        <li><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6015400/pdf/JAH3-7-e007492.pdf'>Cardiovascular and Cerebrovascular Emergency Department Visits Associated With Wildfire Smoke Exposure in California in 2015</a></li>
        <li><a href='https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1002601'>The San Diego 2007 wildfires and Medi-Cal emergency department presentations, inpatient hospitalizations, and outpatient visits: An observational study of smoke exposure periods and a bidirectional case-crossover analysis</a></li>
        
        </ul>"
      ),
      
      # wild fire smoke product box
      box(
        title = "Wildfire Smoke Data",
        HTML(
          "<h4>The Wildfire Smoke data on this dashboard comes from the Nation Oceanic and Atmospheric Administration's Office of Satellite and Product Operations.  Smoke data is part of the Office's <a href='https://www.ospo.noaa.gov/Products/land/hms.html' target='_blank'>Hazard Mapping System (HMS) Fire and Smoke Product</a></h4>
          
          <ul>
          <li>The initial HMS product for the current day is created and updated by a satellite analyst roughly between 8am and 10am Eastern Time. After 10am, the analysis is fine-tuned as time permits as additional satellite data becomes available. Areas of smoke are analyzed and added to the analysis during daylight hours as visible satellite imagery becomes available. The product is finalized and 'completed' for the archive the following morning - generally by around 800am.</li>
          <li>The fire sizes depicted in the product are primarily determined by the field of view of the satellite instrument, or the resolution of the analysis tool. They should not be used to estimate specific fire perimeters.</li>
          <li>The ability to detect fires and smoke can be compromised by many factors, including cloud cover, tree canopy, terrain, the size of the fire or smoke plume, the time of the day, etc. The satellite sensors used to detect fires are sensitive to heat sources and reflected sunlight. Analysts do their best to distinguish between fires and other heat sources or highly reflective surfaces, such as factories, mines, gas flares, solar panels, clouds, etc. but some false detects do get included in the analysis.</li></ul>
          
          <h4>Technical Documentation of the HMS Products</h4>
          <ul>
          <li><a href='https://www.researchgate.net/profile/Mark_Ruminski/publication/252456636_Use_of_multiple_satellite_sensors_in_NOAA%27s_operational_near_real-time_fire_and_smoke_detection_and_characterization_program/links/5498d67a0cf2c5a7e342c6e8.pdf'>Use of multiple satellite sensors in NOAA's operational near real-time fire and smoke detection and characterization program </a></li>
          <li><a href='https://journals.ametsoc.org/doi/pdf/10.1175/2008WAF2222165.1'>Description and Verification of the NOAA Smoke Forecasting System: The 2007 Fire Season</a></li>
          
          </ul>
          
          <h4>View the latest Fire and Smoke in Goolge Earth </h4>
          <ul>
          <li><a href='https://www.ospo.noaa.gov/data/land/fire/fire.kml'>Fire</a></li>
          <li><a href='https://www.ospo.noaa.gov/data/land/fire/smoke.kml'>Smoke</a></li>
          </ul>"
        )
        
        ),
      
      # CA pop smoke product box
      box(
        title = "California Vulnerable Population Data",
        HTML(
          "<h4>California population data come from the California Department of Public Health's <a href='https://discovery.cdph.ca.gov/ohe/CCHVIz/' target='_blank'>Climate Change & Health Vulnerability Indicators (CCHVIs)</a>.</h4>
          
          To quantify person days of smoke, HMS daily light, medium, and heavy smoke plumes were intersected with California Census Tracts. Population data for 2010 were used to calculate the number of people exposed to various levels of smoke. Children includes those aged under 5 while elderly includes individuals aged 65 and over."
        )
        
        )
      # ,
      # textOutput("text"),
      # tableOutput("table")
      #
      ),
    
    tabItem(tabName = "droughtDash",
            HTML("<h4>Under Construction</h4>")), 
    tabItem(tabName = "actions",
            HTML("<h4>Under Construction</h4>"))
      ))
      )

server <- function(input, output) { 

  rv <- reactiveValues(selectedCounty = NULL)
  
  
  observeEvent(input$cmap_selected, {
    rv$selectedCounty <- ifelse(is.null(input$cmap_selected), NULL, as.integer(input$cmap_selected))
  })
  
  
  dateList <- reactive({
       format(seq(as.Date(input$Time[1], format = "%Y%m%d"),as.Date(input$Time[2], format = "%Y%m%d"), by = "day"),"%Y%m%d")

    })
  
  numberOfDays <- reactive(label = "numberOfDays", x = length(dateList()))
  
  numberOfPeople <- reactive(label = "numberOfPeople", {
    if (length(input$cmap_selected) == 0) {
      sum(pop$popmean)
    } else {
      pop[.(6L, rv$selectedCounty), sum(popmean)]
    }
  })
  
  
  countyName <- reactive(label = "countyName",{
    ifelse(length(input$cmap_selected) == 0 , "California", as.character(ca$NAME_1[ca$countyFIPS == input$cmap_selected]))
  })
  
  ################ for testing dateList ###########
  # dateList <-  format(seq(as.Date("20180712", format = "%Y%m%d"),as.Date("20180715", format = "%Y%m%d"), by = "day"),"%Y%m%d")
 
  ######## calculate Day values #########
  countDays <-
    reactive ({
      
      if(length(input$cmap_selected) == 0){
        
         replace_na(data = smoke, list(
            light = 0,
            medium = 0,
            heavy = 0
          )) %>% 
          .[.(as.integer(dateList())), .(
          l = max(light, na.rm = T),
          m = max(medium, na.rm = T),
          h = max(heavy, na.rm = T)
        ), by = date] %>% 
          na.omit() %>%
          .[, .(
            lightDays = sum(l, na.rm = T),
            mediumDays = sum(m, na.rm = T),
            heavyDays = sum(h, na.rm = T)  
          )] 
      } else {
        replace_na(data = smoke, list(
          light = 0,
          medium = 0,
          heavy = 0
        )) %>% 
          .[.(as.integer(dateList()), 6L, rv$selectedCounty), .(
          l = max(light, na.rm = T),
          m = max(medium, na.rm = T),
          h = max(heavy, na.rm = T)
        ), by = date] %>% 
          na.omit() %>%
          .[, .(
            lightDays = sum(l, na.rm = T),
            mediumDays = sum(m, na.rm = T),
            heavyDays = sum(h, na.rm = T)
          )] 
      }
    })
  
   ######## Days Box text #########
  
  output$lightDaysBox <- renderValueBox({
    valueBox(paste0(round(100*(countDays()$lightDays/numberOfDays()),0),"% of Days"),
             ifelse(countyName() == "California", 
                    paste0(countyName(), " was affected by light wildfire smoke (~5 ug/m3)"),
                    paste0(countyName(), " County was affected by light wildfire smoke (~5 ug/m3)")),
      icon = icon("calendar",
                  lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$mediumDaysBox <- renderValueBox({
    valueBox(paste0(round(100*(countDays()$mediumDays/numberOfDays()),0),"% of Days"),
             ifelse(countyName() == "California", 
                    paste0(countyName(), " was affected by medium wildfire smoke (~16 ug/m3)"),
                    paste0(countyName(), " County was affected by medium wildfire smoke (~16 ug/m3)")),
             icon = icon("calendar",
                         lib = "glyphicon"),
             color = "orange"
    )
  })
  
  output$heavyDaysBox <- renderValueBox({
    valueBox(paste0(round(100*(countDays()$heavyDays/numberOfDays()),0),"% of Days"),
             ifelse(countyName() == "California", 
                    paste0(countyName(), " was affected by heavy wildfire smoke (~27+ ug/m3)"),
                    paste0(countyName(), " County was affected by heavy wildfire smoke (~27+ ug/m3)")),
             icon = icon("calendar",
                         lib = "glyphicon"),
             color = "red"
    )
  })

  ######## calculate people values #########
  countPop <-
    reactive ({
      
      if(length(input$cmap_selected) == 0){
        
        replace_na(data = smoke, list(
          light = 0,
          medium = 0,
          heavy = 0
        )) %>% 
          .[.(as.integer(dateList())), .(
            l = max(light, na.rm = T),
            m = max(medium, na.rm = T),
            h = max(heavy, na.rm = T),
            pop = mean(POPULATION, na.rm = T)
          ), by = .(COUNTYFP, TRACTCE, BLKGRPCE)] %>% 
          na.omit() %>%
          .[, .(
            lightPop = sum(l*pop, na.rm = T),
            mediumPop = sum(m*pop, na.rm = T),
            heavyPop = sum(h*pop, na.rm = T)
          )] 
      } else {
        replace_na(data = smoke, list(
          light = 0,
          medium = 0,
          heavy = 0
        )) %>% 
          .[.(as.integer(dateList()), 6L, rv$selectedCounty), .(
            l = max(light, na.rm = T),
            m = max(medium, na.rm = T),
            h = max(heavy, na.rm = T),
            pop = mean(POPULATION, na.rm = T)
          ), by = .(COUNTYFP, TRACTCE, BLKGRPCE)] %>% 
          na.omit() %>%
          .[, .(
            lightPop = sum(l*pop, na.rm = T),
            mediumPop = sum(m*pop, na.rm = T),
            heavyPop = sum(h*pop, na.rm = T)
          )]
      }
    })
  

  ######## people Box text #########
  
  output$lightPopBox <- renderValueBox({
    valueBox(paste0(round(100*(countPop()$lightPop/numberOfPeople()),0),"% of People"), 
             ifelse(countyName() == "California", 
                    paste0("in ",countyName(), " were affected by light wildfire smoke (~5 ug/m3)"),
                    paste0("in ",countyName(), " County were affected by light wildfire smoke (~5 ug/m3)")),
             icon = icon("user",
                         lib = "glyphicon"),
             color = "yellow"
    )
  })
  
  output$mediumPopBox <- renderValueBox({
    valueBox(paste0(round(100*(countPop()$mediumPop/numberOfPeople()),0),"% of People"),
             ifelse(countyName() == "California", 
                    paste0("in ",countyName(), " were affected by medium wildfire smoke (~16 ug/m3)"),
                    paste0("in ",countyName(), " County were affected by medium wildfire smoke (~16 ug/m3)")),
             icon = icon("user",
                         lib = "glyphicon"),
             color = "orange"
    )
  })
  
  output$heavyPopBox <- renderValueBox({
    valueBox(paste0(round(100*(countPop()$heavyPop/numberOfPeople()),0),"% of People"),
             ifelse(countyName() == "California", 
                    paste0("in ",countyName(), " were affected by heavy wildfire smoke (~27+ ug/m3)"),
                    paste0("in ",countyName(), " County were affected by heavy wildfire smoke (~27+ ug/m3)")),
             icon = icon("user",
                         lib = "glyphicon"),
             color = "red"
    )
  })
  

    
   
    
  # output$dygraph <- renderDygraph({
  #   # dates <- seq(as.Date(beginning):as.Date(end), by = "day")
  # 
  #   foo <- if(length(input$cmap_selected) == 0) {
  #     smoke[.(as.integer(dateList()))] %>%
  #       replace_na(list(
  #         light = 0,
  #         medium = 0,
  #         heavy = 0
  #       )) %>%
  #       .[, .(
  #         lightSmoke = sum(light * POPULATION, na.rm = T) / 1000000,
  #         mediumSmoke = sum(medium * POPULATION, na.rm = T) / 1000000,
  #         heavySmoke = sum(heavy * POPULATION, na.rm = T) / 1000000
  #       ), by = date] %>%
  #       .[, date2 := as.POSIXct(as.Date(as.character(date),  format = "%Y%m%d"))] %>%
  #       .[, .(date2, lightSmoke, mediumSmoke, heavySmoke)] %>%
  #       .[order(date2)]
  # 
  #   } else{
  #     smoke[.(as.integer(dateList()), 6L, rv$selectedCounty)] %>%
  #     replace_na(list(
  #       light = 0,
  #       medium = 0,
  #       heavy = 0
  #     )) %>%
  #     .[, .(
  #       lightSmoke = sum(light * POPULATION, na.rm = T) / 1000000,
  #       mediumSmoke = sum(medium * POPULATION, na.rm = T) / 1000000,
  #       heavySmoke = sum(heavy * POPULATION, na.rm = T) / 1000000
  #     ), by = date] %>%
  #     .[, date2 := as.POSIXct(as.Date(as.character(date),  format = "%Y%m%d"))] %>%
  #     .[, .(date2, lightSmoke, mediumSmoke, heavySmoke)] %>%
  #     .[order(date2)]
  #     }
  # 
  #   dygraph(xts(foo, order.by = foo$date2), main = "Wildfire Smoke Exposures in CA")  %>%
  #     dySeries("lightSmoke", drawPoints = F, color = "yellow") %>%
  #     dySeries("mediumSmoke", drawPoints = F, color = "orange") %>%
  #     dySeries("heavySmoke", drawPoints = F, color = "red") %>%
  #     dyOptions(fillGraph = TRUE, fillAlpha = 0.4) %>% dyRangeSelector(dateWindow = c("2018-06-01", "2018-09-30"), height = 20)
  # 
  # })
   

  SmokeByDay <-
    reactive ({
      
      if(length(input$cmap_selected) == 0){
      
      smoke[.(as.integer(dateList()))] %>%
        replace_na(list(
          light = 0,
          medium = 0,
          heavy = 0
        ))
      } else {
        smoke[.(as.integer(dateList()), 6L, rv$selectedCounty)] %>%
          replace_na(list(
            light = 0,
            medium = 0,
            heavy = 0
          ))
        
      }
    })
        
        
   
    # mapTemp <- reactive({
    #     CAtracts %>% merge(SmokeDaysByTract(), by = "ct10")
    # })
    
    
 ############ MAP ###############################   
 ######### Create a Map to selcet COunty #############

    
   output$cmap <- renderggiraph({
     
     
     g <- ggplot(data = {
       
       merge(ca, 
             {
               replace_na(data = smoke, list(
                 light = 0,
                 medium = 0,
                 heavy = 0
               )) %>%
                 .[.(as.integer(dateList())), .(PDs = sum(heavy * POPULATION, na.rm = T)), by = .(COUNTYFP)]
             }, 
             by.x = "countyFIPS", 
             by.y = "COUNTYFP",
             all.x = T
       ) %>% replace_na(list(PDs = 0))
       
       
     }) + 
       geom_sf() + 
       geom_sf_interactive(aes(fill = PDs, 
                               tooltip = NAME_1, 
                               data_id = countyFIPS), 
                               size = 0.5) + 
       scale_fill_gradient2(low = "white",mid = "yellow",high = "red2",na.value = "gray50")+
       theme(axis.line=element_blank(),
             axis.text.x=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks=element_blank(),
             axis.title.x=element_blank(),
             axis.title.y=element_blank(),
             legend.position="none",
             panel.background=element_blank(),
             panel.border=element_blank(),
             panel.grid.major=element_line(colour = "transparent"),
             panel.grid.minor=element_blank(),
             plot.background=element_blank())
     
     
     ggiraph(code = print(g), selection_type = "single",
             hover_css = "cursor:pointer; fill:blue; stroke:black;")
     
     
   })
    
    
    
############### PLOT ############################   
    
   output$VulnPlot <- renderPlotly({
       
    ggplotly({ SmokeByDay()[, .(lightSmoke = sum(light*POPULATION, na.rm = T),
                        mediumSmoke = sum(medium*POPULATION, na.rm = T),
                        heavySmoke = sum(heavy*POPULATION, na.rm = T),
                        date2 = as.Date(as.character(date),"%Y%m%d")), by = date] %>%
       melt.data.table(id.vars = "date2", measure.vars = c("lightSmoke","mediumSmoke","heavySmoke"), variable.name = "smokeLevel", value.name = "Persons") %>%
       ggplot(aes(
         x = date2,
         y = Persons/1000000, 
         fill = smokeLevel
       )) +
       geom_area(stat="identity", position = "dodge") + 
       scale_fill_manual(values = c("yellow","orange","red")) +
       ylab("Millions of Persons Exposed") + 
       xlab("Date") + 
       guides(alpha = FALSE) + 
       theme_minimal() #+ 
       # geom_hline(yintercept = 37.25396, color = "red", linetype="dashed")  + 
       # geom_label(aes(x = as.Date("2018-05-01"),y = 37.25396, label="Total CA Population")) 
    }) %>% 
       config(collaborate = FALSE,
              displaylogo = FALSE,
              modeBarButtonsToRemove = list(
                'toggleSpikelines',
                'sendDataToCloud',
                'hoverCompareCartesian',
                'zoom2d',
                'pan2d',
                'select2d',
                'lasso2d',
                'zoomIn2d',
                'zoomOut2d',
                'autoScale2d',
                'resetScale2d',
                'hoverClosestCartesian'
              )
    )
   })
   
  
   output$table <- renderDataTable(SmokeByDay())
    
   output$feedback <- renderUI({
         HTML(paste0("<h5>Displaying... <strong>", countyName(),"</strong></h5>"))
     })
   
   output$plotTitle <- renderText(ifelse(countyName() == "California", 
                                         paste0("Heavy, Medium, and Light Wildfire Smoke Exposures in California   |   ", format(input$Time[1], format="%B %d %Y"), " - ", format(input$Time[2], format="%B %d %Y")),
                                         paste0("Heavy, Medium, and Light Wildfire Smoke Exposures in ", countyName(), " County   |   ", format(input$Time[1], format="%B %d %Y"), " - ", format(input$Time[2], format="%B %d %Y")))
   )
  
   ################# random outputs to check some things #############
    # 
    # output$table <- renderTable(SmokeDaysByTract())
    # 
    # output$text <- renderText(paste0("number of days in the period you selected: ",length(dateList())))
    # 
   
   # options(bitmapType='cairo')
   # 
    outputOptions(output, "cmap", suspendWhenHidden = FALSE)
    outputOptions(output, "VulnPlot", suspendWhenHidden = FALSE)
   # outputOptions(output, "TimePlot", suspendWhenHidden = FALSE)
   # 
    }

shinyApp(ui, server)
