
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(sf)
library(data.table)
library(dygraphs)
library(xts)

# setwd("~/GitHub/WFSmokeExp/taketwo/")
smoke <- fread("CAsmokeFile.csv")
smoke <- copy(smoke)
setkey(smoke, date, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)

ca <- st_read("counties.geoJSON") %>% mutate(countyFIPS = as.integer(substr(COUNTYFI_1, 3,5)))

beginning <- as.character(min(unique(smoke$date))) # determine first day
end <- as.character(max(unique(smoke$date))) # determine last day

# CApop <- smoke[,.(BGpop = mean(POPULATION, na.rm = T)), keyby = .(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)] %>% .[,.(sum(BGpop))]

# CApop$V1 = 37.25396 million

tableList <- fread("CenPop2010_Mean_BG06.txt")

allBGs <- tableList[, geoIDer := paste0(STATEFP,"_",COUNTYFP,"_",TRACTCE,"_",BLKGRPCE)]
allBGs <- copy(allBGs)
setkey(allBGs, STATEFP, COUNTYFP, geoIDer)

popCentroidsBG <- st_as_sf(tableList, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

ui <- dashboardPage(
  dashboardHeader(title = "CA Climate Exposures Dashboard"),
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
      # box(
      #   title = "Heavy, Medium, and Light Wildfire Smoke Exposures in California",
      #   solidHeader = T,
      #   status = "primary",
      #   collapsible = T,
      #   collapsed = F,
      #   width = "100%",
      #   dateRangeInput(width = "30%",
      #     inputId = "Time",
      #     label = "Select Time Period to View",
      #     min = as.Date(beginning, "%Y%m%d"),
      #     max = as.Date(end, "%Y%m%d"),
      #     start = as.Date("20180601", "%Y%m%d"),
      #     end = as.Date("20180930", "%Y%m%d"),
      #     separator = " to ",
      #     format =
      #   ),
      #   plotOutput("VulnPlot")
      # ),
      box(
        title = "Select a County",
        status = "info",
        collapsible = T,
        collapsed = F,
        width = "30%",
        ggiraphOutput('cmap')
      ),
      box(
        title = "Heavy, Medium, and Light Wildfire Smoke Exposures in California",
        solidHeader = T,
        status = "primary",
        collapsible = T,
        collapsed = F,
        width = "70%",
        dygraphOutput("dygraph")
      ), 
      textOutput(outputId = "feedback")
      
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
    rv$selectedCounty <- as.integer(input$cmap_selected)
  })
  
  
    dateList <- reactive({
       format(seq(as.Date(input$Time[1]),as.Date(input$Time[2]), by = "day"),"%Y%m%d")

    })

    
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
  
  output$dygraph <- renderDygraph({
    # dates <- seq(as.Date(beginning):as.Date(end), by = "day")

    foo <- if(is.null(rv$selectedCounty)) {
      smoke[.(unique(date))] %>%
        replace_na(list(
          light = 0,
          medium = 0,
          heavy = 0
        )) %>%
        .[, .(
          lightSmoke = sum(light * POPULATION, na.rm = T) / 1000000,
          mediumSmoke = sum(medium * POPULATION, na.rm = T) / 1000000,
          heavySmoke = sum(heavy * POPULATION, na.rm = T) / 1000000
        ), by = date] %>%
        .[, date2 := as.POSIXct(as.Date(as.character(date),  format = "%Y%m%d"))] %>%
        .[, .(date2, lightSmoke, mediumSmoke, heavySmoke)] %>%
        .[order(date2)]
      
    } else{
      smoke[.(unique(date), 6L, c(1,3,5))] %>%
      replace_na(list(
        light = 0,
        medium = 0,
        heavy = 0
      )) %>%
      .[, .(
        lightSmoke = sum(light * POPULATION, na.rm = T) / 1000000,
        mediumSmoke = sum(medium * POPULATION, na.rm = T) / 1000000,
        heavySmoke = sum(heavy * POPULATION, na.rm = T) / 1000000
      ), by = date] %>%
      .[, date2 := as.POSIXct(as.Date(as.character(date),  format = "%Y%m%d"))] %>%
      .[, .(date2, lightSmoke, mediumSmoke, heavySmoke)] %>%
      .[order(date2)]
      }

    dygraph(xts(foo, order.by = foo$date2), main = "Wildfire Smoke Exposures in CA")  %>%
      dySeries("lightSmoke", drawPoints = F, color = "yellow") %>%
      dySeries("mediumSmoke", drawPoints = F, color = "orange") %>%
      dySeries("heavySmoke", drawPoints = F, color = "red") %>%
      dyOptions(fillGraph = TRUE, fillAlpha = 0.4) %>% dyRangeSelector(dateWindow = c("2018-06-01", "2018-09-30"), height = 20)

  })
   

  SmokeByDay <-
    reactive ({
      smoke[.(as.integer(as.character(input$Time[1], format = "%Y%m%d")):as.integer(as.character(input$Time[2], format = "%Y%m%d")))] %>%
        replace_na(list(
          light = 0,
          medium = 0,
          heavy = 0
        ))
    })
   
    # mapTemp <- reactive({
    #     CAtracts %>% merge(SmokeDaysByTract(), by = "ct10")
    # })
    
    
 ############ MAP ###############################   
 ######### Create a Map to selcet COunty #############

    
   output$cmap <- renderggiraph({
     
     g <- ggplot(ca) + 
       geom_sf() + 
       geom_sf_interactive(aes(tooltip = NAME_1, 
                               data_id = countyFIPS), 
                               size = 0.5) + 
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
     
     
     ggiraph(code = print(g), selection_type = "multiple",
             hover_css = "cursor:pointer; fill:grey; stroke:black;")
     
     
   })
    
    
    
############### PLOT ############################   
    
    plotData <- reactive({
     
    vuln[, ct10:= paste0("0",as.character(as.numeric(ct10)))] %>%
        merge(SmokeDaysByTract(), by = "ct10") %>%
        .[, c("lightPD", "mediumPD", "heavyPD") := .(population * light,
                                                     population * medium,
                                                     population * heavy)] %>%
        .[cohort != "total" & cohort != "other" &  race != "Total", .(Heavy = sum(heavyPD, na.rm = T),
                                                                      Medium = sum(mediumPD, na.rm = T),
                                                                      Light = sum(lightPD, na.rm = T)), by= .(cohort, race)] %>%
        melt.data.table(id.vars = c("cohort", "race"), measure.vars = c("Light", "Medium","Heavy"), value.name = "PersonDays",value.factor = T, variable.name = "SmokeDensity")
      
        
       
   })
   
   
   output$VulnPlot <- renderPlot({
       
       SmokeByDay()[, .(lightSmoke = sum(light*POPULATION, na.rm = T),
                        mediumSmoke = sum(medium*POPULATION, na.rm = T),
                        heavySmoke = sum(heavy*POPULATION, na.rm = T),
                        date2 = as.Date(as.character(date),"%Y%m%d")), by = date] %>%
       melt.data.table(id.vars = "date2", measure.vars = c("lightSmoke","mediumSmoke","heavySmoke"), variable.name = "smokeLevel", value.name = "Persons")%>%
       ggplot(aes(
         x = date2,
         y = Persons/1000000, 
         fill = smokeLevel
       )) +
       scale_fill_manual(values = c("yellow","orange","red")) +
       geom_area(stat = "identity", position = "dodge", alpha= 0.6) + 
       ylab("Millions of Persons Exposed") + 
       xlab("Date") + 
       guides(alpha = FALSE) + 
       theme_minimal() + 
       geom_hline(yintercept = 37.25396, color = "red", linetype="dashed") # + 
       # geom_label(aes(x = as.Date("2018-05-01"),y = 37.25396, label="Total CA Population")) 
     
   })
   
  
   output$table <- renderDataTable(SmokeByDay())
    
   output$feedback <- renderPrint({
     paste0("County Selected:  ", rv$selectedCounty)
     })
   
  
   ##################### TIme Series plot #################################
   
   output$TimePlot <- renderPlot({
   
   SmokeByDay %>% ggplot() + 
       geom_area(aes(x = date, y= Persons/1000000, fill = smoke), position = "dodge") + 
       scale_fill_manual(values = c("yellow","orange","red")) +
       ylab("Millions of People") + xlab("Date")  + 
       geom_hline(yintercept = 37.25, color = "red", linetype="dashed") + 
       geom_label(aes(x = as.Date("2018-05-01"),y = 37.25, label="Total CA Population")) + 
       geom_vline(xintercept = as.Date("2018-07-27"), color = "gray20", linetype="twodash") + 
       geom_label(aes(x = as.Date("2018-07-27"),y = 35, label="Mendocino Complex Start", hjust="right")) +
       geom_vline(xintercept = as.Date("2018-07-23"), color = "gray20", linetype="dotdash") + 
       geom_label(aes(x = as.Date("2018-07-23"),y = 32, label="Carr Fire Start", hjust="right")) +
       geom_vline(xintercept = as.Date("2018-07-13"), color = "gray20", linetype="longdash") + 
       geom_label(aes(x = as.Date("2018-07-13"),y = 29, label="Ferguson Fire Start", hjust="right")) + theme_minimal()
   })
   
   
   ################# random outputs to check some things #############
    # 
    # output$table <- renderTable(SmokeDaysByTract())
    # 
    # output$text <- renderText(paste0("number of days in the period you selected: ",length(dateList())))
    # 
   
   # options(bitmapType='cairo')
   # 
   # outputOptions(output, "HMLdaysMap", suspendWhenHidden = FALSE)
    outputOptions(output, "VulnPlot", suspendWhenHidden = FALSE)
   # outputOptions(output, "TimePlot", suspendWhenHidden = FALSE)
   # 
    }

shinyApp(ui, server)
