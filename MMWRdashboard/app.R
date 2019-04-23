
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(plotly)
library(dygraphs)
library(xts)
library(sf)
library(data.table)
library(ggiraph)


smoke <- fread("USsmokeFile.csv") %>% mutate(ST_CNTY = paste0(STATEFP,"_", COUNTYFP)) %>% data.table()
smoke <- copy(smoke)
setkey(smoke, date, STATEFP, COUNTYFP)

counties <- st_read("UScounties.json") %>% mutate(STATEFP = as.integer(as.character(STATE)), 
                                                  COUNTYFP = as.integer(as.character(COUNTY)), 
                                                  ST_CNTY = paste0(STATEFP,"_", COUNTYFP))  %>% filter(STATEFP < 60)

states <- st_read("states.json") %>% mutate(STATEFP = as.integer(as.character(STATE)))  %>% filter(STATEFP < 60) %>%  
  st_intersection(., st_set_crs(st_as_sf(as(raster::extent(-124.848974, 24.396308,-66.885444, 49.384358), "SpatialPolygons")), st_crs(.)))

davies <- fread("daviesData.csv")

beginning <- as.character(min(unique(smoke$date))) # determine first day
end <- as.character(max(unique(smoke$date))) # determine last day

pop <- smoke[, .(popmean = max(POPULATION, na.rm = T)), by = .(STATEFP, COUNTYFP)]
setkey(pop, STATEFP, COUNTYFP) 

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Wildfire Smoke"),
  dashboardSidebar(sidebarMenu(
    menuItem("Time Series", tabName = "smokeTime", icon = icon("hourglass-half")),
    menuItem("Download", tabName = "smokeDownload", icon = icon("download")),
    menuItem("Background", tabName = "fireBack", icon = icon("info"))
  )),
  dashboardBody(tabItems(
    tabItem(
      tabName = "smokeTime",
      fluidRow(
        box(
          title = "Isolate Exposures to a Location (State, County) and Time",
          collapsed = F,
          footer = "Displaying person-days of HEAVY Smoke exposure for the selected time period",
          width = 3,
          dateRangeInput(
            width = "100%",
            inputId = "Time",
            label = "Select Time Period to View",
            min = as.Date(beginning, "%Y%m%d"),
            max = as.Date(end, "%Y%m%d"),
            start = as.Date("20180101", "%Y%m%d"),
            end = as.Date("20181201", "%Y%m%d"),
            separator = " to ",
            format =
          ),
          ggiraphOutput('smap', height = "200px"),
          ggiraphOutput('cmap', height = "200px")

        )
        ,
        box(
          title = textOutput("plotTitle"),
          collapsed = F,
          width = 9,
          dygraphOutput("dygraph", height = "550px")#,
          # plotlyOutput("VulnPlot")
          
        )
      ),
      fluidRow(
        # valueBoxOutput("lightDaysBox",width = 4),
        # valueBoxOutput("mediumDaysBox",width = 4),
        # valueBoxOutput("heavyDaysBox",width = 4),
        valueBoxOutput("lightPopBox",width = 4),
        valueBoxOutput("mediumPopBox",width = 4),
        valueBoxOutput("heavyPopBox",width = 4)
      )
    ),
    tabItem("smokeDownload",
            fluidRow(
              column(9,HTML("<h3>Use controls on the 'Time Series' page to select location and days</h3>")), 
              column(3,
                     p(),
                     downloadButton(outputId = "smoke.downloadData", label = "Download Selected Data")
              )),             
            fluidRow(
              box(
                title = "Table of Plotted Data",
                footer = "Table of Plotted Data",
                collapsed = T,
                width = "100%",
                dataTableOutput('table')
              )
            )
    ),
    
    
    
    # background Tab Content\
    
    
    
    tabItem(
      tabName = "fireBack",
      
      box(width = 12, background = "navy", collapsible = F,
          HTML(
            "<h3>Wildfires and severe smoke can create dangerous conditions for people, especially those with chronic health conditions. Smoke from wildfires is a mixture of gases and fine particles from burning trees and other plant materials, as well as structures and cars. Smoke can hurt your eyes, irritate your respiratory system, and worsen chronic heart and lung diseases. Below you will find more information about wildfire smoke and health, as well as the data featured on this site.</h3>"
          )
      ),
      box(
        width = 12,
        collapsible = T,
        collapsed = T,
        title = "Wildfire Smoke and Health Equity",
        HTML(
          "<ul>
          <li><strong>Age:</strong> The very young and the very old are more sensitive to the air quality impacts of wildfires. For children, their developing respiratory systems make them vulnerable to long-term impacts of wildfire smoke, particulates, and ground level ozone.</li>
          <li><strong>Place:</strong> Communities and households at the wildland-urban interface where human-built environments are adjacent to areas of wildland vegetation are at greater risk of wildfires.</li>
          <li><strong>Socioeconomic Status:</strong> Low-income households are less likely to have disaster insurance and have fewer resources to cope with or rebound from home and property loss or temporary displacement. </li>
          <li><strong>Race and Ethnicity:</strong> African Americans have higher rates of asthma and cardiovascular disease that increase sensitivity to the health effects of smoke. </li>
          <li><strong>Tribal Communities:</strong> Native American and Alaska Native populations living near forested regions are at increased risk of displacement, smoke-exposure, injury, and property loss, especially if more populated areas are prioritized for fire management response.</li>
          <li><strong>Legal Status:</strong> Undocumented families are not eligible for FEMA assistance and even those who are eligible may fear applying. Immigrants may also have concerns about accessing evacuation shelters and other relief services due to inadequate cultural and linguistic competency of service providers, and undocumented immigrants may fear legal repercussions of seeking services.</li>
          <li><strong>Emergency Responders:</strong> Firefighters, health care personnel and emergency responders are at increased risk for injury, death, and respiratory impacts of wildfires, as well as mental health affects due to trauma. Wildfire fire fighters are exposed to safety hazards, including burns, electrocution, ash, slips, trips and falls, falling trees, rocks, and vehicle accidents.</li>
          <li><strong>Outdoor Workers:</strong> Other outdoor workers (farmworkers, utility workers) are at risk for respiratory effects of smoke and pollution.</li>
          <li><strong>Underlying Health Conditions and Status:</strong> Individuals with pre-existing cardiac or respiratory disease are at risk of disease exacerbations due to wildfire smoke. The emergency conditions created by wildfires disrupt individuals' ability to adequately manage their health conditions.
          <ul><li>People with disabilities (mobility, sensory, cognitive) may be at greater risk of injury or death during evacuations from wildfires.</li></ul></li>
          </ul>
          
          "
        )
        
        ),
      
      box(
        width = 12,
        collapsible = T,
        collapsed = T,
        title = "Evidence on Health Effects of Wildfire Smoke Exposures",
        HTML(
          'One day after exposure to heavy wildfire smoke individuals over the age of 65 had a <strong>42 % increase in risk of heart attack, 20% increase in risk of stroke, and increased respiratory emergency department visits</strong></P>
          <p>Wildfires emit fine particles and ozone precursors that in turn increase the risk of premature death and adverse chronic and acute cardiovascular and respiratory health outcomes [<a href = "https://health2016.globalchange.gov/executive-summary#finding-43" target="_blank">Likely, High Confidence</a>]. Climate change is projected to increase the number of naturally occurring wildfires in parts of the United States, increasing emissions of particulate matter and ozone precursors and resulting in additional adverse health outcomes [<a href = "https://health2016.globalchange.gov/executive-summary#finding-43" target="_blank">Likely, High Confidence</a>]</p>
          
          
          <P>            Peer Reviewed Articles:<ul>
          <li><a href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5010409/" target="_blank">Critical Review of Health Impacts of Wildfire Smoke Exposure </a></li>
          <li><a href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6015400/pdf/JAH3-7-e007492.pdf" target="_blank">Cardiovascular and Cerebrovascular Emergency Department Visits Associated With Wildfire Smoke Exposure in California in 2015</a></li>
          <li><a href="https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1002601" target="_blank">The San Diego 2007 wildfires and Medi-Cal emergency department presentations, inpatient hospitalizations, and outpatient visits: An observational study of smoke exposure periods and a bidirectional case-crossover analysis</a></li>
          
          </ul>'
        )
        ),
      
      
      # wild fire smoke product box
      box(
        width = 12,
        collapsible = T,
        collapsed = T,
        title = "Information on the Wildfire Smoke Data in this Dashboard",
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
          
          <h4>View the latest US Fire and Smoke in Goolge Earth </h4>
          <ul>
          <li><a href='https://www.ospo.noaa.gov/data/land/fire/fire.kml'>Fire</a></li>
          <li><a href='https://www.ospo.noaa.gov/data/land/fire/smoke.kml'>Smoke</a></li>
          </ul>"
        )
        
        ),
      
      # CA pop smoke product box
      box(
        width = 12,
        collapsible = T,
        collapsed = T,
        title = "Information on the California Population Data in this Dashboard",
        HTML(
          "<h4>Population data come from the US Census <a href='https://www.census.gov/geo/reference/centersofpop.html' target='_blank'>Centers of Population (2010)</a>.</h4>
          
          To quantify person days of smoke, HMS daily light, medium, and heavy smoke plumes were intersected with California Block Group <a href='https://www.census.gov/geo/reference/centersofpop.html' target=_'blank'>Centers of Population</a>. Population data for 2010 were used to calculate the number of people exposed to various levels of smoke."
        )
        
        )
      # ,
      # textOutput("text"),
      # tableOutput("table")
      #
      )





      ))
      )

server <- function(input, output) { 

  rv <- reactiveValues(selectedState = NULL)
  
  
  observeEvent(input$smap_selected, {
    rv$selectedState <- ifelse(is.null(input$smap_selected), NULL, as.integer(input$smap_selected))
    rv$selectedCounty <- NULL
  })
  
  rv <- reactiveValues(selectedCounty = NULL)
  
  observeEvent(input$cmap_selected, {
    rv$selectedCounty <- ifelse(is.null(input$cmap_selected), NULL, as.integer(input$cmap_selected))
  })
  
  
  dateList <- reactive({
       format(seq(as.Date(input$Time[1], format = "%Y%m%d"),as.Date(input$Time[2], format = "%Y%m%d"), by = "day"),"%Y%m%d")

    })

  
  
  ###### reactive create smoke subset #######
  smokeByDay <-
    reactive ({
      
      if(length(input$smap_selected) == 0 & length(input$cmap_selected) == 0){
        
        smoke[.(as.integer(dateList()))] 
      
      } else if (length(input$smap_selected) > 0 & length(input$cmap_selected) == 0) {
        
        smoke[.(as.integer(dateList()), rv$selectedState)] 
        
      }
      else {
        
        smoke[.(as.integer(dateList()), rv$selectedState, rv$selectedCounty)] 
        
      }
    })
  
  
    
  # numberOfDays <- reactive(label = "numberOfDays", x = length(dateList()))
  # 
  # numberOfPeople <- reactive(label = "numberOfPeople", {
  #   if (length(input$smap_selected) == 0 & length(input$cmap_selected) == 0) {
  #     sum(pop$popmean)
  #   } else if (length(input$smap_selected) > 0 & length(input$cmap_selected) == 0) {
  #     pop[.(rv$selectedState), sum(popmean)]
  #   } 
  #   else {
  #     pop[.(rv$selectedState, rv$selectedCounty), sum(popmean)]
  #   }
  # })
  
  placeName <- reactive(label = "placeName", {
    ifelse(
      length(input$smap_selected) == 0 ,
      "United States",
      ifelse(
        length(input$cmap_selected) == 0 ,
        as.character(states$NAME[states$STATEFP == input$smap_selected]),
        paste0(
          as.character(counties$NAME[counties$COUNTYFP == input$cmap_selected &
                                       counties$STATEFP == input$smap_selected]),
          " County, ",
          as.character(states$NAME[states$STATEFP == input$smap_selected])
        )
      )
    )
  })
  
  ################ for testing dateList ###########
  # dateList <-  format(seq(as.Date("20180712", format = "%Y%m%d"),as.Date("20180715", format = "%Y%m%d"), by = "day"),"%Y%m%d")
   
    
  output$dygraph <- renderDygraph({
    # dates <- seq(as.Date(beginning):as.Date(end), by = "day")

    foo <- smokeByDay() %>%
      .[, .(
        Light = sum(light , na.rm = T) / 1000000,
        Medium = sum(medium, na.rm = T) / 1000000,
        Heavy = sum(heavy , na.rm = T) / 1000000
      ), by = date] %>%
      .[, Date := as.POSIXct(as.Date(as.character(date),  format = "%Y%m%d"))] %>%
      .[, .(Date, Light, Medium, Heavy)] %>%
      .[order(Date)]
      

    dygraph(xts(foo, order.by = foo$Date), main = "Wildfire Smoke Exposure (Millions of Persons Under Smoke Plumes)")  %>%
      dySeries("Light", drawPoints = F, strokeWidth = 3, color = "yellow") %>%
      dySeries("Medium", drawPoints = F, strokeWidth = 3, color = "orange") %>%
      dySeries("Heavy", drawPoints = F, strokeWidth = 5, color = "red") %>%
      dyOptions(fillGraph = FALSE, fillAlpha = 0.8) %>% dyRangeSelector(dateWindow = c("2018-06-01", "2018-12-30"), height = 20)

  })
   

  
  ######## calculate Day values #########
  countDays <-
    reactive ({
      smokeByDay() %>%
        .[, .(
          l = ifelse(max(as.numeric(light)) > 0, 1, 0),
          m = ifelse(max(as.numeric(medium)) > 0 , 1, 0),
          h = ifelse(max(as.numeric(heavy)) > 0, 1, 0)
        ), by = date] %>%
        na.omit() %>%
        .[, .(
          lightDays = sum(l, na.rm = T),
          mediumDays = sum(m, na.rm = T),
          heavyDays = sum(h, na.rm = T)
        )]
    })
  ######## Days Box text #########
  
  output$lightDaysBox <- renderValueBox({
    valueBox(
      paste0(round(
        (countDays()$lightDays), 0
      ), " Days"),
      ifelse(
        placeName() == "California",
        paste0(
          placeName(),
          " was affected by light wildfire smoke (~5 ug/m3)"
        ),
        paste0(
          placeName(),
          " County was affected by light wildfire smoke (~5 ug/m3)"
        )
      ),
      icon = icon("calendar",
                  lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$mediumDaysBox <- renderValueBox({
    valueBox(
      paste0(round(
        (countDays()$mediumDays), 0
      ), " Days"),
      ifelse(
        placeName() == "California",
        paste0(
          placeName(),
          " was affected by medium wildfire smoke (~16 ug/m3)"
        ),
        paste0(
          placeName(),
          " County was affected by medium wildfire smoke (~16 ug/m3)"
        )
      ),
      icon = icon("calendar",
                  lib = "glyphicon"),
      color = "orange"
    )
  })
  
  output$heavyDaysBox <- renderValueBox({
    valueBox(
      paste0(countDays()$heavyDays, " Days"),
      ifelse(
        placeName() == "California",
        paste0(
          "The state of ",
          placeName(),
          " was affected by heavy wildfire smoke (~27+ ug/m3) on ",
          countDays()$heavyDays,
          " days between ",
          format(input$Time[1], format = "%b %d %y"),
          " and ",
          format(input$Time[2], format = "%b %d %y")
        ),
        paste0(
          "If you lived in ",
          placeName(),
          " between ",
          format(input$Time[1], format = "%b %d %y"),
          " and ",
          format(input$Time[2], format = "%b %d %y"),
          " you were likely exposed to ",
          countDays()$heavyDays,
          " days of heavy wildfire smoke (~27+ ug/m3)"
        )
      ),
      icon = icon("calendar",
                  lib = "glyphicon"),
      color = "red"
    )
  })
  
  ######## calculate people values #########
  countPop <-
    reactive ({
     smokeByDay() %>%
          .[, .(
            lightPop = sum(as.numeric(light), na.rm = T),
            mediumPop = sum(as.numeric(medium), na.rm = T),
            heavyPop = sum(as.numeric(heavy), na.rm = T)
          )]
      
    })
  
  ######## people Box text #########
  
  output$lightPopBox <- renderValueBox({
    valueBox(
      paste0(round(countPop()$lightPop / 1000000, 2), " M person-days"),
      ifelse(
        placeName() == "California",
        paste0("of light wildfire smoke (~5 ug/m3) exposure in ", placeName()),
        paste0(
          "of light wildfire smoke (~5 ug/m3) exposure in ",
          placeName()
        )
      ),
      icon = icon("user",
                  lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$mediumPopBox <- renderValueBox({
    valueBox(
      paste0(round(countPop()$mediumPop / 1000000, 2), " M person-days"),
      ifelse(
        placeName() == "California",
        paste0("medium wildfire smoke (~16 ug/m3) exposure in ", placeName()),
        paste0(
          "of medium wildfire smoke (~16 ug/m3) exposure in ",
          placeName()
        )
      ),
      icon = icon("user",
                  lib = "glyphicon"),
      color = "orange"
    )
  })
  
  output$heavyPopBox <- renderValueBox({
    valueBox(
      paste0(round(countPop()$heavyPop / 1000000, 2), " M person-days"),
      ifelse(
        placeName() == "California",
        paste0(
          "of heavy wildfire smoke (~27+ ug/m3) exposure in ",
          placeName()
        ),
        paste0(
          "of heavy wildfire smoke (~27+ ug/m3) exposure in ",
          placeName()
        )
      ),
      icon = icon("user",
                  lib = "glyphicon"),
      color = "red"
    )
  })
  
    
 ############ MAP ###############################   
 ######### Create a Map to select State #############
    
  output$smap <- renderggiraph({
    g <- ggplot(data = {
      merge(states,
            {
              replace_na(data = smoke, list(
                light = 0,
                medium = 0,
                heavy = 0
              )) %>%
                .[.(as.integer(dateList())), .(PDs = sum(heavy, na.rm = T)), by = .(STATEFP)]
            },
            by.x = "STATEFP",
            by.y = "STATEFP",
            # field in the smoke table
            all.x = T) %>% replace_na(list(PDs = 0))
    }) +
      geom_sf() +
      geom_sf_interactive(aes(
        fill = PDs,
        tooltip = NAME,
        data_id = STATEFP
      ),
      size = 0.5) +
      scale_fill_gradient2(
        low = "white",
        mid = "yellow",
        high = "red",
        na.value = "gray50"
      ) +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()
      )
    
    ggiraph(code = print(g),
            selection_type = "single",
            hover_css = "cursor:pointer; fill:gray; stroke:black;")
    
  })
    
  
  
  ############ County MAP ###############################   
  ######### Create a Map to select County  #############
  
  
  output$cmap <- renderggiraph({
    if(length(input$smap_selected) == 0){
      return(NULL)
    } else {
    gg <- ggplot(data = {
      merge(filter(counties, STATEFP == input$smap_selected),
            {
              replace_na(data = smoke, list(
                light = 0,
                medium = 0,
                heavy = 0
              )) %>%
                .[.(as.integer(dateList()), rv$selectedState), .(PDs = sum(heavy, na.rm = T)), by = .(COUNTYFP)]
            },
            by.x = "COUNTYFP",
            by.y = "COUNTYFP",
            #field in the smoke table
            all.x = T) %>% replace_na(list(PDs = 0))
    }) +
      geom_sf() +
      geom_sf_interactive(aes(
        fill = PDs,
        tooltip = NAME,
        data_id = COUNTYFP
      ),
      size = 0.5) +
      scale_fill_gradient2(
        low = "white",
        mid = "yellow",
        high = "red",
        na.value = "gray50"
      ) +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()
      )
    
    ggiraph(code = print(gg),
            selection_type = "single",
            hover_css = "cursor:pointer; fill:gray; stroke:black;")
    }
  })
  
    
    
############### PLOT ############################   
    
   # output$VulnPlot <- renderPlotly({
   #     
   #  ggplotly({ smokeByDay()[, .(Light = sum(light, na.rm = T),
   #                      Medium = sum(medium, na.rm = T),
   #                      Heavy = sum(heavy, na.rm = T),
   #                      date = as.Date(as.character(date),"%Y%m%d")), by = date] %>%
   #     melt.data.table(id.vars = "date", measure.vars = c("Light","Medium","Heavy"), variable.name = "smokeLevel", value.name = "Persons") %>%
   #     ggplot(aes(
   #       x = date2,
   #       y = Persons/1000000, 
   #       fill = smokeLevel
   #     )) +
   #     geom_area(stat="identity", position = "dodge") + 
   #     scale_fill_manual(values = c("yellow","orange","red")) +
   #     ylab("Millions of Persons Exposed") + 
   #     xlab("Date") + 
   #     guides(alpha = FALSE) + 
   #     theme_minimal() #+ 
   #     # geom_hline(yintercept = 37.25396, color = "red", linetype="dashed")  + 
   #     # geom_label(aes(x = as.Date("2018-05-01"),y = 37.25396, label="Total CA Population")) 
   #  }) %>% 
   #     config(collaborate = FALSE,
   #            displaylogo = FALSE,
   #            modeBarButtonsToRemove = list(
   #              'toggleSpikelines',
   #              'sendDataToCloud',
   #              'hoverCompareCartesian',
   #              'zoom2d',
   #              'pan2d',
   #              'select2d',
   #              'lasso2d',
   #              'zoomIn2d',
   #              'zoomOut2d',
   #              'autoScale2d',
   #              'resetScale2d',
   #              'hoverClosestCartesian'
   #            )
   #  )
   # })
   
  
   output$table <- renderDataTable(smokeByDay())
   
   output$smoke.downloadData <- downloadHandler(
     filename = function () {
       paste0("selectedWFsmokeData.csv")
     },
     
     content = function(file) {
       fwrite({
         smokeByDay()
       }, file)
     }
   )
   
   
   output$feedback <- renderPrint({
          paste0("Displaying: ", placeName())
     })
   
   output$plotTitle <- renderText(ifelse(placeName() == "United States", 
                                         paste0("Heavy, Medium, and Light Wildfire Smoke Exposures in the US   |   ", format(input$Time[1], format="%B %d %Y"), " - ", format(input$Time[2], format="%B %d %Y")),
                                         paste0("Heavy, Medium, and Light Wildfire Smoke Exposures in ", placeName(), "    |   ", format(input$Time[1], format="%B %d %Y"), " - ", format(input$Time[2], format="%B %d %Y")))
   )
  
  
   ################# random outputs to check some things #############
    # 
    # output$table <- renderTable(SmokeDaysByTract())
    # 
    # output$text <- renderText(paste0("number of days in the period you selected: ",length(dateList())))
    # 
   
   # options(bitmapType='cairo')
   # 
   # outputOptions(output, "HMLdaysMap", suspendWhenHidden = FALSE)
    outputOptions(output, "dygraph", suspendWhenHidden = FALSE)
   # outputOptions(output, "TimePlot", suspendWhenHidden = FALSE)
   # 
    }

shinyApp(ui, server)
