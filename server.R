library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(urbnthemes)

# set_urbn_defaults(style = "map")

# Leaflet bindings are a bit slow; sampling first 10000
# set.seed(100)
# districtdata <- districts[sample.int(nrow(districts), 10000),]
# By ordering by population, we ensure that the bigger head start centers
# will be drawn last and thus be easier to see
districtdata <- districts[order(districts$population),] # decreasing=TRUE

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size
    
    # reorders a lot of null values
    if(colorBy=="number_of_schools") {
      districtdata <- districts[order(is.na(districts$number_of_schools), decreasing = TRUE), ]
    } else if(colorBy=="enrollment") {
      districtdata <- districts[order(is.na(districts$enrollment), decreasing = TRUE), ]
    } else if(colorBy=="teachers_total_fte") {
      districtdata <- districts[order(is.na(districts$teachers_total_fte), decreasing = TRUE), ]
    }
    
    # colorData <- districtdata[[colorBy]]
    # pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    
    
    colorData <- districtdata[[colorBy]]
    
    if (colorBy == "default") {
      fillColor <- "#3B528BFF"
    } else if (colorBy == "number_of_schools") {
      pal <- colorBin("viridis", colorData, bins=c(0, 2, 5, 21, 37, 75, 100, 851), pretty = FALSE)
      fillColor <- pal(colorData)
    } else if (colorBy == "enrollment") {
      pal <- colorBin("viridis", colorData, bins=c(0, 249, 677, 2634.46, 12373.51, 22112.57, 31851.62, 435958), pretty = FALSE)
      fillColor <- pal(colorData)
    } else if (colorBy == "teachers_total_fte") {
      pal <- colorBin("viridis", colorData, bins=c(0, 20, 52, 176.2, 780.25, 1384.3, 1988.35, 23348), pretty = FALSE)
      fillColor <- pal(colorData)
    } else {
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
      fillColor <- pal(colorData)
    }
    
    # radius <- districtdata[[sizeBy]]
    # radius <- districtdata[[sizeBy]] / max(districtdata[[sizeBy]]) * 30000
    
    if(sizeBy=="default") {
      radius <- 2000
    } else {
      radius <- districtdata[[sizeBy]]
    }
    
    # reorders based on 
    # if(colorBy=="population") {
    #   districtdata <- districts[order(districts$population),]
    # } else if(colorBy=="median_household_income") {
    #   districtdata <- districts[order(districts$median_household_income),]
    # } else if(colorBy=="median_household_income") {
    #   districtdata <- districts[order(districts$median_household_income),]
    
    leafletProxy("map", data = districtdata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~zip_location,
                 stroke=FALSE, fillOpacity=0.4, fillColor=fillColor) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend") # %>%
      # addPolygons(
      #   data = dmv_green_data,
      #   fillColor = "green",
      #   fillOpacity = 0.7,
      #   color = "black",
      #   weight = 1,
      #   group = "Green Spaces"
      # )
  })
  
  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- districts[districts$zip_location == zipcode,]
    
    # handles exception of mult HS centers in one zip same lat/lon
    multZips = sum(districts$zip_location == zipcode)
    schoolNames = list()
    numSchools = 0
    students = 0
    teachers = 0
    if(multZips>1) {
      for(i in 1:multZips) {
        numSchools = numSchools + selectedZip$number_of_schools[i]
        students = students + selectedZip$enrollment[i]
        teachers = teachers + selectedZip$teachers_total_fte[i]
        schoolNames<-c(schoolNames,selectedZip$lea_name[i])
      }
    }
    
    ## debugging
    # print(multZips)
    # message("county name:", selectedZip$county_name[1])
    # message("name:", selectedZip$lea_name, schoolNames)
    # message("zip:", selectedZip$zip_location[1])
    # message("Value of income:", selectedZip$median_household_income[1])
    # message("Value of pop:", selectedZip$population[1])
    # message("Value of schools:", selectedZip$number_of_schools, numSchools)
    # message("Value of student:", selectedZip$enrollment, students)
    # message("Value of teacher:", selectedZip$teachers_total_fte, teachers)

    content <- as.character(tagList(
      tags$h4(ifelse(multZips>1, schoolNames, selectedZip$lea_name)),
      tags$strong(HTML(sprintf("%s, %s %s",
                               selectedZip$county_name[1], selectedZip$state_location[1], selectedZip$zip_location[1]
      ))), tags$br(),
      sprintf("Median household income: %s", dollar(selectedZip$median_household_income[1])), tags$br(),
      # sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
      sprintf("Population: %s", selectedZip$population[1]), tags$br(),
      sprintf("Number of schools: %s", ifelse(multZips>1, numSchools, selectedZip$number_of_schools)), tags$br(),
      sprintf("Total student enrollment: %s", ifelse(multZips>1, students, selectedZip$enrollment)), tags$br(),
      sprintf("Total number of teachers: %s", ifelse(multZips>1, teachers, selectedZip$teachers_total_fte))
    ))
    
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
    
    
    # leafletProxy("map") %>%
    #   addPolygons(
    #     data = dmv_green_data,
    #     fillColor = "green",
    #     fillOpacity = 0.7,
    #     color = "black",
    #     weight = 1,
    #     group = "Green Spaces"
    #   ) %>%
    #   addLegend(
    #     "bottomright",
    #     colors = "green", # Legend color
    #     labels = "Green Space", # Legend label
    #     opacity = 0.7
    #   ) %>%
    #   addLayersControl(
    #     overlayGroups = "Green Spaces", # Group name for the green spaces layer
    #     options = layersControlOptions(collapsed = TRUE) # Collapsed layer control
    #   )
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  
  ## County Map ##############################################
  
  observe({
    # colorCountyBy <- input$countyColor
    # print(colorCountyBy)

    pal <- colorQuantile("YlOrRd", NULL, n = 9)

    popup_dat <- paste0("<strong>County: </strong>",
                        merged_data$NAMELSAD,
                        "<br><strong>Population: </strong>",
                        merged_data$TotalPopulation)

    output$countyMap <- renderLeaflet({
      leaflet(data = merged_data) %>%
        addTiles() %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
        addPolygons(fillColor = ~pal(TotalPopulation),
                    fillOpacity = 0.8,
                    color = "#BDBDC3",
                    weight = 1,
                    popup = popup_dat)%>%
        addLegend("bottomleft", pal=pal, values=merged_data$TotalPopulation, title="population",
                  layerId="colorLegend")
    })
  })
  
  ##### ATTEMPT TO ADD COLOR SWITCHING TO COUNTY MAP
  
  # observe({
  #   colorCountyBy <- input$countyColor
  # 
  #   # pal <- colorQuantile("YlOrRd", NULL, n = 9)
  # 
  #   colorData <- districtdata[[colorCountyBy]]
  #   pal <- colorBin("viridis", colorData, bins=c(0, 2, 5, 21, 37, 75, 100, 851), pretty = FALSE)
  #   fillColor <- pal(colorData)
  # 
  #   popup_dat <- paste0("<strong>County: </strong>",
  #                       merged_data$NAMELSAD,
  #                       "<br><strong>Population: </strong>",
  #                       merged_data$TotalPopulation)
  # 
  #   output$countyMap <- renderLeaflet({
  #     leaflet(data = merged_data) %>%
  #       addTiles() %>%
  #       setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
  #       addPolygons(fillColor = fillColor,
  #                   fillOpacity = 0.8,
  #                   color = "#BDBDC3",
  #                   weight = 1,
  #                   popup = popup_dat)%>%
  #       addLegend("bottomleft", pal=pal, values=merged_data$colorCountyBy, title="population",
  #                 layerId="colorLegend")
  #   })
  # })
  
  
  
  # using leafletproxy for county map - not working?
  
  # output$countyMap <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles() %>%
  #     setView(lng = -93.85, lat = 37.45, zoom = 4)
  # })
  # 
  # observe({
  #   pal <- colorQuantile("YlOrRd", NULL, n = 9)
  # 
  #   popup_dat <- paste0("<strong>County: </strong>",
  #                       merged_data$NAMELSAD,
  #                       "<br><strong>Population: </strong>",
  #                       merged_data$TotalPopulation)
  #   
  #   leafletProxy("countyMap", data = merged_data) %>%
  #     clearShapes() %>%
  #     addPolygons(fillColor = ~pal(TotalPopulation), 
  #                 fillOpacity = 0.8, 
  #                 color = "#BDBDC3", 
  #                 weight = 1,
  #                 popup = popup_dat)%>%
  #     addLegend("bottomleft", pal=pal, values=merged_data$TotalPopulation, title="population",
  #               layerId="colorLegend")
  # })
  
  ## Data Explorer ###########################################
  
  observe({
    counties <- if (is.null(input$states)) character(0) else {
      dplyr::filter(cleantable, State %in% input$states) %>%
        `$`('County') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$counties[input$counties %in% counties])
    updateSelectizeInput(session, "counties", choices = counties,
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        dplyr::filter(State %in% input$states,
               is.null(input$counties) | County %in% input$counties) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectizeInput(session, "zipcodes", choices = zipcodes,
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      dplyr::filter(
        is.null(input$states) | State %in% input$states,
        is.null(input$counties) | County %in% input$counties,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "ziptable")
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
