library(dplyr)
library(shiny)
library(shinyWidgets)
library(rgdal)
library(sp)
library(xts)
library(leaflet)

# datasets
data <- read.csv("data/full-data.csv")
counties <- read.csv("data/Counties.csv")
allprecincts <- list()
for(i in unique(data$County)){
  x <- unique(data$id[which(data$County == i)])
  allprecincts[[i]] <- x
}

# ui object
ui <- fluidPage(
  tags$head(includeCSS("www/style.css")),
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      div(
        id = "name",
        h1("Florida Voter Data"),
        hr(),
        p(
          id = "desc",
          "testing..."
        )
      ),
      br(),
      div(
        id = "clear",
        actionButton("clearfilters", "Reset")
      ),
      radioButtons(
        inputId = "detaillevel",
        label = "Map view:",
        choices = c("County-level" = "county", "Precinct-level" = "precinct"),
        inline = T
      ),
      pickerInput(
        inputId = "countyselected",
        label = "Counties:",
        choices = sort(unique(data$County)),
        options = list(`actions-box` = TRUE),
        multiple = T,
        selected = unique(data$County)
      ),
      uiOutput('precinctpicker'),
      pickerInput(
        inputId = "yearselected",
        label = "Year:",
        choices = sort(unique(data$Year), decreasing = T),
        #multiple = T,
        #selected = unique(data$Year)[1]
      ),
      selectInput(
        inputId = "variableselected",
        label = "Variable:",
        choices = colnames(data)[!(names(data) %in% c("Year", "County", "Precinct", "Gender", "id"))]
      ),
      checkboxInput("morefilters", "More filters?"),
      conditionalPanel(
        condition = "input.morefilters == true",
        pickerInput(
          inputId = "genderselected",
          label = "Genders:",
          choices = sort(unique(data$Gender)),
          multiple = T,
          selected = unique(data$Gender)
        )
      ),
      div(
        id = "downloads",
        downloadButton("downloadFilteredData", "Download Data"),
        downloadButton("downloadData", "Download All Data")
      )
    ),
    mainPanel(
      id = "mainpanel",
      leafletOutput(outputId = "map", height = "100%")
    )
  ),
  hr(),
  fluidRow(
    column(
      4,
      div(
        id = "source",
        p(
          "Data source: ",
          a(
            "test",
            href = "https://joshmire.com"
          )
        )
      )
    ),
    column(
      4,
      div(
        id = "signature",
        p(
          a(
            img(
              src = "logo.png"
            ),
            href = "https://joshmire.com"
          )
        )
      )
    ),
    column(
      4,
      div(
        id = "update",
        p(
          "Last updated: ", 
          span(
            style = "font-style:italic;", 
            if(substr(format(as.Date(file.mtime("app.R")), "%d"), 1, 1) == 0){
              paste0(format(as.Date(file.mtime("app.R")), "%B "), substr(format(as.Date(file.mtime("app.R")), "%d"), 2, 2), format(as.Date(file.mtime("app.R")), ", %Y"))
            }
            else{
              format(as.Date(file.mtime("app.R")), "%B %d, %Y")
            }
          )
        )
      )
    )
  )
)

# server()
server <- function(input, output, session) {
  
  selectedprecincts <- reactiveVal(allprecincts)
  
  observeEvent(
    input$precinctselected,
    {
      for(i in length(names(selectedprecincts()))){
        for(j in length(selectedprecincts()[[i]])){
          if(!selectedprecincts()[[i]][j] %in% input$precinctselected){
            selectedprecincts()[[i]][j] <- NULL
          }
        }
      }
    }
  )
  

  
  output$precinctpicker <- renderUI({
    conditionalPanel(
      condition = "input.detaillevel == 'precinct'",
      pickerInput(
        inputId = "precinctselected",
        label = "Precincts:",
        choices = allprecincts[which(names(allprecincts) %in% input$countyselected)],
        options = list(`actions-box` = TRUE),
        multiple = T,
        selected = unlist(selectedprecincts())
      )
    )
  })

  observeEvent(
    input$clearfilters,
    {
      updateRadioButtons(
        session,
        "detaillevel",
        selected = "county"
      )
      updatePickerInput(
        session,
        "countyselected",
        selected = unique(data$County)
      )
      updatePickerInput(
        session,
        "precinctselected",
        selected = NULL
      )
      updatePickerInput(
        session,
        "yearselected",
        selected = sort(unique(data$Year))[1]
      )
      updateCheckboxInput(
        session,
        "morefilters",
        value = F
      )
      updatePickerInput(
        session,
        "genderselected",
        selected = unique(data$Gender)
      )
    }
  )
  
  observeEvent(
    input$map_shape_click,
    {
    event <- input$map_shape_click
    updatePickerInput(
      session,
      "countyselected",
      selected = event$id
    )
  })
  
  output$map <- renderLeaflet({
    if(input$detaillevel == "county"){
      map <- readOGR("shapefiles/florida/15356ca1-ed9e-4709-a247-f58c117b1a94202049-1-l5bb1k.49fqp.shp")
      map@data$ABVS <- counties$abv
      datafiltered <- data[which(data$Year == input$yearselected & data$County %in% input$countyselected & data$Gender %in% input$genderselected), ] %>%
        group_by(Year, County) %>%
        summarise(
          Percent = mean(Percent),
          Number = mean(Number)
        ) %>%
        ungroup() %>%
        data.frame(.)
      ordercounties <- match(map@data$NAME, datafiltered$County)
      map@data <- datafiltered[ordercounties, ]
      map$variableplot <- as.numeric(map@data[, input$variableselected])
      if(input$variableselected == "Percent"){
        map$variablelabel <- paste0(round(map@data[, input$variableselected], 4) * 100, "%")
      }
      else{
        map$variablelabel <- as.numeric(map@data[, input$variableselected])
      }
      pal <- colorBin("YlOrRd", domain = map$variableplot, bins = 7)
      labels <- sprintf("%s County: %s", map$County, map$variablelabel) %>%
        lapply(htmltools::HTML)
      l <- leaflet(map) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~ pal(variableplot),
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          label = labels,
          layerId = map$County
        ) %>%
        leaflet::addLegend(
          pal = pal, 
          values = ~variableplot,
          opacity = 0.7, 
          title = NULL,
          position = "bottomright"
        )
    }
    else{
      map <- spTransform(readOGR("test/FL/fl_2016.shp"), CRS("+init=epsg:4326"))
      datafiltered <- data[which(data$Year == input$yearselected & data$County %in% input$countyselected & data$id %in% input$precinctselected & data$Gender %in% input$genderselected), ] %>%
        group_by(Year, id, Precinct) %>%
        summarise(
          Percent = mean(Percent),
          Number = mean(Number)
        ) %>%
        ungroup() %>%
        data.frame(.)
      orderprecincts <- match(map@data$countypct, datafiltered$id)
      map@data <- datafiltered[orderprecincts, ]
      map$variableplot <- as.numeric(map@data[, input$variableselected])
      if(input$variableselected == "Percent"){
        map$variablelabel <- paste0(round(map@data[, input$variableselected], 4) * 100, "%")
      }
      else{
        map$variablelabel <- as.numeric(map@data[, input$variableselected])
      }
      pal <- colorBin("YlOrRd", domain = map$variableplot, bins = 7)
      labels <- sprintf("Precinct %s: %s", map$Precinct, map$variablelabel) %>%
        lapply(htmltools::HTML)
      l <- leaflet(map) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~ pal(variableplot),
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          label = labels
        ) %>%
        leaflet::addLegend(
          pal = pal, 
          values = ~variableplot,
          opacity = 0.7, 
          title = NULL,
          position = "bottomright"
        )
    }
  })
  
  output$downloadFilteredData <- downloadHandler(
    filename = "filtered_data.csv",
    content = function(file) {
      write.csv(data[which(data$Year == input$yearselected & data$County %in% input$countyselected & data$Precinct %in% input$precinctselected & data$Gender %in% input$genderselected), c("County", "Precinct", "Year", "Gender", input$variableselected)], file, row.names = FALSE)
    }
  )
  
  output$downloadData <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(data, file, row.names = FALSE)
    }
  )
  
}

# shinyApp()
shinyApp(ui = ui, server = server)