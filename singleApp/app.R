
# Load required packages --------------------------------------------------

# library(shiny)
# runApp("singleApp")

library(jsonlite)
library(ggplot2) # NOT used
suppressPackageStartupMessages(library(googleVis))

# define a function to sanitize strings (the API call would fail with strings containing ',' or ' ')
sanitizeString <- function(inputString){
  inputString <- gsub(pattern = ",", replacement = "", x = inputString)
  sanitized_string <- gsub(pattern = " ", replacement = "+", x = inputString)
  return(sanitized_string)
}


# General settings --------------------------------------------------------

api_open_request <- "https://api.fda.gov/"
api_endpoint <- "device/event.json?"
api_key = "api_key=12345"

api_firstCall_medDev <- paste0(api_open_request, api_endpoint,
                               "search=date_received:[1991-01-01+TO+", Sys.Date(), "]",
                               "&count=device.generic_name.exact&limit=1000")

api_firstCall_manufacturer <- paste0(api_open_request, api_endpoint,
                                     "search=date_received:[1991-01-01+TO+", Sys.Date(), "]",
                                     "&count=device.manufacturer_d_name.exact&limit=1000")


# Make API calls to populate selectInputs ---------------------------------

medDevices <- fromJSON(api_firstCall_medDev)
manufacturers <- fromJSON(api_firstCall_manufacturer)

# extract the list of the most common medical devices and the 1000 most common manufacturers
medDevices_list <- sort(medDevices$results$term)
manufacturers_list <- sort(manufacturers$results$term)

# add "ALL" to the lists
medDevices_list <- append(x = c("ALL"), values = medDevices_list)
manufacturers_list <- append(x = c("ALL"), values = manufacturers_list)


# server Fuction ----------------------------------------------------------

server <- function(input, output) {
  
  # date range to use in the API call
  search_dates <- reactive({
    paste0("search=date_received:[", input$dateRange[1], "+TO+", input$dateRange[2], "]")
  })
  
  # manufacturer to use in the API call
  search_manufacturer <- reactive({
    if (input$chosen_manufacturer != "ALL") {
      paste0("+AND+device.manufacturer_d_name:", sanitizeString(input$chosen_manufacturer))
    } else {
      ""
    }
  })
  
  # medical device to use in the API call
  search_medDev <- reactive({
    if (input$chosen_medDev != "ALL") {
      paste0("+AND+device.generic_name:", sanitizeString(input$chosen_medDev))
    } else {
      ""
    }    
  })

  # API call to obtain a time series
  timeSeries <- reactive({
    api_response_timeSeries <- fromJSON(
      paste0(
        api_open_request, api_endpoint,
        search_dates(), search_manufacturer(), search_medDev(),
        "&count=date_received"
      )
    )
    dates_posix <- strptime(api_response_timeSeries$results$time, "%Y%m%d")
    received_by_fda <- as.Date(dates_posix, "%Y-%m-%d")
    data.frame(dates = received_by_fda, reports = api_response_timeSeries$results$count)
  })
  
  # API call to obtain the top10 list of the countries where most of the adverse events occurred
  top10Countries <- reactive({
    api_response_top10Countries <- fromJSON(
      paste0(
        api_open_request, api_endpoint,
        search_dates(), search_manufacturer(), search_medDev(),
        "&count=device.manufacturer_d_country.exact&limit=10"
      )
    )
    data.frame(countries = api_response_top10Countries$results$term,
               reports   = api_response_top10Countries$results$count)
  })
  
  # API call to obtain the most recurring event types regarding the adverse events occurred
  # Note: exclude 'No answer provided', '' (empty string), and '?'
  eventType <- reactive({
    api_response_eventType <- fromJSON(
      paste0(
        api_open_request, api_endpoint,
        search_dates(), search_manufacturer(), search_medDev(),
        "+AND+(",
        "event_type:", "\"", "malfunction", "\"",
        "+event_type:", "\"", "injury", "\"",
        "+event_type:", "\"", "death", "\"",
        "+event_type:", "\"", "other", "\"",
        ")",
        "&count=event_type.exact"
      )
    )
    data.frame(event   = api_response_eventType$results$term,
               reports = api_response_eventType$results$count)
  })
  
  # API call to obtain the most frequently reported Medical Devices having event_type:death
  deaths <- reactive({
    api_response_deaths <- fromJSON(
      paste0(
        api_open_request, api_endpoint,
        search_dates(), search_manufacturer(),
        "+AND+event_type:death",
        "&count=device.generic_name.exact",
        "&limit=10"
      )
    )
    data.frame(device  = api_response_deaths$results$term,
               reports = api_response_deaths$results$count)
  })
  
  # API call to obtain the most frequently reported Medical Devices having event_type:injury
  injuries <- reactive({
    api_response_injuries <- fromJSON(
      paste0(
        api_open_request, api_endpoint,
        search_dates(), search_manufacturer(),
        "+AND+event_type:injury",
        "&count=device.generic_name.exact",
        "&limit=10"
      )
    )
    data.frame(device  = api_response_injuries$results$term,
               reports = api_response_injuries$results$count)
  })
  
  # API call to obtain the most frequently reported Medical Devices having event_type:malfunction
  malfunctions <- reactive({
    api_response_malfunctions <- fromJSON(
      paste0(
        api_open_request, api_endpoint,
        search_dates(), search_manufacturer(),
        "+AND+event_type:malfunction",
        "&count=device.generic_name.exact",
        "&limit=10"
      )
    )
    data.frame(device  = api_response_malfunctions$results$term,
               reports = api_response_malfunctions$results$count)
  })
  
  # API call to obtain the most important field in the Medical Device Reports
  # Note: the maximum number of elements returned by the openFDA elasticsearch-based API is 100.
  # A solution would be making more API calls skipping previously generated results (e.g. "&skip=100").
  medDevReports <- reactive({
    api_response_df <- fromJSON(
      paste0(
        api_open_request, api_endpoint,
        search_dates(), search_manufacturer(), search_medDev(),
        "&limit=100&skip=0"
      )
    )
    
    # create empty lists to store medical devices identification data
    generic_name         <- character()
    brand_name           <- character()
    product_code         <- character()
    manufacturer_name    <- character()
    manufacturer_country <- character()
    
    # api_response_df$results$device is a list (there can be more than one device involved in a single
    # adverse event), so we can append new data to an existing list, and build a data frame afterwards.
    
    for(i in seq(from = 1, to = length(api_response_df$results$device))) {
      
      date_posix <- strptime(api_response_df$results$date_received[i], "%Y%m%d")
      received_by_fda <- as.Date(date_posix, "%Y-%m-%d")
      product_code <- append(product_code, api_response_df$results$device[[i]]$device_report_product_code)
      generic_name <- append(generic_name, api_response_df$results$device[[i]]$generic_name)
      brand_name <- append(brand_name, api_response_df$results$device[[i]]$brand_name)
      manufacturer_name <- append(manufacturer_name, api_response_df$results$device[[i]]$manufacturer_d_name)
      manufacturer_country <- append(manufacturer_country, api_response_df$results$device[[i]]$manufacturer_d_country)
      
    }
    
    # create the data frame and return it to the reactive expression
    data.frame(
      receivedByFDA = received_by_fda,
      # eventKey      = api_response_df$results$event_key,
      eventType     = api_response_df$results$event_type,
      # eventLocation = api_response_df$results$event_location,
      # reportNumber  = api_response_df$results$report_number,
      reportSource  = api_response_df$results$report_source_code,
      genericName   = generic_name,
      brandName     = brand_name,
      prodCode      = product_code,
      # manCountry  = manufacturer_country,
      manName     = manufacturer_name
    )
    
  })
  
  # create dynamically the manufacturer_selectInput UI component
  output$manufacturer_selectInput <- renderUI({
    searchString <- toupper("")
    manufacturer_RegEx <- sort(unique(
      manufacturers_list[grep(pattern = searchString, x = manufacturers_list, ignore.case = TRUE)]))
    selectInput(inputId = "chosen_manufacturer", label = "",
                choices = manufacturer_RegEx, selected = manufacturers_list[1])
  })
        
  # create dynamically the medDev_selectInput UI component
  output$medDev_selectInput <- renderUI({
    searchString <- toupper("")
    medDev_RegEx <- sort(unique(
      medDevices_list[grep(pattern = searchString, x = medDevices_list, ignore.case = TRUE)]))
    selectInput(inputId = "chosen_medDev", label = "",
                choices = medDev_RegEx, selected = medDevices_list[1])
  })
    
  # Time Series
  # Note: googleVis charts are not shown within RStudio
  output$gVisTimeSeriesView <- renderGvis({
    gvisLineChart(data = timeSeries(), options = list(
      title="", vAxis="{title:'Reports'}", hAxis="{title:'Time'}"))
    })
  
  # Bar Plot top10 countries
  # Note: googleVis charts are not shown within RStudio
  output$gVisBarPlotCountriesView <- renderGvis({
    gvisBarChart(data = top10Countries(), options = list(
      title="", vAxis="{title:'Countries'}", hAxis="{title:'Reports'}"))
  })
  
  # Bar Plot event type
  # Note: googleVis charts are not shown within RStudio
  output$gVisBarPlotEventTypeView <- renderGvis({
    gvisBarChart(data = eventType(), options = list(
      title="", vAxis="{title:'Event Type'}", hAxis="{title:'Reports'}"))
  })
  
  # Bar Plot death
  # Note: googleVis charts are not shown within RStudio
  output$gVisBarPlotDeathView <- renderGvis({
    gvisBarChart(data = deaths(), options = list(
      series="[{color:'black'}]",
      title="Deaths", vAxis="{title:'Medical Devices'}", hAxis="{title:'Reports'}"))
  })
  
  # Bar Plot injury
  # Note: googleVis charts are not shown within RStudio
  output$gVisBarPlotInjuryView <- renderGvis({
    gvisBarChart(data = injuries(), options = list(
      series="[{color:'orange'}]",
      title="Injuries", vAxis="{title:'Medical Devices'}", hAxis="{title:'Reports'}"))
  })
  
  # Bar Plot malfunction
  # Note: googleVis charts are not shown within RStudio
  output$gVisBarPlotMalfunctionView <- renderGvis({
    gvisBarChart(data = malfunctions(), options = list(
      series="[{color:'red'}]",
      title="Malfunctions", vAxis="{title:'Medical Devices'}", hAxis="{title:'Reports'}"))
  })
  
  # Table containing the most important fields in a Medical Device adverse event Report
  output$tableView <- renderDataTable({
    medDevReports()
  })
  
} # close server function


# User Interface ----------------------------------------------------------

ui <- shinyUI(fluidPage(
  
  # load the CSS (try with a different CSS file)
  # theme = "bootstrap.css",
  
  titlePanel("MAUDE adverse events (titlePanel)"),
  
  # global layout of the shiny application
  sidebarLayout(position = "left",
    
    sidebarPanel(
      
      helpText(strong("Date Range"), "(Format: yyyy-mm-dd)"),
      ### helpText(em("Format: yyyy-mm-dd")),
      dateRangeInput("dateRange", label = "", start = Sys.Date() - 365, end = Sys.Date()),
      
      hr(),      
      
      helpText(strong("Manufacturer"), "(e.g. GE Healthcare)"),
      uiOutput("manufacturer_selectInput"),
      
      hr(),
      
      helpText(strong("Medical Device "), "(e.g. infusion pump)"),
      uiOutput("medDev_selectInput"),
      
      hr(),
      
      # Metadata and Disclaimer
      helpText(strong("Metadata")),
      helpText("License: ", a("http://open.fda.gov/license",
                              href="http://open.fda.gov/license", target="_blank")),
      helpText("API reference: ", a("https://open.fda.gov/api/reference/",
                                    href="https://open.fda.gov/api/reference/", target="_blank")),
      helpText("Last update: ", medDevices$meta$last_updated),
      helpText(strong("Disclaimer")),
      helpText(medDevices$meta$disclaimer)
        
    ),  # close sidebarPanel
    
    mainPanel(
      
      # if request code is 404 show an error message, otherwise print a time series
      h4("Medical Device Reports received by the FDA"),
      htmlOutput("gVisTimeSeriesView"),
      
      h4("Where are reported medical devices manufactured?"),
      htmlOutput("gVisBarPlotCountriesView"),
      
      h4("Outcomes associated with the adverse event"),
      htmlOutput("gVisBarPlotEventTypeView"),
      
      h4("Most frequently reported Medical Devices"),
      helpText(em("Note: ASKU means ASKed but Unaivailable")),      
      htmlOutput("gVisBarPlotDeathView"),
      htmlOutput("gVisBarPlotInjuryView"),
      htmlOutput("gVisBarPlotMalfunctionView"),
      
      h4("Results"),
      dataTableOutput("tableView")

    )
    
  )  # close sidebarLayout
  
)  # close fluidPage

)  # close ui function

shinyApp(ui = ui, server = server)


# Reference ---------------------------------------------------------------

# MAUDE database - http://www.accessdata.fda.gov/scripts/cdrh/cfdocs/cfmaude/search.cfm
# openFDA API basics - https://open.fda.gov/api/reference/
# openFDA API devices - https://open.fda.gov/device/event/
# openFDA API devices reference - https://open.fda.gov/device/event/reference/
