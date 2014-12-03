
# Load required packages --------------------------------------------------

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
  
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
    })
    
  # construct the string for the API call (it's a reactive expression, NOT a renderText) 
  api_call <- reactive({
    
    str0 <- paste0("search=date_received:[", input$dateRange[1], "+TO+", input$dateRange[2], "]")
    
    if (input$chosen_manufacturer != "ALL") {
      str1 <- paste0("+AND+device.manufacturer_d_name:", sanitizeString(input$chosen_manufacturer))
    }
    
    else str1 <- ""
    
    if (input$chosen_medDev != "ALL") {
      str2 <- paste0("+AND+device.generic_name:", sanitizeString(input$chosen_medDev))
    }
    else str2 <- ""
    
    str3 <- "&count=date_received"
    
    # string for the API call
    paste0(api_open_request, api_endpoint, str0, str1, str2, str3)    
    })
  
  # make the API call (implement a tryCatch in order to handle HTTP 404 request code)
  api_response <- reactive({ fromJSON(api_call()) })
  
  # extract a data frame from the API response
  df <- reactive({
    df_tmp <- api_response()$results
    date_posix <- strptime(df_tmp$time, "%Y%m%d")
    received_by_fda <- as.Date(date_posix, "%Y-%m-%d")
    data.frame(dates = received_by_fda, reports = df_tmp$count)
    })
  
  # create dynamically the manufacturer_helpText UI component
  output$manufacturer_helpText <- renderUI({
    helpText(strong("Manufacturer"), "(e.g. GE Healthcare)")      
  })
  
  # create dynamically the manufacturer_selectInput UI component
  output$manufacturer_selectInput <- renderUI({
    searchString <- toupper("")
    manufacturer_RegEx <- sort(unique(
      manufacturers_list[grep(pattern = searchString, x = manufacturers_list, ignore.case = TRUE)]))
    selectInput(inputId = "chosen_manufacturer", label = "",
                choices = manufacturer_RegEx, selected = manufacturers_list[1])
    })
    
  # create dynamically the medDev_helpText UI component
  output$medDev_helpText <- renderUI({
    helpText(strong("Medical Device "), "(e.g. infusion pump)")
    })
    
  # create dynamically the medDev_selectInput UI component
  output$medDev_selectInput <- renderUI({
    searchString <- toupper("")
    medDev_RegEx <- sort(unique(
      medDevices_list[grep(pattern = searchString, x = medDevices_list, ignore.case = TRUE)]))
    selectInput(inputId = "chosen_medDev", label = "",
                choices = medDev_RegEx, selected = medDevices_list[1])
    })
  
  # summary string about the results from the API call
  output$summaryView <- renderPrint({
    max_index <- which.max(df()$reports)
    paste0("The maximum number of reports received by the FDA in a single day is ", df()$reports[max_index],
           ", and they were received on " , df()$dates[max_index])
    })

  # plot the time series of the reports across the selected date range (NOT shown)
  output$timeSeriesView <- renderPlot({
    ggplot(data = df(), aes(x = dates, y = reports)) +
      geom_line() +
      labs(x = "Time", y = "Adverse Event Reports") +
      ggtitle(paste0("Reports received by the FDA from ", input$dateRange[1], " to ", input$dateRange[2]))
    })
  
  # plot the time series of the reports across the selected date range
  # Note: googleVis charts are not shown within RStudio
  output$gVisTimeSeriesView <- renderGvis({
    gvisLineChart(data = df(), options = list(
      title="", vAxis="{title:'Reports'}", hAxis="{title:'Time'}"))
    })
  
  # plot a GeoChart showing the areas where the adverse events were occurred
  # Note: googleVis charts are not shown within RStudio
  output$gVisMapView <- renderGvis({
    gvisGeoChart(df())
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
      
      helpText(strong("Date Range")),
      helpText(em("Format: yyyy-mm-dd")),
      dateRangeInput("dateRange", label = "", start = Sys.Date() - 365, end = Sys.Date()),
      
      hr(),
      
      uiOutput("manufacturer_helpText"),
      uiOutput("manufacturer_selectInput"),
      
      hr(),
      
      uiOutput("medDev_helpText"),
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
      
      # h4("Summary"),
      # textOutput("summaryView"),
      
      # h4("Time Series"),
      # plotOutput("timeSeriesView"),
      
      # if request code is 404 show an error message, otherwise print a time series
      h4("Reports received by the FDA"),
      htmlOutput("gVisTimeSeriesView"),
      
      h4("GoogleVis GeoChart"),
      htmlOutput("gVisMapView")

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

