
# Load required packages --------------------------------------------------

library(jsonlite)
library(ggplot2)


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
medDevices_list <- medDevices$results$term
manufacturers_list <- manufacturers$results$term


# server Fuction ----------------------------------------------------------

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
    })
  
  ### output$startDate <- renderText({input$dateRange[1]})
  ### output$endDate   <- renderText({input$dateRange[2]})
  
  # create the string for the API call (it's a reactive expression, NOT a renderText) 
  api_call <- reactive({
    
    str0 <- paste0("search=date_received:[", input$dateRange[1], "+TO+", input$dateRange[2], "]")
    
    if (input$manufacturer_checkboxInput == TRUE)
      str1 <- paste0("+AND+device.manufacturer_d_name:", input$chosen_manufacturer)
    else str1 <- ""
    
    if (input$medDev_checkboxInput == TRUE)
      str2 <- paste0("+AND+device.generic_name:", input$chosen_medDev)
    else str2 <- ""
    
    str3 <- "&count=date_received"
    
    # string for the API call
    paste0(api_open_request, api_endpoint, str0, str1, str2, str3)    
    })
  
  # DEBUG
  output$renderPrint1 <- renderPrint({input$dateRange[1]})
  output$renderPrint2 <- renderPrint({input$dateRange[2]})
  
  
  ##############################################################
  # make the API call (TO BE FIXED)
  
  api_response <- reactive({ fromJSON(api_call()) })
  
  df <- reactive({
    df_tmp <- api_response()$results
    date_posix <- strptime(df_tmp$time, "%Y%m%d")
    received_by_fda <- as.Date(date_posix, "%Y-%m-%d")
    data.frame(dates = received_by_fda, reports = df_tmp$count)
    })
  
  output$df_from_API_call <- renderTable({ api_response <- fromJSON(api_call())
                                        head(api_response$results) })
  # DEBUG
#  output$df_from_API_call <- reactive({ api_response <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2015-01-01]+AND+device.generic_name:x-ray&count=date_received")
 #                                       class(api_response$results) })
#  values$time_series <- fromJSON(api_call())
#   time_series <- fromJSON(renderText({"api_call"}))
#   date_posix <- strptime(time_series$results$time, "%Y%m%d")
#   received_by_fda <- as.Date(date_posix, "%Y-%m-%d")
#   output$df <- data.frame(dates = received_by_fda, reports = time_series$results$count)
  ##############################################################
  
  # create dynamically the medDev_helpText UI component
  output$medDev_helpText <- renderUI({
    if (input$medDev_checkboxInput == TRUE)
      helpText(strong("Medical Device "), "(e.g. infusion pump)")
    })
    
  # create dynamically the medDev_selectInput UI component
  output$medDev_selectInput <- renderUI({
    searchString <- toupper("")
    medDev_RegEx <- sort(unique(
      medDevices_list[grep(pattern = searchString, x = medDevices_list, ignore.case = TRUE)]))
    if (input$medDev_checkboxInput == TRUE)
      selectInput(inputId = "chosen_medDev", label = "",
                  choices = medDev_RegEx, selected = medDev_RegEx[1])
    })
  
  # create dynamically the manufacturer_helpText UI component
  output$manufacturer_helpText <- renderUI({
    if (input$manufacturer_checkboxInput == TRUE)
      helpText(strong("Manufacturer"), "(e.g. GE Healthcare)")
    })
  
  # create dynamically the manufacturer_selectInput UI component
  output$manufacturer_selectInput <- renderUI({
    searchString <- toupper("")
    manufacturer_RegEx <- sort(unique(
      manufacturers_list[grep(pattern = searchString, x = manufacturers_list, ignore.case = TRUE)]))
    if (input$manufacturer_checkboxInput == TRUE)
      selectInput(inputId = "chosen_manufacturer", label = "",
                  choices = manufacturer_RegEx, selected = manufacturer_RegEx[1])
    })

  # summary string about the results from the API call
  output$summaryView <- renderPrint({
    max_index <- which.max(df()$reports)
    paste0("The maximum number of reports received by the FDA in a single day is ", df()$reports[max_index],
           ", and they were received on " , df()$dates[max_index])
    })

  # plot the time series of the reports across the selected date range
  output$timeSeriesView <- renderPlot({
    ggplot(data = df(), aes(x = dates, y = reports)) +
      geom_line() +
      labs(x = "Time", y = "Adverse Event Reports") +
      ggtitle(paste0("Reports received by the FDA from ", input$dateRange[1], " to ", input$dateRange[2]))
    })
  
} # close server function


# User Interface ----------------------------------------------------------

ui <- shinyUI(fluidPage(
  
  # load the CSS (try with a different CSS file)
  # theme = "bootstrap.css",
  
  titlePanel("Hello Shiny (titlePanel)"),
  
  # global layout of the shiny application
  sidebarLayout(position = "left",
    
    # sidebar panel
    sidebarPanel(
      
      helpText(strong("Date Range")),
      helpText(em("Format: yyyy-mm-dd")),
      dateRangeInput("dateRange", label = "", start = Sys.Date() - 365, end = Sys.Date()),
      
      hr(),
      
      checkboxInput("manufacturer_checkboxInput", "Choose a specific Manufacturer", value = FALSE),
      # select the manufacturer (if checkbox is checked)
      uiOutput("manufacturer_helpText"),
      uiOutput("manufacturer_selectInput"),
      
      hr(),
      
      checkboxInput("medDev_checkboxInput", "Choose a specific Medical Device", value = FALSE),
      # select the medical device (if checkbox is checked)
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
      helpText(medDevices$meta$disclaimer),
      
      uiOutput("renderPrint1"), # start date
      uiOutput("renderPrint2"), # end date
      uiOutput("startDate"),
      uiOutput("endDate")
      
    ),  # close sidebarPanel
    
    mainPanel(
      h4("Summary"),
      textOutput("summaryView"),
      
      h4("Time Series"),
      plotOutput("timeSeriesView"),
      
      tableOutput("df_from_API_call") # DEBUG
      
      ### plotOutput("distPlot")
    )
    
  )  # close sidebarLayout
  
)  # close fluidPage

)  # close ui function

shinyApp(ui = ui, server = server)