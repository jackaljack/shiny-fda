# required libraries here...


# App settings and first API call (to populate selectInputs) --------------
med_devices <- fromJSON(
  paste0("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+",
         Sys.Date(),
         "]&count=device.generic_name.exact&limit=1000"))

# metadata
med_devices$meta$disclaimer
med_devices$meta$license
med_devices$meta$last_updated

# estraggo la lista dei 1000 medical devices più frequenti
med_devices_list <- med_devices$results$term


# server Fuction ----------------------------------------------------------

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
    })
  
  # text widget per limitare la ricerca del DM con una regular expression (PER ADESSO NON SERVE)
  output$value <- renderPrint({
    input$med_dev_textInput
    })
  
  # create dynamically the selectInput to choose the medical device on the basis of the textInput string
  output$med_dev_selectInput <- renderUI({
    # get the string in the textInput and use it for RegEx pattern matching
    searchString <- toupper(input$med_dev_textInput)
    med_dev_RegEx <- sort(unique(
      med_devices_list[grep(pattern = searchString, x = med_devices_list, ignore.case = TRUE)]))
    selectInput(inputId = "chosen_med_dev", label = "",
                choices = med_dev_RegEx, selected = med_dev_RegEx[1])
    })
  
} # close server function


# User Interface ----------------------------------------------------------

ui <- shinyUI(fluidPage(
  
  # load the CSS
  # theme = "bootstrap.css",
  
  titlePanel("Hello Shiny (titlePanel)"),
  
  ### define the header of the application
  ### headerPanel("Header (brutto, meglio titlePanel)"),
  
  # global layout of the shiny application
  sidebarLayout(position = "left",
    
    # sidebar panel
    sidebarPanel(
      
      # textInput per restringere la ricerca dei DM
      helpText("Look for a specific ", strong("Medical Device")),
      helpText(em("Keyword (e.g. x-ray, pump, bed)")),
      textInput("med_dev_textInput", label = "", value = ""),
      # populate the selectInput dynamically (see server function)
      uiOutput("med_dev_selectInput"),
      
      hr(),
      
      helpText(strong("Date Range")),
      helpText(em("Format: yyyy-mm-dd")),
      dateRangeInput("dateRange", label = "", start = Sys.Date() - 365, end = Sys.Date()),
      
      # Metadata and Disclaimer
      hr(),
      helpText(strong("Metadata")),
      helpText("License: ", a("http://open.fda.gov/license",
                              href="http://open.fda.gov/license", target="_blank")),
      helpText("Last update: ", med_devices$meta$last_updated),
      helpText(strong("Disclaimer")),
      helpText(med_devices$meta$disclaimer)
      
    ),  # close sidebarPanel
    
    # main panel
    mainPanel(
      ### plotOutput("distPlot")
    )
    
  )  # close sidebarLayout
  
)  # close fluidPage

)  # close ui function

shinyApp(ui = ui, server = server)