library(jsonlite)
library(ggplot2)
suppressPackageStartupMessages(library(googleVis))

api_open_request <- "https://api.fda.gov/"
api_endpoint <- "device/event.json?"
api_key = "api_key=12345"

# date_received is the date the report was received by the FDA
date_start <- "1991-01-01"
date_end <- "2015-01-01" 
search_dates <- paste0("search=date_received:[", date_start, "+TO+", date_end, "]")

search_medDev1 <- "x-ray"
search_medDev2 <- "infusion pump"
search_medDev3 <- "glucose"

# remove ',' and replace ' ' with '+' otherwise it won't work
search_manufacturer1 <- "ZIMMER+INC." 
search_manufacturer2 <- "COVIDIEN"
search_manufacturer3 <- "GE+HEALTHCARE"
search_manufacturer4 <- "MEDTRONIC+MINIMED"
search_manufacturer5 <- "BAXTER+HEALTHCARE+PTE.+LTD."
search_manufacturer6 <- "SMITHS+MEDICAL+MD+INC."

search_limit <- "&limit=10"
# use 'skip' in combination with 'limit' to paginate results.
search_skip <- "&skip=0" # skip=0, 10, 20, 30, etc...


# examples of API calls ---------------------------------------------------

# most common manufacturers NAMES for the chosen medical device
api_call_manufacturers <- paste0(api_open_request, api_endpoint, search_dates,
                                 "+AND+device.generic_name:", "\"", search_medDev1, "\"",
                                 "&count=device.manufacturer_d_name.exact",
                                 search_limit, search_skip) 
api_response_manufacturers <- fromJSON(api_call_manufacturers)

# most common manufacturers COUNTRIES for the chosen medical device
api_call_countries <- paste0(api_open_request, api_endpoint, search_dates,
                             "+AND+device.generic_name:", "\"", search_medDev1, "\"",
                             "&count=device.manufacturer_d_country.exact",
                             search_limit, search_skip) 
api_response_countries <- fromJSON(api_call_countries)

# most common medical devices produced by the chosen manufacturer
api_call_medDev <- paste0(api_open_request, api_endpoint, search_dates,
                          "+AND+device.manufacturer_d_name:", "\"", search_manufacturer1, "\"",
                          "&count=device.generic_name.exact",
                          search_limit, search_skip) 
api_response_medDev <- fromJSON(api_call_medDev)

# more ideas for API calls
product_code <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=device.device_report_product_code.exact")
generic_name <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=device.generic_name.exact")
brand_name <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=device.brand_name.exact")
manufacturer_name <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=device.manufacturer_d_name.exact")
manufacturer_country <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=device.manufacturer_d_country.exact")
source_type <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=source_type.exact")
event_type <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=event_type.exact")
report_source_code <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=report_source_code.exact")
reporter <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=reporter_occupation_code.exact") # too many levels
hospital_AND_death <- fromJSON("https://api.fda.gov/device/event.json?search=event_location:hospital+AND+event_type:death&count=device.generic_name.exact")
home_AND_death <- fromJSON("https://api.fda.gov/device/event.json?search=event_location:home+AND+event_type:death&count=device.generic_name.exact")
device_operator <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=device.device_operator.exact") # too dirty
patient_sequence_number_outcome <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=patient.sequence_number_outcome.exact") # too messy
event_location <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=event_location.exact") # rather useless, incompleted data

# interesting comparisons
head(home_AND_death$results)
head(hospital_AND_death$results)


# Some ideas for the plots ------------------------------------------------

fda <- fromJSON(paste0("https://api.fda.gov/device/event.json?", search_dates, "&limit=100"))
# create empty lists to store medical devices identification data
generic_name <- character()
brand_name <- character()
product_code <- character()
manufacturer_name <- character()
manufacturer_country <- character()

# fda$results$device is a list (there can be more than one device involved in a single adverse event),
# so we can append new data to an existing list, and build a data frame afterwards.
for(i in seq(from = 1, to = length(fda$results$device))) {
  
  date_posix <- strptime(fda$results$date_received[i], "%Y%m%d")
  received_by_fda <- as.Date(date_posix, "%Y-%m-%d")
  product_code <- append(product_code, fda$results$device[[i]]$device_report_product_code)
  generic_name <- append(generic_name, fda$results$device[[i]]$generic_name)
  brand_name <- append(brand_name, fda$results$device[[i]]$brand_name)
  manufacturer_name <- append(manufacturer_name, fda$results$device[[i]]$manufacturer_d_name)
  manufacturer_country <- append(manufacturer_country, fda$results$device[[i]]$manufacturer_d_country)

}

# create the data frame from the previously generated lists
medDevReports <- data.frame(
  eventKey = fda$results$event_key,
  eventType = fda$results$event_type,
  eventLocation = fda$results$event_location,
  reportNumber = fda$results$report_number,
  reportSource = fda$results$report_source_code,
  genericName = generic_name,
  brandName = brand_name,
  prodCode = product_code,
  manufName = manufacturer_name,
  manufCountry = manufacturer_country,
  # deviceOperator = device_operator,
  receivedByFDA = received_by_fda)

dim(medDevReports)[1]

ggplot(medDevReports, aes(x = prodCode, )) +
  geom_bar(stat="bin", fill="red", colour="black", na.rm = FALSE) + 
  labs(x="Medical Devices", y="Frequency") +
  coord_flip() + 
  ggtitle("This is a title")

ggplot(medDevReports, aes(x = manufCountry, )) +
  geom_bar(stat="bin", fill="blue", colour="black", na.rm = FALSE) + 
  facet_grid(prodCode ~ .) +
  labs(x="Medical Devices", y="Frequency") +
  coord_flip() + 
  ggtitle("This is a title")

# pattern matching and replacement on manufacturer
str1 <- "ZIMMER, INC."
str1 <- gsub(pattern = ",", replacement = "", x = str1)
str1 <- gsub(pattern = " ", replacement = "+", x = str1)

# pattern matching and replacement on medical device
str2 <- "PUMP, INFUSION, IMPLANTED, PROGRAMMABLE"
str2 <- gsub(pattern = ",", replacement = "", x = str2)
str2 <- gsub(pattern = " ", replacement = "+", x = str2)

# medical devices produced by the chosen manufacturer (put this in a reactive expression, to update selectInput)
api_call <- paste0(api_open_request, api_endpoint, search_dates,
                   "+AND+device.manufacturer_d_name:", "\"", search_manufacturer3, "\"",
                   "&count=device.generic_name.exact",
                   "&limit=1000", search_skip) 
api_response <- fromJSON(api_call)
apimedDevSubset <- api_response$results$term


sanitizeString <- function(inputString){
  inputString <- gsub(pattern = ",", replacement = "", x = inputString)
  sanitized_string <- gsub(pattern = " ", replacement = "+", x = inputString)
  return(sanitized_string)
}

# try/catch for API calls
search_manufacturerNA <- "giveMe404"
api_call_404 <- paste0(api_open_request, api_endpoint, search_dates,
                       "+AND+device.manufacturer_d_name:", "\"", search_manufacturerNA, "\"",
                       "&count=device.generic_name.exact",
                       "&limit=1000", search_skip)
api_response_404 <- fromJSON(api_call_404)

tryCatch(
  stop("you threw an error"), 
  error = function(e) 
  {
    print(e$message) # or whatever error handling code you want
  })

# This simple tryCatch works
tryCatch(
  expr = fromJSON(api_call_404),
  error = function(e) 
  {
    print(paste0(e$message, ": Try with a different combination of manufacturer/medical device"))
    print("Try with a different combination of manufacturer/medical device")
  })


# 2014-12-03 --------------------------------------------------------------

# top10 devices  (*ASKU=ASKed but Unaivailable, show a helpText below)
response <- fromJSON(paste0(api_open_request, api_endpoint, search_dates,
                            "+AND+device.manufacturer_d_country:US",
                            "+AND+event_location:hospital",
                            "+AND+event_type:death",
                            "&count=device.generic_name.exact",
                            "&limit=10"))
reordered_term <- with(response$results, reorder(x = term, X = count))
ggplot(response$results, aes(x = reordered_term, y = count)) +
  geom_bar(stat = "identity", fill = "blue", colour = "black", na.rm = FALSE) +
  coord_flip()

# not ver useful
response <- fromJSON(paste0(api_open_request, api_endpoint, search_dates,
                            "AND+device.manufacturer_d_country:UK", "&count=manufacturer_d_city"))
head(response$results)

# not ver useful
response <- fromJSON(paste0(api_open_request, api_endpoint, search_dates,
                            "&count=number_patients_in_event")) # dividi per event_type, event_location
head(response$results)

# not ver useful
response <- fromJSON(paste0(api_open_request, api_endpoint, search_dates,
                            "&count=number_devices_in_event"))  # dividi per event_type, event_location
head(response$results)
