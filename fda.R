library(shiny)
library(jsonlite)

runApp("shiny-fda")

# see here: https://open.fda.gov/api/reference/

str0 <- "https://api.fda.gov/device/event.json?"
str1 <- "https://api.fda.gov/drug/event.json?api_key="

api_key = "12345"

str2 <- "search=date_received:[19910101+TO+20150101]+AND+device.generic_name:x-ray"
str3 <- "&count=date_received"
str4 <- "&limit=10"
# use 'skip' in combination with 'limit' to paginate results.
str5 <- "&skip=0" # skip=0, 10, 20, 30, etc...

api_call <- paste0(str0, str2, str4, str5)
fda <- fromJSON(api_call)

# how to access generic_name and the other fields? Maybe with a lambda expression?
fda$results$device[[1]]$generic_name
fda$results$device[[1]]$manufacturer_d_name
fda$results$device[[1]]$device_report_product_code
fda$results$device[[1]]$manufacturer_d_country

# metadata
total_results <- fda$meta$results$total
disclaimer <- fda$meta$disclaimer
license <- fda$meta$license
last_update <- fda$meta$last_updated

exact_match <- "glucose"
api_call2 <- paste0(str0, "search=date_received:[19910101+TO+20150101]+AND+device.generic_name:", "\"", exact_match, "\"", str4) 
fda <- fromJSON(api_call2)
fda$results$device[[1]]$generic_name
