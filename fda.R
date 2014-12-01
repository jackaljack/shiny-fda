library(shiny)
library(jsonlite)
library(ggplot2)
suppressPackageStartupMessages(library(googleVis))

runApp("maudeApp")

# see here: https://open.fda.gov/api/reference/

api_open_request <- "https://api.fda.gov/"
api_endpoint <- "device/event.json?"
api_key = "api_key=12345"

### str1 <- "https://api.fda.gov/drug/event.json?api_key="

# date_received is the date the report was received by the FDA
date_start <- "1991-01-01" # date picker
date_end <- "2015-01-01" # date picker
search_dates <- paste0("date_received:[", date_start, "+TO+", date_end, "]")

# search a medical device, a drug, or a particular kind of food (regular expression)
search_item <- "x-ray"
  
str2 <- "search=date_received:[19910101+TO+20150101]+AND+device.generic_name:x-ray"
str3 <- "&count=date_received"
###str4 <- "&limit=10"
# use 'skip' in combination with 'limit' to paginate results.
###str5 <- "&skip=0" # skip=0, 10, 20, 30, etc...
###str6 <- "&skip=100"

api_call <- paste0(str0, str2, str4, str5)
fda <- fromJSON(api_call)

# if there are more than 1000 results from an API call, we have to make more calls and get a list
# for each call and merge all of them in a single data frame afterwards.
df <- as.data.frame(rbind(fda$results, fda2$results))

# metadata
total_results <- fda$meta$results$total
disclaimer <- fda$meta$disclaimer
license <- fda$meta$license
last_update <- fda$meta$last_updated

exact_match <- "glucose"
api_call2 <- paste0(str0, "search=date_received:[19910101+TO+20150101]+AND+device.generic_name:", "\"", exact_match, "\"", str4) 
fda <- fromJSON(api_call2)

fda0 <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[19910101+TO+20150101]+AND+device.generic_name:x-ray&count=device.manufacturer_d_country.exact")

fda$results$device[[1]]$generic_name

fda <- fromJSON("https://api.fda.gov/device/enforcement.json?&count=report_date")
fda$results$recalling_firm

fda <- fromJSON("https://api.fda.gov/device/event.json?&count=device.generic_name.exact&limit=30")
ggplot(fda$results, aes(x = term, y = count)) +
  geom_bar(stat="identity", fill="lightblue", colour="black", na.rm = FALSE) + 
  geom_text(aes(label = count), hjust=-0.2) +
  labs(x="Medical Devices", y="Frequency") +
  coord_flip() + 
  ggtitle("This is a title")


fda <- fromJSON("https://api.fda.gov/device/event.json?search=device.generic_name:x-ray&limit=30")
# total results
fda$meta$results$total


# create empty lists to store medical devices identification data
product_code <- character()
generic_name <- character()
brand_name <- character()
manufacturer_name <- character()
manufacturer_country <- character()
device_operator <- character()

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
  device_operator <- append(device_operator, fda$results$device[[i]]$device_operator)

}

# create the data frame from the previously generated lists
devices <- data.frame(productCode = product_code,
                      genericName = generic_name,
                      brandName = brand_name,
                      manufacturerName = manufacturer_name,
                      manufacturerCountry = manufacturer_country,
                      deviceOperator = device_operator,
                      receivedByFDA = received_by_fda)

###############################
# cosa altro metto nel dataframe
fda$results$device[[1]]$implant_flag # FORSE
fda$results$device[[1]]$manufacturer_d_state # FORSE
fda$results$device[[1]]$device_availability # CREDO, ma riguarda
fda$results$device[[1]]$device_event_key # NON CREDO
fda$results$device[[1]]$device_evaluated_by_manufacturer # FORSE
###############################


dim(devices)[1]

barplot(summary(devices$manufacturerCountry))

ggplot(devices, aes(x = manufacturerCountry, )) +
  geom_bar(stat="bin", fill="blue", colour="black", na.rm = FALSE) + 
  labs(x="Medical Devices", y="Frequency") +
  coord_flip() + 
  ggtitle("This is a title")


ggplot(devices, aes(x = productCode, )) +
  geom_bar(stat="bin", fill="red", colour="black", na.rm = FALSE) + 
  labs(x="Medical Devices", y="Frequency") +
  coord_flip() + 
  ggtitle("This is a title")

ggplot(devices, aes(x = brandName, )) +
  geom_bar(stat="bin", fill="red", colour="black", na.rm = FALSE) + 
  labs(x="Medical Devices", y="Frequency") +
  coord_flip() + 
  ggtitle("This is a title")

ggplot(devices, aes(x = manufacturerCountry, )) +
  geom_bar(stat="bin", fill="blue", colour="black", na.rm = FALSE) + 
  facet_grid(productCode ~ .) +
  labs(x="Medical Devices", y="Frequency") +
  coord_flip() + 
  ggtitle("This is a title")

ggplot(devices, aes(x = manufacturerName, fill = manufacturerCountry)) +
  geom_bar(stat="bin", na.rm = FALSE) + 
  labs(x="Medical Devices", y="Frequency") +
  facet_grid(productCode ~ .) +
  coord_flip() + 
  ggtitle("This is a title")


ggplot(fda0$results, aes(x = term, y = count)) +
  + geom_bar(stat = "identity", fill = "red", colour = "black", na.rm = FALSE) + 
  + coord_flip()

# googleVis bar chart
fda00 <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+1999-12-31]+AND+device.generic_name:x-ray&count=event_type.exact")
# potrei rinominare count in count_anno, cosi' avrei un dataframe con tutti i dati per tutti gli anni
Bar <- gvisBarChart(fda00$results, options = list(
  title="Adverse events for 'X-ray'",
  vAxis="{title:'event type'}",
  hAxis="{title:'adverse events'}"))
plot(Bar)

# puoi anche aggiungere la parte di stringa +AND+device.generic_name:x-ray a search=
product_code <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=device.device_report_product_code.exact")
generic_name <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=device.generic_name.exact")
brand_name <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=device.brand_name.exact")
manufacturer_name <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=device.manufacturer_d_name.exact")
manufacturer_country <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=device.manufacturer_d_country.exact")
### device_operator <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=device.device_operator.exact") ### troppo sporco
### patient_sequence_number_outcome <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=patient.sequence_number_outcome.exact") ### e' un casino, meglio evitare
source_type <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=source_type.exact")
event_type <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=event_type.exact")
### event_location <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=event_location.exact") ### praticamente inutile, dati incompleti
report_source_code <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=report_source_code.exact")
reporter <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+2013-12-31]&count=reporter_occupation_code.exact")

# per chiamate "combinate" devo usare AND e devo fare una chiamata per ogni combinazione
hospital_AND_death <- fromJSON("https://api.fda.gov/device/event.json?search=event_location:hospital+AND+event_type:death&count=device.generic_name.exact")

time_series <- fromJSON("https://api.fda.gov/device/event.json?search=date_received:[19910101+TO+20150101]+AND+device.generic_name:x-ray&count=date_received")
date_posix2 <- strptime(time_series$results$time, "%Y%m%d")
received_by_fda2 <- as.Date(date_posix2, "%Y-%m-%d")
df <- data.frame(dates = received_by_fda2, reports = time_series$results$count)
ggplot(data = df, aes(x = dates, y = reports)) +
  geom_line() +
  labs(x="Year", y="Reports") +
  ggtitle("This is a title")

# NOTA: il 18 agosto del 2009 ci sono stati moltissimi report per x-ray (979). Perché?
df[1988,]

# troviamo le date in cui sono stati riportati più di 250 reports per 'x-ray'
xRay_250 <- which(df$reports > 250)
# sono quasi tutti nel 2009, e qualcuno nel 2008. Probabilmente questo fatto è legato a questo:
# http://www.fda.gov/radiation-emittingproducts/radiationemittingproductsandprocedures/medicalimaging/medicalx-rays/ucm115354.htm
# Nel 2009 è uscito un report sulle radiazioni ionizzanti a cui sono sottoposti i cittadini USA
# http://www.ncrponline.org/Publications/Press_Releases/160press.html
df[xRay_250,]

Bar <- gvisBarChart(generic_name$results[1:10, 1:2], options = list(
  title="Adverse events",
  vAxis="{title:'manufacturers'}",
  hAxis="{title:'adverse events'}"))
plot(Bar)

# WARNING!
# con count .exact restituisce di deafult 100 elementi. Se ne voglio di più posso impostare limit,
# ma in ogni caso non mi restituisce più di 1000 risultati. In pratica non credo sia un problema, perché
# i 1000 risultati più frequenti negli anni 1999-2014 dovrebbero coprire quasi tutti i dispositivi medici esistenti
# Però è bene dire che c'è questo problema
# NOTA: è un po' lenta la chiamata, ma tanto la uso per estrarre il disclaimer, la licenza, il last update,
# quindi tanto la dovrei fare in ogni caso questa chiamata alle API della FDA.
# La data inziale i cui la FDA ha iniziato a raccogliere questi dati dovrebbe essere il 1991, quindi è fissa
# Uso Sys.Date() per ottenere la data di oggi come data finale
med_devices <- fromJSON(paste0("https://api.fda.gov/device/event.json?search=date_received:[1991-01-01+TO+", Sys.Date(),"]&count=device.generic_name.exact&limit=1000"))

med_devices$meta$disclaimer
med_devices$meta$license
med_devices$meta$last_updated

# estraggo la lista dei 1000 medical devices più frequenti perché mi serve per la regular expression sul
# selectInput che scrivo a mano (vedi app shiny SpeseDM)
med_devices_list <- med_devices$results$term
searchString <- "x-ray"
dmRegExpr <- sort(unique(med_devices_list[grep(pattern = searchString, x = med_devices_list, ignore.case = TRUE)]))
