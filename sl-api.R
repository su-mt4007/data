################################################################ 
#' # Trafiklab API query to get data for homework 3
#'
#' @author Michael Höhle, 2019-11-17
##############################################################################

# Load packages 
library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)

# Read api key from a safe place. Note: This is not reproducible code as the access key
# is stored outside the git. If you want to run this code yourself you need to register 
# at https://www.trafiklab.se/
# Warning: Always be carefult not to store your access credentials as part of a public git!
# Also note that the keys are stored as part of the URL query, so make sure to
# remove them from any response objects.
api_key_hl <- readLines("~/Secrets/Trafiklab/stop.key")
api_key_reseplan <- readLines("~/Secrets/Trafiklab/reseplaneren.key")
api_key_gtfs_static <- readLines("~/Secrets/Trafiklab/gtfs-static.key")

#' Make and parse a query to the Stopplatser & Linier API
query_slhl_api <- function(model, api_key) {
    response <- GET(url = paste0("https://api.sl.se/api2/LineData.json?model=",model,"&key=",api_key))
    
    #Remove key from URL, if this would be stores somewhere.
    #response$url <- ""
    #response$request$url <- ""
    
    response_parsed <- fromJSON(content(response, as="text"))
    response_parsed
    
    return(response_parsed$ResponseData$Result)
}

# query_gtfs_static <- function(api_key) {
#   url <- paste0("https://opendata.samtrafiken.se/sl/sl.zip?key=", api_key)
#   response <- GET(url)
#   response
#   response_parsed <- fromJSON(content(response, as="text"))
#   ##Store as ZIP??
# }

query_reseplaneren <- function(api_key) {
  fromId <- sites %>% filter(SiteName == "Albano") %>% slice(1) %>% pull(SiteId)
  toId <- sites %>% filter(SiteName == "Medborgarplatsen") %>% slice(1) %>% pull(SiteId)
  
  parameters <- list(lang="en", originExtId=fromId, destExtId=toId, passlist=1)
  parameters_str <- paste0(names(parameters), "=", parameters, collapse="&")
  url <- paste0("https://api.sl.se/api2/TravelplannerV3_1/trip.json?key=",api_key,"&", parameters_str)
  response <- GET(url)
  response
  response_parsed <- fromJSON(content(response, as="text"))
  # names(response_parsed$Trip)
  # response_parsed$Trip$duration
  # dim(response_parsed$Trip$LegList$Leg[[1]])
  response_parsed$Trip
}




query_data <- function() {
  lines <- query_slhl_api(model = "Line", api_key=api_key_hl)
  journeyPatterns <- query_slhl_api(model = "JourneyPattern", api_key=api_key_hl)
  stopAreas <- query_slhl_api(model = "StopAreas", api_key=api_key_hl)
  stopPoints <- query_slhl_api(model = "StopPoint", api_key=api_key_hl)
  transportmodes <- query_slhl_api(model = "Transportmode", api_key=api_key_hl)
  sites <- query_slhl_api(model = "Site", api_key=api_key_hl)
  Sys.time() # "2019-11-17 13:44:59 CET"
  
  ##Store as part of an SQL database
  library(RSQLite)
  unlink("sl-api.sqlite")
  mydb <- dbConnect(RSQLite::SQLite(), "sl-api.sqlite")
  tables <- c("lines", "journeyPatterns", "stopAreas","stopPoints", "transportmodes","sites")
  walk(tables, ~ dbWriteTable(mydb, .x, get(.x)))
  dbDisconnect(mydb)
  
  ## Make the query to the reseplan
  trips <- query_reseplaneren(api_key_resaplan)
  time_of_reseplaneren_query <- Sys.time() # "2019-11-17 14:00:54 CET"   "2019-11-17 21:45:05 CET"
  save(file="reseplaner-2019-11-17.RData", list=c("trips","time_of_reseplaneren_query"))
  
  invisible()
}

