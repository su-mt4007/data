library(tidyverse)
library(jsonlite)

#' Script to fetch music from the API
#' 
get_SR_music <- function(channel, date = Sys.Date(), size = 500){
    # Grabs music played on Swedish Radio channel from api.sr.se.
    #
    # API call, see http://sverigesradio.se/api/documentation/v2/metoder/musik.html
    url <- paste("http://api.sr.se/api/v2/playlists/getplaylistbychannelid?id=", channel, 
                 "&startdatetime=", date,"&format=json", "&size=", size,
                 sep = "")
    fromJSON(url)[[2]] %>% 
        # Fix date/time format
        mutate(start_time = as.POSIXct(as.numeric(substr(starttimeutc, 7, 16)), origin = "1970-01-01"),
               stop_time = as.POSIXct(as.numeric(substr(stoptimeutc, 7, 16)), origin = "1970-01-01")) %>% 
        select(-starttimeutc, -stoptimeutc)
}