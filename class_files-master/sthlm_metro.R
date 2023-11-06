library(tidyverse)
Line <- read_csv("../HW_data_HT18/Line.csv") %>% 
    filter(DefaultTransportModeCode == "METRO") %>% 
    select(LineNumber, LineName = DefaultTransportMode)
LinePlatform <- read_csv("../HW_data_HT18/JourneyPattern.csv") %>%
    filter(LineNumber %in% Line$LineNumber) %>% 
    mutate(PlatformNumber = as.numeric(JourneyPatternPointNumber)) %>% 
    select(LineNumber, PlatformNumber, Direction = DirectionCode)
Platform <- read_csv("../HW_data_HT18/StopArea.csv") %>% 
    filter(StopPointNumber %in% LinePlatform$PlatformNumber) %>% 
    select(PlatformNumber = StopPointNumber,
           StationName = StopPointName,
           Longitude = LocationEastingCoordinate,
           Latitude = LocationNorthingCoordinate)
library(RSQLite)
con <- dbConnect(SQLite(), "sthlm_metro.sqlite")
dbWriteTable(con, "Line", Line, overwrite = TRUE)
dbWriteTable(con, "LinePlatform", LinePlatform, overwrite = TRUE)
dbWriteTable(con, "Platform", Platform, overwrite = TRUE)



    
