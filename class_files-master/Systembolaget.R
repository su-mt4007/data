###############################################################
## Script to scrape Systembolaget's API
##
## Author: Martin Sköld
###############################################################

library(httr)
library(rvest)
library(tidyverse)
library(xml2)

api_response <- GET("https://www.systembolaget.se/api/assortment/products/xml")
items_xml <- read_xml(api_response)

item_names <- items_xml %>% 
    xml_find_all("//artikel") %>% 
    map(html_children) %>% 
    map(html_name)

names <- reduce(item_names, union)

item_values <- items_xml %>% 
    xml_find_all("//artikel") %>% 
    map(html_children) %>% 
    map(html_text)

replace_NULL <- function(l) map(l, function(x) ifelse(is.null(x), NA, x))

items_df <- map2(item_values, item_names, set_names) %>% 
    transpose(.names = names) %>%
    map(replace_NULL) %>% 
    map(unlist) %>%
    data.frame(stringsAsFactors = FALSE)
    
write_csv(items_df, path = paste("systembolaget", Sys.Date(), ".csv", sep=""))
