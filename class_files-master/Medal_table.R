library(tidyverse)
library(rvest)
library(stringr)
library(purrr)


scrape_medals <- function(year){
    # Scrapes the Wikipedia medal table for a particular winter olympic year, 
    # inspired by http://staff.math.su.se/hoehle/blog/2016/08/21/gapMedal.html
    # The tables are not quite consistent across years, so this only works from
    # 1980 at the time of writing (2018-01-09)
    url <- paste("https://en.wikipedia.org/wiki/", year, "_Winter_Olympics_medal_table", sep = "")
    read_html(url) %>%
        # A list of tables
        html_table(fill = TRUE) %>% 
        # Pick the list with a "Rank"-column
        .[[which(map_lgl(., function(df) names(df)[1] == "Rank"))]] %>% 
        mutate(Year = year) %>% 
        # Remove totals column
        filter(!str_detect(Rank, "Total")) %>% 
        # Remove * for hosting country (column 2 is not consistently named)
        mutate(Country = str_replace(.[[2]], "\\*", "")) %>% 
        # Extract country names
        #mutate(Country = str_sub(Country, 1, -7)) %>% 
        # Select and order columns
        select(Year, Country, Gold, Silver, Bronze, Total)
}

# Scrape for a sequence of years and combine
year_list <- c(1988, 1992, 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022)
winter_medals <- map(year_list, scrape_medals) %>% 
    reduce(bind_rows) %>% 
    mutate(Country = ifelse(Country %in% c("East Germany", "West Germany"), "Germany", Country)) %>% 
    mutate(Country = ifelse(Country == "Unified Team", "Soviet Union", Country)) %>% 
    mutate(Country = ifelse(Country == "Olympic Athletes from Russia", "Russia", Country),
           Country = ifelse(Country == "ROC", "Russia", Country)) %>% 
    group_by(Country, Year) %>% 
    summarise_at(c("Gold", "Silver", "Bronze", "Total"), sum) %>% 
    ungroup()

# Country splits and joins
#
# During the scraped period
# - East and West Germany has joined into Germany
# - Soviet Union has split into 15 countries
# - Czechoslovakia has split into Czech and Slovak republics
# - Yugoslavia has split into 4 countries
#
# The population data is split according to current countries. This
# means we do not have population numbers for East and West Germany,
# hence we choose to join their medal counts. To get population counts
# for the split countries, we aggregate.



# Contains population data 1960-2016
pop_data <- read_csv("https://raw.githubusercontent.com/datasets/population/master/data/population.csv") %>% 
    rename(Country = `Country Name`, Population = Value) %>% 
    # Chance to IOC-style names
    mutate(Country = ifelse(Country == "United Kingdom", "Great Britain", Country)) %>%
    mutate(Country = ifelse(Country == "Russian Federation", "Russia", Country)) %>%
    mutate(Country = ifelse(Country == "Korea, Rep.", "South Korea", Country)) %>% 
    mutate(Country = ifelse(str_detect(Country, "Korea, Dem."), "North Korea", Country)) %>% 
    mutate(Country = ifelse(Country == "Slovak Republic", "Slovakia", Country))

# Predict 2018/2022 population by linear regression on 2012-2016

pop_data_2018 <- pop_data %>% 
    filter(Year %in% 2012:2016) %>% 
    group_by(Country, `Country Code`) %>% 
    summarise(Population = predict(lm(Population ~ Year), newdata = data.frame(Year = 2018))) %>% 
    ungroup() %>% 
    mutate(Year = 2018, Population = round(Population))

pop_data_2022 <- pop_data %>% 
  filter(Year %in% 2012:2016) %>% 
  group_by(Country, `Country Code`) %>% 
  summarise(Population = predict(lm(Population ~ Year), newdata = data.frame(Year = 2022))) %>% 
  ungroup() %>% 
  mutate(Year = 2022, Population = round(Population))
# Join
pop_data <- bind_rows(pop_data, pop_data_2018, pop_data_2022)


# Aggregate to get populations of former unions
Soviet_union <- filter(pop_data, Country %in% c("Russia", "Armenia", "Azerbaijan", "Belarus", "Estonia", 
                                                "Georgia", "Kazakhstan", "Kyrgyzstan", "Latvia", "Lithuania", 
                                                "Moldova", "Tajikistan", "Turkmenistan", "Ukraine","Uzbekistan")) %>% 
    group_by(Year) %>% 
    summarise(Population = sum(Population)) %>% 
    mutate(Country = "Soviet Union", `Country Code` = "USR")
Former_Yugoslavia <- filter(pop_data, Country %in% c("Bosnia and Herzegovina", "Croatia", "Montenegro", "Serbia")) %>% 
    group_by(Year) %>% 
    summarise(Population = sum(Population)) %>% 
    mutate(Country = "Yugoslavia", `Country Code` = "YUG")
Czechoslovakia <- filter(pop_data, Country %in% c("Czech Republic", "Slovak Republic")) %>% 
    group_by(Year) %>% 
    summarise(Population = sum(Population)) %>% 
    mutate(Country = "Czechoslovakia", `Country Code` = "TCH")

pop_data <- bind_rows(pop_data, Soviet_union, Former_Yugoslavia, Czechoslovakia)

winter_medals <- left_join(winter_medals, pop_data, by = c("Country", "Year"))

write_csv(winter_medals, file = paste("Winter_medals", Sys.Date(), ".csv", sep=""))
