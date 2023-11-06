# To be run in the root of a clone of https://github.com/veekun/pokedex.git
library(tidyverse)
library(RSQLite)
con <- dbConnect(SQLite(), "pokedex.sqlite")
add_table <- function(file){
  table <- read.csv(paste0("pokedex/data/csv/", file))
  dbWriteTable(con, str_remove(file, ".csv"), table, overwrite = TRUE)
}
files <- dir("pokedex/data/csv/") %>% .[str_detect(., ".csv")]
walk(files, add_table)
