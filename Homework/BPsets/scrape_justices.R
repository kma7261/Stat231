library(tidyverse) 
library(rvest)
library(robotstxt)

url <- "https://en.wikipedia.org/wiki/List_of_justices_of_the_Supreme_Court_of_the_United_States"
tables <- url %>%
  read_html() %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)
supreme_justices <- tables[[2]]

write_csv(x = supreme_justices, "~/Github/Stat231/Homework/BPsets/justices.csv")


