setwd("/Users/chungchenran/Documents/Google_Certificate/Coding_Club_R_Tutorial2/CC-12-Webscraping-master")

install.packages("rvest") # To import a html file
install.packages("dplyr") # To use pipes

library(rvest)
library(dplyr)

Penguin_html <- readLines("Aptenodytes forsteri (Emperor Penguin).html")

grep("Scientific Name:", Penguin_html)

Penguin_html[131:135]

# Double check the line containing the scienific name.
grep("Scientific Name:", Penguin_html)
Penguin_html[131:135]
Penguin_html[133]

# Isolate line in new vector
species_line <- Penguin_html[133]

## Use pipes to grab the text and get rid of unwanted information like html tags
species_name <- species_line %>%
  gsub("<td class=\"sciName\"><span class=\"notranslate\"><span class=\"sciname\">", "", .) %>%  # Remove leading html tag
  gsub("</span></span></td>", "", .) %>%  # Remove trailing html tag
  gsub("^\\s+|\\s+$", "", .)  # Remove whitespace and replace with nothing
species_name

grep("Common Name", Penguin_html)
Penguin_html[130:160]
Penguin_html[150:160]
Penguin_html[151]

common_name <- Penguin_html[151] %>%
  gsub("<td>", "", .) %>%
  gsub("</td>", "", .) %>%
  gsub("^\\s+|\\s+$", "", .)
common_name

grep("Red List Category", Penguin_html)
Penguin_html[179:185]
Penguin_html[182]

red_cat <- gsub("^\\s+|\\s+$", "", Penguin_html[182])
red_cat

grep("Date Assessed:", Penguin_html)
Penguin_html[192:197]
Penguin_html[196]

date_assess <- Penguin_html[196] %>%
  gsub("<td>", "",.) %>%
  gsub("</td>", "",.) %>%
  gsub("\\s", "",.)
date_assess

iucn <- data.frame(species_name, common_name, red_cat, date_assess)

download.file("http://www.iucnredlist.org/details/22697752/0", "Emperor_Penguin.html")

url <- "https://www.iucnredlist.org/species/22697752/157658053"

webpage <- read_html(url)

links <- webpage %>% html_nodes("a") %>% html_attr("href")

webpage_vector <- webpage %>% readLines()

# https://statsandr.com/blog/web-scraping-in-r/

link <- "https://www.nytimes.com/"

NYT_page <- read_html(link)

summaries_css <- NYT_page %>%
  html_elements(css = ".summary-class")

#site-content > div > div.smartphone.tablet > div > div:nth-child(1) > div > div > div > div.css-1171xf4.e1ppw5w20 > section > div.css-1h1983p.e1yccyp20 > section > div > div.css-1432v6n.e17qa79g0 > div > section > a > div > p
#//*[@id="site-content"]/div/div[1]/div/div[1]/div/div/div/div[1]/section/div[2]/section/div/div[1]/div/section/a/div/p
#//*[@id="site-content"]/div/div[1]/div/div[1]/div/div/div/div[1]/section/div[2]/section/div/div[1]/div/section/a/div/p/text()
#//*[@id="site-content"]/div/div[1]/div/div[1]/div/div/div/div[1]/section/div[2]/section/div/div[1]/div/section/a/div/p
#class="summary-class css-dcsqcp"

summaries_xpath <- NYT_page %>%
  html_elements(xpath = "//*[contains(@class, 'summary-class')]")

NYT_summaries_css <- html_text(summaries_css)
NYT_summaries_xpath <- html_text(summaries_xpath)

link <- "https://en.wikipedia.org/wiki/List_of_Formula_One_drivers"

page <- read_html(link)

drivers_F1 <- html_element(page, "table.sortable") %>%
  html_table()

head(drivers_F1) # first 6 rows

tail(drivers_F1) # last 6 rows

str(drivers_F1) # structure of the dataset

drivers_F1 <- drivers_F1[c(1:4, 7:9)] # select variables

drivers_F1 <- drivers_F1[-nrow(drivers_F1), ] # remove last row

drivers_F1$`Drivers' Championships` <- substr(drivers_F1$`Drivers' Championships`,
                                              start = 1, stop = 1
)

write.csv(drivers_F1, "F1_drivers.csv", row.names = FALSE)

library(tidyverse)

drivers_F1 %>%
  group_by(Nationality) %>%
  summarise(championship_country = sum(as.double(`Drivers' Championships`))) %>%
  arrange(desc(championship_country))

drivers_F1 %>%
  group_by(`Driver name`) %>%
  summarise(championship_pilot = sum(as.double(`Drivers' Championships`))) %>%
  arrange(desc(championship_pilot))

drivers_F1 %>%
  filter(`Pole positions` > 1) %>%
  ggplot(aes(x = as.double(`Pole positions`), y = as.double(`Drivers' Championships`))) +
  geom_point(position = "jitter") +
  labs(y = "Championships won", x = "Pole positions") +
  theme_minimal()

# R API tutorial (https://www.dataquest.io/blog/r-api-tutorial/)
library(httr)
library(jsonlite)

res = GET("https://api.open-notify.org/astros.json")

# Accessing REST API using R (https://www.geeksforgeeks.org/accessing-rest-api-using-r-programming/)

# Installing the packages
install.packages("httr")
install.packages("jsonlite")

# Loading packages
library(httr)
library(jsonlite)

# Initializing API Call
call <- "http://www.omdbapi.com/?i=tt3896198&apikey=948d3551&plot=full&r=json"

# Getting details in API
get_movie_details <- GET(url = call)

# Getting status of HTTP Call
status_code(get_movie_details)

# Content in the API
str(content(get_movie_details))

# Converting content to text
get_movie_text <- content(get_movie_details,
                          "text", encoding = "UTF-8")
get_movie_text

# Parsing data in JSON
get_movie_json <- fromJSON(get_movie_text, 
                           flatten = TRUE)
get_movie_json

# Converting into dataframe
get_movie_dataframe <- as.data.frame(get_movie_json)

