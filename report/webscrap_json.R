library(rvest)
library(quanteda)
library(httr)
library(tidyverse)
library(rebus)
library(lubridate)
library(purrr)
library(jsonlite)


# Create a function to have the number of pages
get_last_page <- function(number){
  pages_data <- str_c("https://www.makeupalley.com/product/showreview.asp/ItemId=", number) %>%
    read_html() %>% 
    # The '.' indicates the class
    html_nodes('.num') %>% 
    # Extract the raw text as a list
    html_text()                   
  
  # The second to last of the buttons is the one
  pages_data[(length(pages_data))] %>%            
    # Take the raw string
    unname() %>%                                     
    # Convert to number
    as.numeric()                                     
}

#------------------------------------------------------------#


get_data_from_json <- function(json_path){
  data <- fromJSON(json_path, flatten = T)
  as.data.frame(data)
}

scrape_write_table <- function(number){
  
  # Read first page html
  first_page <- str_c("https://www.makeupalley.com/product/showreview.asp/ItemId=",number) %>%
    read_html()
  
  # Extract the number of pages that have to be queried html
  latest_page_number <- get_last_page(first_page)
  
  # Create first page of json
  json_first_page <- str_c("https://api.makeupalley.com/api/v1/products/", number, "/reviews?page=")
  
  # Generate the target URLs
  list_of_pages <- str_c(json_first_page, 1:latest_page_number)
  
  # Apply the extraction and bind the individual results back into one table, 
  # which is then written as a csv file into the data directory
  list_of_pages %>% 
    # Apply to all URLs
    map(get_data_from_json) %>%  
    # Combine the tibbles into one tibble
    bind_rows() %>% 
    #rename columns 
    rename_at(vars(starts_with("getRecords.")), 
              funs(str_replace(., "getRecords.", ""))) %>%
    # Write a tab-separated file
    write_csv(str_c('data/',number,'.csv'))
}

#------------------------------------------------------------#

#Example belowe
items <- c(48043, 143906, 74107, 160671, 73469, 151181, 19626, 30275, 3123, 
           76243, 195577, 164554)
lapply(items, scrape_write_table)
