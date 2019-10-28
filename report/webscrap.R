library(rvest)
library(quanteda)
library(httr)
library(tidyverse)
library(rebus)
library(lubridate)
library(purrr)

laroche.html <- html("https://www.makeupalley.com/product/showreview.asp/ItemId=48043/Anthelios-Ultra-Light-Face-Sunscreen-SPF-60/La-Roche-Posay/Sunscreen")

laroche.rev <- laroche.html %>% html_nodes(xpath = '//*[@id="product-reviews-data"]/p') %>%
              html_text()

roc = "https://www.makeupalley.com/product/showreview.asp/ItemId=48043/Anthelios-Ultra-Light-Face-Sunscreen-SPF-60/La-Roche-Posay/Sunscreen/page=1/#reviews"

# Create a function to have the number of pages
get_last_page <- function(html){
  pages_data <- html %>% 
    # The '.' indicates the class
    html_nodes('.num') %>% 
    # Extract the raw text as a list
    html_text()                   
  
# The second to last of the buttons is the one
pages_data[(length(pages_data)-1)] %>%            
    # Take the raw string
    unname() %>%                                     
    # Convert to number
    as.numeric()                                     
  }

#Extract reviews
get_reviews <- function(html){
 text <-  html %>% 
    # The relevant tag
    html_nodes('.review-text') %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%                       
    # Convert the list into a vector
    unlist()
 
 text[1:length(text)]
}

#Extract reviewer name
get_reviewer_names <- function(html){
  html %>% 
    html_nodes('.user-name') %>% 
    html_text() %>%
    str_remove("from") %>%
    unlist()
}



get_review_dates <- function(html){
  status <- html %>% 
    html_nodes('.date') %>% 
    html_text() %>%
    print()
}
  
  

# if(str_detect(status, "days")){
#   unitsd = str_extract(status, start = 1, end = 2)
#   status = Sys.Date() - as.difftime(unitsd, unit = "days")
#   
#   print(status)
#   
#   
#   } else if(str_detect(status, "months")){
#     unitsm = str_extract(status, start = 1, end = 2)
#     status = Sys.Date() - as.difftime(unitsm, unit = "days")
#     
#   } else
#     status
  




#Extract rating
get_number_rating <- function(html){
  
  ratings <-  html %>% 
    html_nodes('.rating-value') %>% 
    html_text() %>% 
    as.numeric() %>%
    unname()
  
  print(ratings[2:length(ratings)])
}


#------------------------------------------------------------#

get_data_table <- function(html, company_name){
  
  # Extract the Basic information from the HTML
  reviews <- get_reviews(html)
  reviewer_names <- get_reviewer_names(html)
  dates <- get_review_dates(html)
  ratings <- get_number_rating(html)
  
  # Combine into a tibble
  combined_data <- tibble(reviewer = reviewer_names,
                          date = dates,
                          rating = ratings,
                          review = reviews) 
  
  # Tag the individual data with the company name
  combined_data %>% 
    mutate(company = company_name) %>% 
    select(company, reviewer, date, rating, review)
}


get_data_from_url <- function(url, company_name){
  html <- read_html(url)
  get_data_table(html, company_name)
}

scrape_write_table <- function(url, company_name){
  
  # Read first page
  first_page <- read_html(url)
  
  # Extract the number of pages that have to be queried
  latest_page_number <- get_last_page(first_page)
  
  # Generate the target URLs
  list_of_pages <- str_c(url, 'page=', 1:latest_page_number)
  
  # Apply the extraction and bind the individual results back into one table, 
  # which is then written as a tsv file into the working directory
  list_of_pages %>% 
    # Apply to all URLs
    map(get_data_from_url, company_name) %>%  
    # Combine the tibbles into one tibble
    bind_rows() %>%                           
    # Write a tab-separated file
    write_csv(str_c(company_name,'.csv'))     
}

