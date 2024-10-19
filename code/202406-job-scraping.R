install('rvest')
install('xml2')
install('tidyverse')

library(xml2)
library(rvest)
library(stringr)
library(purrr)

url <- "https://www.jobbank.gc.ca/jobsearch/jobposting/41167220?source=searchresults"
page <- xml2::read_html(url)


page %>%
  html_table(fill = TRUE)
``
xml_child(page, 2) |>
    html_nodes('.job-posting-detail-requirements') |>
  html_text2()


ul_elements <-
  xml_child(page, 2) |>
  html_elements('ul')

ul_elements |>
  html_elements('li') |>
  as.character() |> as_tibble() |> View()
  
xml_child(page, 2) |> 
  html_attr("href")

all_nodes <- html_nodes(xml_child(page, 2), "*") |>
  html_text()

remove_newlines <-
  all_nodes |>
  map_chr(~str_remove_all(., '[\n\t]'))
  
js_pattern <- "<script[^>]*>(.*?)</script>|javascript:|on\\w+\\s*=\\s*\"[^\"]*\"|on\\w+\\s*=\\s*'[^']*'|\\w+\\s*\\(.*?\\);"

remove_js_code <- function(text_vector) {
  # Identify which strings contain JavaScript code
  contains_js <- str_detect(text_vector, js_pattern)
  # Remove the strings that contain JavaScript code
  cleaned_text_vector <- text_vector[!contains_js]
  cleaned_text_vector
}

remove_empty_vectors <- function(text_vector) {
  is_empty <- text_vector == ''
  # Remove the strings that contain JavaScript code
  cleaned_text_vector <- text_vector[!is_empty]
  cleaned_text_vector
}


js_removed <-
  remove_newlines |>
  remove_js_code()

remove_empties <-
  js_removed |>
  remove_empty_vectors() |>
  unique()


remove_empties |> as_tibble() |> View()
