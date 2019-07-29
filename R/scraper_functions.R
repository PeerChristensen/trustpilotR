#' Functions for scraping data
#'
#'
#' @param base_url url provided by user
#'
#' @export
#'
#' @import rvest
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @import jsonlite


### FUNCTION: GET N PAGES ###

n_pages <- function(base_url){

  base_url                         %>%
    xml2::read_html()              %>%
    html_nodes(".pagination-page") %>%
    html_text()                    %>%
    as.numeric()                   %>%
    max(na.rm=T)
}

### FUNCTION: GET ALL PAGES ###

get_pages <- function(base_url){

  last_page    = n_pages(base_url)
  page_numbers = seq_along(1:last_page)
  urls         = paste0(base_url,"?page=", page_numbers)
}

### FUNCTION: GET DATE ###

get_date <- function(html) {

  html %>%
    html_nodes(".review-card [data-initial-state='review-dates']") %>%
    html_text() %>%
    map(fromJSON) %>%
    map_chr(1) %>%
    lubridate::date()
}
### FUNCTION: GET INFO ###

get_info <- function(html) {

  html %>%
    html_nodes("[data-initial-state='review-info']") %>%
    html_text() %>%
    map_df(fromJSON)
}

### FUNCTION: GET RATING ###

# get_rating <- function(html) {
#
#   html                         %>%
#     html_nodes(".content-section__review-info .star-rating") %>%
#     html_attrs()               %>%
#     str_extract("[:digit:]")   %>%
#     as.numeric()
# }

### FUNCTION: GET REVIEW TEXT ###

get_review_text <- function(html) {

  html %>%
    html_nodes(".review-content__text") %>%
    html_text(trim=T)
}
