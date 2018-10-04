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

### FUNCTION: GET TIME ###

get_time <- function(html) {

  time = html             %>%
    html_nodes("time")    %>%
    html_attrs()          %>%
    map(1)                %>%
    unlist()              %>%
    date()

  class = html            %>%
    html_nodes("time")    %>%
    html_attrs()          %>%
    map(2)

  data.frame(time,class)     %>%
    filter(class == "ndate") %>%
    select(time)             %>%
    pull(time)               %>%
    tail(20)
}

### FUNCTION: GET ID ###

get_id <- function(html) {

  html                                    %>%
    html_nodes("consumer-review-picture") %>%
    html_attr("consumer-display-name")
}

### FUNCTION: GET RATING ###

get_rating <- function(html) {

  html                         %>%
    html_nodes(".star-rating") %>%
    html_attrs()               %>%
    str_extract("[:digit:]")   %>%
    as.numeric()               %>%
    tail(20)
}

### FUNCTION: GET REVIEW TEXT ###

get_review_text <- function(html) {

  html                                     %>%
    html_nodes(".review-info__body__text") %>%
    html_text(trim=T)
}
