#' Main function at user level
#'
#' @param base_url url provided by user
#' @param page_lim number of pages to parse
#' @param company  company name
#' @param verbose  whether to print current url
#'
#' @export
#'
#' @import rvest
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @import purrr
#' @import xml2
#' @examples
#' get_reviews("https://dk.trustpilot.com/review/trustpilot.com",5,"trustpilot")


### MAIN FUNCTION: PREPARE REVIEW DATASET ###

get_reviews <- function(base_url, page_lim = NULL, company = NULL, verbose = TRUE) {


  ### NESTED FUNCTION ###

  build_dfs <- function(url) {
    Sys.sleep(10)

    # whether to print the current url
    if (verbose == TRUE) {
      message(url)
    }

    # get HTML
    html <- url %>% xml2::read_html()

    # get info
    info <- html %>% get_info()

    # get date
    date <- html %>% get_date()

    # get review
    review <- html %>% get_review_text()
    review <- as.character(review)

    # # gather variables in tibble
    # if (length(id) != length(review)) {
    #   id = NA
    # }
    # if (length(time) != length(review)) {
    #   time = NA
    # }
    # if (length(rating) != length(review)) {
    #   rating = NA
    # }

    info %>%
      tibble::add_column(date = date,review=review) %>%
      select(date,consumerName,stars,review,everything())
  }

  ### START OF MAIN FUNCTION ###

  # if page limit is set, do..
  if (!is.null(page_lim)) {
    #urls <- get_pages(base_url)
    urls <- paste0(base_url,"?page=", seq(1,page_lim))
  }

  else if (is.null(page_lim)) {
    urls <- get_pages(base_url)

  }

  # if company name is set, do..
  if (is.null(company)) {
    data <- urls %>% map(build_dfs)
    data <- do.call("rbind",data)
  }

  # if company name is not set, do..
  else if (!is.null(company)) {
    data <- urls %>% map(build_dfs)
    data <- do.call("rbind",data) %>%
      mutate(company = company)   %>%
      select(company, everything())
  }
}

# example
#base_url = "https://dk.trustpilot.com/review/www.3.dk"
#d = get_reviews(base_url,6,verbose=T)
#d


