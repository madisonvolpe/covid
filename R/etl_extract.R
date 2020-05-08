globalVariables(c(".", "link", "link_date"))
#' Scrape COVID-19 Daily Reports from Github
#' @description \code{etl_extract} obtains COVID-19 daily reports uploaded by CSSEGISandData on Github.
#' Arguments, such as month, day, and year let users obtain data for a specified time period.
#' When no arguments are specified then all available data is scraped from github. The downloaded
#' datsets are then saved as csvs in the \emph{raw} folder within the directory that the user specified.
#' If no directory was specified then the files are saved in the \emph{raw} folder within the temp
#' directory that was created.
#' @inheritParams etl::etl_extract
#' @rdname etl_extract.etl_covid
#' @method etl_extract etl_covid
#' @import etl
#' @import dplyr
#' @importFrom lubridate month year
#' @importFrom purrr keep map set_names
#' @importFrom readr write_csv
#' @importFrom  rvest html_nodes html_attr
#' @importFrom stringr str_extract
#' @importFrom  xml2 read_html
#' @importFrom utils head
#' @param month
#' numeric vector specifying month(s)
#' @param day
#' numeric vector specifying day(s)
#' @param year
#' numeric vector specifying year(s)
#' @export
#' @examples
#' covid_dat <- etl::etl("covid")
#' \dontrun{
#' covid_dat %>%
#' etl_extract(month = 4, day = 1:2, year = 2020)
#' }

etl_extract.etl_covid <- function(obj, month, day, year, ...) {

  # Specify URL where covid data is stored
  covid_dailyreports <- "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports"
  # Specify xpath to pull links
  x_path_href <- "//td[@class='content']/span/a"

  # Pull out links to download
  links <- covid_dailyreports %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = x_path_href) %>%
    rvest::html_attr("href") %>%
    purrr::keep(~stringr::str_detect(.x, ".csv$")) %>%
    paste0("https://github.com", .)

  links <- data.frame(link = links, stringsAsFactors = FALSE)
  links <- dplyr::mutate(links, link_date = lubridate::mdy(stringr::str_extract(link, "\\d{2}-\\d{2}-\\d{4}")))

  if(missing(month) & missing(day) & missing(year)){

    links <- links

  } else if (!missing(month) & !missing(day) & !missing(year)){

    links <- dplyr::filter(links, lubridate::month(link_date) %in% month &
                           lubridate::day(link_date) %in% day &
                           lubridate::year(link_date) %in% year)

  } else if (missing(day) & missing(year)) {

    links <- dplyr::filter(links, lubridate::month(link_date) %in% month)

  } else if(missing(month) & missing(day)) {

    links <- dplyr::filter(links, lubridate::year(link_date) %in% year)

  } else if(missing(month) & missing(year)){

    links <- dplyr::filter(links, lubridate::day(link_date) %in% day)

  } else if(missing(day)){

    links <- dplyr::filter(links, lubridate::month(link_date) %in% month &
                             lubridate::year(link_date) %in% year)

  } else if(missing(year)){

    links <- dplyr::filter(links, lubridate::month(link_date) %in% month &
                             lubridate::day(link_date) %in% day)

  } else {

    links <- dplyr::filter(links, lubridate::day(link_date) %in% day &
                             lubridate::year(link_date) %in% year)

  }

  if(nrow(links)>0){
    links
  } else {
    warning('Please enter valid month and/or year')
  }


  list_covid_df <- purrr::map(links$link, links_to_df) %>% purrr::set_names(links$link_date)
  list_covid_df %>% names(.) %>% purrr::map(~readr::write_csv(list_covid_df[[.]], paste0(attr(obj, "raw_dir"),"/",.,".csv")))

  message("Writing the following files as csvs to raw folder in directory specified or raw folder in temp if directory
          was not specified")

  Sys.sleep(5)
  print(list_covid_df)

  invisible(obj)
}

# Internal function

#' Convert a github webpage link to df
#' @param link_df a character string representing a github link
#' @noRd

links_to_df <- function(link_df){

  x_path = "//table[@class = 'js-csv-data csv-data js-file-line-container']"

  df_extracted <-   link_df %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = x_path) %>%
    rvest::html_table() %>%
    as.data.frame() %>%
    purrr::discard(~sum(is.na(.x)) == length(.x))

  df_extracted_titles <- unlist(head(df_extracted, 1))
  names(df_extracted_titles) <- NULL

  df_final <- df_extracted %>%
    dplyr::slice(2:n()) %>%
    purrr::set_names(df_extracted_titles)

  return(df_final)

}

