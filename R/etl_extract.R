#' etl_extract.etl_covid
#' @rdname etl_extract.etl_covid
#' @method etl_extract etl_covid
#' @import etl
#' @import dplyr
#' @importFrom lubridate month year
#' @importFrom purrr keep map
#' @importFrom  rvest html_nodes html_attr
#' @importFrom stringr str_extract
#' @importFrom  xml2 read_html
#' @inheritParams etl::etl_extract
#' @param month
#' fill in
#' @param year
#' fill in
#' @details This function obtains the daily reports of COVID-19 from the John Hopkins CSSE repo.
#' Users can specify the month and year they would like to obtain data for. However,
#' all data is pulled when no arguments are specified. The downloaded dataframes
#' are saved in the directory the user specified.
#' @export

etl_extract.etl_covid <- function(obj, month, year,...) {

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

  if(missing(month) & missing(year)){

    links <- links

  } else {

    links <- dplyr::filter(links, lubridate::month(link_date) %in% month & lubridate::year(link_date) %in% year)

  }

  if(nrow(links)>0){
    links
  } else {
    warning('Please enter valid month and/or year')
  }

  list_covid_df <- purrr::map(links$link, links_to_df)

  # Always return obj invisibly to ensure pipeability!
  #invisible(obj)
  return(list_covid_df)
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

