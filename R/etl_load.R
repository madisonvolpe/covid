#' Load Covid-19 Data into PostgreSQL Database
#' @rdname etl_load.etl_covid
#' @method etl_load etl_covid
#' @import etl
#' @import dplyr
#' @inheritParams etl::etl_load
#'
#'


etl_load.etl_covid <- function(obj, ...){

  # Reading In

  files <- list.files(attr(obj, "load_dir"), "\\.csv", full.names = T)
  transformed_dfs <- purrr::map(files, readr::read_csv) %>% set_names(map_chr(files, ~str_extract(.x, "\\d{4}\\-\\d{2}\\-\\d{2}")))

}
