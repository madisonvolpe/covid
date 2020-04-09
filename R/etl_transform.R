#' Transform COVID-19 Data to be Uploaded into SQL Database
#' @description \code{etl_transform} takes files saved in the \emph{raw} folder
#' from \code{etl_extract} and transforms them into usable datasets to be uploaded
#' into a SQL database. After transformation, files are saved in the \emph{load}
#' folder within the specified directory or the temp directory if the user did
#' not specify a directory. Under the hood, \code{etl_transform} cleans datasets
#' by standardizing colummn names and removing unnecessary punctuation. Most
#' importantly the the function only selects relevant columns: province/state,
#' country/region, last_update, confirmed, deaths, and recovered. Arguments,
#' such as month, day, and year let users transform data for a specified
#' time period. When no arguments are specified then all data saved
#' in the \emph{raw} folder will be transformed and saved in the
#' \emph{load} folder.
#' @rdname etl_transform.etl_covid
#' @method etl_transform etl_covid
#' @import etl
#' @import dplyr
#' @importFrom purrr map
#' @inheritParams etl::etl_transform
#' @param month
#' fill in
#' @param year
#' fill in
#' @export

etl_transform.etl_covid <- function(obj, month, day, year, ...){

  # Reading in

  # Read in raw files from raw_dir, raw_dir is either specified directory in initial etl call
  # or from temp directory in initial etl call
  files <- list.files(attr(obj, "raw_dir"), "\\.csv", full.names = T)
  covid_dfs <- purrr::map(files, readr::read_csv) %>% set_names(map_chr(files, ~str_extract(.x, "\\d{4}\\-\\d{2}\\-\\d{2}")))

  # To select dfs to transform
  nms_covid_dfs <- names(covid_dfs)

  if(missing(month) & missing(day) & missing(year)){

    covid_dfs <- covid_dfs

  } else if (missing(day) & missing(year)) {

    covid_dfs <- covid_dfs[lubridate::month(nms_covid_dfs) %in% month]

  } else if(missing(month) & missing(day)) {

    covid_dfs <- covid_dfs[lubridate::year(nms_covid_dfs) %in% year]

  } else if(missing(month) & missing(year)){

    covid_dfs <- covid_dfs[lubridate::day(nms_covid_dfs) %in% day]

  } else if(missing(day)){

    covid_dfs <- covid_dfs[lubridate::month(nms_covid_dfs) %in% month &
                             lubridate::year(nms_covid_dfs) %in% year]

  } else if(missing(year)){

    covid_dfs <- covid_dfs[lubridate::month(nms_covid_dfs) %in% month &
                           lubridate::day(nms_covid_dfs) %in% day]

  } else {

    covid_dfs <- covid_dfs[lubridate::day(nms_covid_dfs) %in% day &
                           lubridate::year(nms_covid_dfs) %in% year]
  }


  # Cleaning

  # Make all column names in dfs match
  covid_dfs <- purrr::map(covid_dfs,standardize_names)

  # Cleaning dataframes for easy loading to SQL db
  covid_dfs <- purrr::map(covid_dfs, ~purrr::map_df(.x, ~stringr::str_replace_all(., "'", "")))
  covid_dfs <- purrr::map(covid_dfs, cleaning_covid_datasets)

  # Keeping only relevant columns (province_state, country_region, last_update, confirmed, recovered, deaths)
  covid_dfs <- purrr::map(covid_dfs, ~dplyr::select(.x, province_state, country_region, last_update,
                                                    confirmed, deaths, recovered))

  # Saving transformed files to load folder in directory or temp directory
  covid_dfs %>% names(.) %>% purrr::map(~readr::write_csv(covid_dfs[[.]], paste0(attr(obj, "load_dir"),"/",.,".csv")))

  Sys.sleep(5)
  print(covid_dfs)

  invisible(obj)

}

# Internal function 1

#' All columns in every dataframe in  the list should have the same column names
#' @param df takes a df and makes column names standardized
#' @noRd

standardize_names <- function(df){

  # Take out column names and apply operations to them
  names_df <- names(df)

  names_df <- tolower(names_df)
  names_df <- stringr::str_replace(names_df,"\\/", "_")
  names_df <- stringr::str_replace(names_df,"_$", "")
  names_df <- trimws(names_df)
  names_df <- stringr::str_replace(names_df, "\\s+", "_")

  names_df[grepl("^lat", names_df)] <- "lat"
  names_df[grepl("^long", names_df)] <- "long"

  names(df) <- names_df

  return(df)
}

# Internal function 2

#' Cleaning all covid dfs to make them acceptable for loading to a SQL database
#' @param df takes a df and cleans it
#' @noRd

cleaning_covid_datasets <- function(df){

  df <- df %>%
    dplyr::mutate_at(vars(starts_with("last")), list(~stringr::str_replace_all(., "T", " "))) %>%
    dplyr::mutate_at(vars(starts_with("last")),
                     list(~as.character(lubridate::parse_date_time(.,orders = c('mdy_hm', 'ymd_hms'))))) %>%
    dplyr::mutate_at(vars(matches("^conf|^reco|^deat|^acti")), as.numeric)

  return(df)
}

