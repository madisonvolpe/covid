#' etl_transform.etl_covid
#' @rdname etl_transform.etl_covid
#' @method etl_extract etl_covid
#' @import etl
#' @import dplyr
#' @importFrom ....
#' @inheritParams etl::etl_transform
#' @param month
#' @param year
#' @details
#' fill in
#' @export

etl_transform.etl_covid <- function(obj, ...){

  list.files(attr(obj, "raw_dir"), "\\.csv", full.names = T)

  # csvs <- csvc[grepl(".csv$", csvs)]
  # csvs <- paste0("./", csvs)
  #
  # covid_dfs <- purrr::map(csvs, readr::read_csv)
  # covid_dfs <- purrr::map(standardize_names)

}

# Internal function
#' All dataframes in list should be standardized
#' param takes a df (from list) and applies cleaning functions
#' @noRd

standardize_names <- function(df){

  # take out column names and apply operations to them
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
