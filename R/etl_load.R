#' Load COVID-19 Data into PostgreSQL Database
#' @description \code{etl_load} takes files saved in the \emph{load} folder to
#' be prepared for loading into a PostgreSQL database. Users must use the db_con
#' argument to specify a database connection. Users can supply month, day, and year
#' arguments to load a specific subset of data into their database.
#' When no arguments are specified all data in the load folder will be uploaded.
#' It is important to be aware that \code{etl_load} is written so that duplicate entries
#' of data are updated like an upsert.
#' @rdname etl_load.etl_covid
#' @method etl_load etl_covid
#' @import etl
#' @import dplyr
#' @importFrom lubridate ymd month day year
#' @importFrom readr read_csv
#' @importFrom purrr set_names map map_chr map_df
#' @importFrom DBI dbSendQuery
#' @importFrom plyr rbind.fill
#' @inheritParams etl::etl_load
#' @param db_con
#' A connection to a PostgreSQL database
#' @param month
#' numeric vector specifying month(s)
#' @param day
#' numeric vector specifying day(s)
#' @param year
#' numeric vector specifying year(s)
#' @examples
#'
#' covid_dat <- etl("covid")
#'
#' # Extracting, Transforming, and Loading all available data
#'
#' covid_dat %>% etl_extract() %>% etl_transform() %>% etl_load(db_con = dbconnection)
#'
#' # Extracting all data, Tranforming data for all of April 2020 and Loading data for the last 5 days of April
#'
#' covid_dat %>% etl_extract() %>% etl_transform(month = 4, year = 2020) %>% etl_load(db_con = dbconnection, month = 4, days = c(25:30), year = 2020)
#'
#' @export


etl_load.etl_covid <- function(obj, db_con, month, day, year, ...){

  # Reading In
  files <- list.files(attr(obj, "load_dir"), "\\.csv", full.names = T)

  transformed_dfs <- list()

  for(i in 1:length(files)){

    if(lubridate::ymd(str_extract(files[i], "\\d{4}\\-\\d{2}\\-\\d{2}")) < lubridate::ymd("2020-03-22")){

      transformed_dfs[[i]] <- readr::read_csv(files[i], col_types = cols(col_character(), col_character(),
                                                                         col_character(), col_integer(),
                                                                         col_integer(), col_integer()))

    } else {

      transformed_dfs[[i]] <- readr::read_csv(files[i], col_types = cols(col_character(), col_character(),
                                                                         col_character(), col_character(),
                                                                         col_integer(), col_integer(),
                                                                         col_integer()))

    }

  }



  transformed_dfs <- transformed_dfs %>% purrr::set_names(map_chr(files, ~str_extract(.x, "\\d{4}\\-\\d{2}\\-\\d{2}")))

  # Selecting data to load based on date parameters
  nms_transformed_dfs <- names(transformed_dfs)

  if(missing(month) & missing(day) & missing(year)){

    transformed_dfs <- transformed_dfs

  } else if (!missing(month) & !missing(day) & !missing(year)){

    transformed_dfs <- transformed_dfs[lubridate::month(nms_transformed_dfs) %in% month &
                                       lubridate::day(nms_transformed_dfs) %in% day &
                                       lubridate::year(nms_transformed_dfs) %in% year]

  } else if (missing(day) & missing(year)) {

    transformed_dfs <- transformed_dfs[lubridate::month(nms_transformed_dfs) %in% month]

  } else if(missing(month) & missing(day)) {

    transformed_dfs <- transformed_dfs[lubridate::year(nms_transformed_dfs) %in% year]

  } else if(missing(month) & missing(year)){

    transformed_dfs <- transformed_dfs[lubridate::day(nms_transformed_dfs) %in% day]

  } else if(missing(day)){

    transformed_dfs <- transformed_dfs[lubridate::month(nms_transformed_dfs) %in% month &
                             lubridate::year(nms_transformed_dfs) %in% year]

  } else if(missing(year)){

    transformed_dfs <- transformed_dfs[lubridate::month(nms_transformed_dfs) %in% month &
                             lubridate::day(nms_transformed_dfs) %in% day]

  } else {

    transformed_dfs <- transformed_dfs[lubridate::day(nms_transformed_dfs) %in% day &
                             lubridate::year(nms_transformed_dfs) %in% year]
  }

  # Take list of dfs and make one big dataframe
  transformed_all <- plyr::rbind.fill(transformed_dfs)
  transformed_all <- dplyr::select(transformed_all, admin, province_state, country_region, last_update, confirmed, deaths, recovered)

  # Pre - Load to have upsert work

    # run distinct command
    transformed_all <- dplyr::distinct(transformed_all, admin, province_state, country_region, last_update, confirmed, deaths, recovered)

    # remove observations where confirmed, deaths, recovered are all NA
    transformed_all  <- transformed_all %>%
      dplyr::mutate(all_na = ifelse(is.na(confirmed) & is.na(recovered) & is.na(deaths), T, F)) %>%
      dplyr::filter(all_na == F ) %>%
      dplyr::select(1:7)

    # if last_update is the same but #s differ update to hire numbers
    transformed_all <- transformed_all %>%
      dplyr::rowwise() %>%
      dplyr::mutate(total_activity = sum(confirmed,
                                  recovered,
                                  deaths,
                                  na.rm = T)) %>%
      dplyr::group_by(admin, province_state, country_region, last_update) %>%
      dplyr::top_n(1, total_activity) %>%
      dplyr::select(1:7)

  # Apply functions to transform df into SQL statement

   transformed_all <- purrr::map_df(transformed_all, na_blank_tonull)
   transformed_all <- purrr::map_df(transformed_all, quote_to_sql)
   transformed_all <- constraint_blank(transformed_all, "admin|province_state")

   transformed_all_sql <- row_to_sql(transformed_all)

   transformed_all_sql_query <- paste0("INSERT INTO covid_stats VALUES ", transformed_all_sql,
                                " ON CONFLICT (admin, province_state, country_region, last_update) DO UPDATE SET confirmed = EXCLUDED.confirmed, deaths = EXCLUDED.deaths, recovered = EXCLUDED.recovered;")

   # transformed_all_sql_query <- paste0("INSERT INTO covid_stats VALUES ", transformed_all_sql,
   #                                     " ON CONFLICT (admin, province_state, country_region, last_update) DO NOTHING;")

   DBI::dbSendQuery(db_con, transformed_all_sql_query)

   invisible(obj)

}

# Internal function 1

#' Convert NA and blank values in each column to NULL for SQL purposes.
#' This function is used after all the lists of dfs are combined into
#' one list. The map_df function is used to apply function to each
#' column in df.
#' @param col takes column in df and cleans it
#' @noRd

na_blank_tonull <- function(col){

  for(i in 1:length(col)){
    if(is.na(col[i])){
      col[i] <- "NULL"
    } else if(is.element(col[i], "")){
      col[i] <- "NULL"
    } else {
      col[i] <- col[i]
    }
  }
  return(col)
}

# Internal function 2

#' When importing data into a table with a unique constraint - the unique constraint
#' is not abided by if there are NULL. In this case, we can make NULL in the
#' first column to NA (this is like a hack)
#' @param df takes the entire df
#' @param pat take the pattern which will be used to perform this operation on specified column
#' @noRd

constraint_blank <- function(df,pat){

  df <- df %>% mutate_at(vars(matches(pat)), list(~ifelse(. == 'NULL',"' '", .)))

  return(df)
}

# Internal Function 3

#' Making data appear the way it should be -- if a SQL query was actually being written.
#' Essentially transforming data to match SQL statement guidelines.
#' Applying this function to each column in df of unlisted transformed dfs.
#' @param col for each column if NULL not quoted -- else quoted
#' @noRd

quote_to_sql <- function(col){

  for(i in 1:length(col)){
    if(is.element(col[i], "NULL")){
      col[i] <- col[i]
    } else {
      col[i] <- paste("'", col[i], "'", sep = "")
    }
  }
  return(col)
}

# Internal Function 4
#' Final transformation before SQL, collapse each row into its own vector.
#' Writing it this way so data can be written as it would be in a SQL
#' Insert Query with an Upsert.
#' @param df this is applied to the transformed dataframe
#' @noRd

row_to_sql <- function(df){

  trans1 <- apply(df, 1, function(x) paste0(x, collapse = ","))
  trans2 <- paste0("(", trans1, ")", sep = "")
  trans3 <- paste0(trans2, collapse = ",", sep = "")

  return(trans3)

}




