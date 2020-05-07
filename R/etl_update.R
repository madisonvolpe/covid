#' Update Database in Single Step: Extract, Transform, + Load
#' @description \emph{etl_update} performs the etl_extract, etl_transform, and etl_load pipe
#' in a single step. You must supply the db_con argument in order to load
#' data into a specfied Postgresql database. You can also supply month, day,
#' and year arguments to limit the extract, transform, and load of data for
#' a specified time period
#' @rdname etl_update.etl_covid
#' @method etl_update etl_covid
#' @import etl
#' @import dplyr
#' @inheritParams etl::etl_update
#' @param month
#' numeric vector specifying month(s)
#' @param day
#' numeric vector specifying day(s)
#' @param year
#' numeric vector specifying year(s)
#' @param db_con
#' a conenction to a PostgreSQL database
#' @export


etl_update.etl_covid <- function(obj, month, day, year, db_con, ...) {

  obj <- obj %>%
    etl_extract(month, day, year, ...) %>%
    etl_transform(month, day, year, ...) %>%
    etl_load(db_con, month, day, year, ...)

  invisible(obj)


}
