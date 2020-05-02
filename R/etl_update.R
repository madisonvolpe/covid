#' Update Database in Single Step: Extract, Transform, + Load
#' @rdname etl_update.etl_covid
#' @method etl_update etl_covid
#' @import etl
#' @import dplyr
#' @inheritParams etl::etl_update
#' @export



etl_update.etl_covid <- function(obj, month, day, year, db_con, ...) {

  obj <- obj %>%
    etl_extract(month, day, year, ...) %>%
    etl_transform(month, day, year, ...) %>%
    etl_load(db_con, month, day, year, ...)

  invisible(obj)


}
