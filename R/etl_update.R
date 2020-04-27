#' Update Database in Single Step: Extract, Transform, + Load
#' @rdname etl_update.etl_covid
#' @method etl_update etl_covid
#' @import etl
#' @import dplyr
#' @inheritParams etl::etl_update
#' @export



etl_update.default <- function(obj, month, day, year, db_con) {

  obj <- obj %>%
    etl_extract(...) %>%
    etl_transform(...) %>%
    etl_load(...)

  invisible(obj)


}
