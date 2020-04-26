#' Update Database in Single Step: Extract, Transform, + Load
#' @rdname etl_update.etl_covid
#' @method etl_update etl_covid
#' @import etl
#' @import dplyr
#' @inheritParams etl::etl_update
#' @export

etl_update <- function(obj, ...) UseMethod("etl_update")

#' @rdname etl_create
#' @method etl_update default
#' @export

etl_update.default <- function(obj, ...) {
  obj <- obj %>%
    etl_extract(...) %>%
    etl_transform(...) %>%
    etl_load(...)
  invisible(obj)
}
