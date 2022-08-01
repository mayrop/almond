# F U N C T I O N S

# https://psyarxiv.com/u8ekc/
#' @export
group_nest_dt <- function(dt, ..., .key = "data") {
  stopifnot(data.table::is.data.table(dt))
  by <- substitute(list(...))
  dt <- dt[, list(list(data.table::.SD)), by = eval(by)]
  data.table::setnames(dt, old = "V1", new = .key)
  dt
}

#' @export
unnest_dt <- function(dt, col, id) {
  stopifnot(data.table::is.data.table(dt))
  by <- substitute(id)
  col <- substitute(unlist(col, recursive = FALSE))
  dt[, eval(col), by = eval(by)]
}
