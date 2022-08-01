# Function to reset all connections
#' @export
reset_mysql_connections <- function() {
  lapply(DBI::dbListConnections(RMySQL::MySQL()), DBI::dbDisconnect)
}

# Function to reset all connections
#' @export
list_mysql_connections <- function() {
  DBI::dbListConnections(RMySQL::MySQL())
}

# Abstract Model
# https://stackoverflow.com/questions/35581343/
#' @importFrom R6 R6Class
#' @importFrom magrittr "%>%"
#' @export
db_model <- R6::R6Class(
  "db_model",
  private = list(
    conn = NULL,
    tables = list()
  ),
  public = list(
    initialize = function(connection, ...) {
      private$conn <- DBI::dbConnect(
        RMySQL::MySQL(),
        user = connection$user,
        password = connection$password,
        dbname = connection$dbname,
        host = connection$host
      )

      private$tables <- purrr::map(private$all_tables,
                                   ~stringr::str_split(., ",") %>%
                                     purrr::map_chr(tail, -1)) %>%
        purrr::map(., ~dplyr::tbl(private$conn, .)) %>%
        purrr::set_names(
          purrr::map_chr(private$all_tables,
                         ~stringr::str_split(., ",") %>%
                           purrr::map_chr(head, 1))
        )
    },
    connection = function() {
      private$conn # returning connection
    },
    query = function(q) {
      DBI::dbGetQuery(private$conn, q)
    },
    describe = function(table) {
      DBI::dbGetQuery(private$conn, paste0("DESCRIBE ", table))
    },
    table = function(table,
                     select_columns = NULL,
                     rename_columns = c(),
                     rename_all = FALSE,
                     prefix = NULL) {
      get_table_prefix <- function(table_name) {
        table_name <- gsub("ies$", "y", table_name)
        table_name <- gsub("s$", "", table_name)

        paste0(table_name, "_")
      }

      # by default, we want the prefix to be the singular name of the table
      # this is a quick and dirty way to accomplish that
      prefix <- prefix %||% get_table_prefix(table)
      pattern <- paste0("^", prefix)
      # this will return a lazy load query

      private$tables[[table]] %>%
        dplyr::select(
          select_columns %||% everything()
        ) %>%
        # this one will rename all the variables manually requested
        dplyr::rename_at(vars(rename_columns), ~paste0(prefix, "_", .)) %>%
        # this one will rename all the variables if requested (adding prefix)
        # this fails with names (if it's still lazy query)
        ## Error in tbl_if_vars(.tbl,
          ## .predicate, caller_env(), .include_group_vars = TRUE)
        ## length(.p) == length(tibble_vars) is not TRUE
        # so replacing names(.) with colnames(.)
        dplyr::rename_if(rename_all & !grepl(pattern, colnames(.)),
                         ~paste0(prefix, .))
    },
    map_table = function(key, table) {
      private$tables[[key]] <- dplyr::tbl(private$conn, table)
    },
    get_tables = function() {
      names(private$tables)
    },
    list_tables = function(pattern = "") {
      DBI::dbListTables(private$conn) %>%
        as.data.frame() %>%
        filter(grepl(pattern, .))
    }
  )
)
