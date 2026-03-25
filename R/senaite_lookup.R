#' Create a Lookup Function for a SENAITE Endpoint
#'
#' Generates a reusable lookup function for translating values between
#' fields of a SENAITE endpoint.
#'
#' The function downloads and flattens all objects from the specified
#' endpoint, then returns a closure that can be used to map values from
#' one field to another.
#'
#' @param endpoint `character(1)`. Name of the SENAITE API endpoint
#'   (e.g., `"analysisservice"`, `"client"`, `"sampletype"`).
#'
#' @param username `character(1)`. SENAITE username for HTTP authentication.
#'   Defaults to the value stored in the system keyring under
#'   `"senaite_username"`.
#'
#' @param password `character(1)`. SENAITE password for HTTP authentication.
#'   Defaults to the value stored in the system keyring under
#'   `"senaite_password"`.
#'
#' @param base_url `character(1)`. Base URL of the SENAITE instance.
#'   Defaults to the value stored in the system keyring under `"BASE_URL"`.
#'
#' @returns
#' A function with signature:
#'
#' ```
#' function(values, from, to)
#' ```
#'
#' where:
#'
#' - `values`: vector of values to match
#' - `from`: column name to match on
#' - `to`: column name to return
#'
#' The returned function performs a keyed lookup using `data.table`
#' and returns the corresponding values from the `to` column.
#'
#' @details
#' ## Workflow
#'
#' 1. Calls \code{get_senaite_data()} with `children = TRUE` and
#'    `complete = TRUE`.
#' 2. Flattens the result using \code{flatten_senaite_output()}.
#' 3. Prints available field names.
#' 4. Returns a lookup function operating on the cached table.
#'
#' ## Performance
#'
#' The endpoint data is downloaded once when `senaite_lookup()` is called.
#' Subsequent lookups are performed locally using `data.table` keyed joins.
#'
#' ## Requirements
#'
#' The `from` column should uniquely identify rows for reliable results.
#'
#' @examples
#' \dontrun{
#' # Create a lookup for analysis services
#' service_lookup <- senaite_lookup("analysisservice")
#'
#' # Map service UID to service title
#' service_lookup(
#'   values = c("uid1", "uid2"),
#'   from   = "uid",
#'   to     = "title"
#' )
#' }
#'
#' @seealso
#' \code{\link{get_senaite_data}},
#' \code{\link{flatten_senaite_output}},
#' \code{\link[data.table]{setkeyv}}
#'
#' @export
senaite_lookup <- function(endpoint,
                           username = keyring::key_get('senaite_username'),
                           password = keyring::key_get('senaite_password'),
                           base_url = keyring::key_get('BASE_URL')) {
  lookup_table <- get_senaite_data(
    endpoint,
    params = list(children = TRUE, complete = TRUE),
    username = username,
    password = password,
    base_url = base_url
  ) |>
    flatten_senaite_output()

  cli::cli_alert_info(paste('Lookup fields: ', paste0(names(lookup_table), collapse = ', ')))

  function(values, from, to) {
    # Use match to find positions and preserve order
    matches <- match(values, lookup_table[[from]])
    lookup_table[[to]][matches]
  }
}
