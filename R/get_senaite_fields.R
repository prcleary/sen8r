#' Retrieve Selected Fields from a SENAITE Endpoint
#'
#' Queries a SENAITE API endpoint and returns only the specified fields
#' for each object. Optionally flattens the result into a tabular format.
#'
#' This function is a convenience wrapper around \code{get_senaite_data()}
#' that:
#'
#' - Ensures core metadata fields are always included
#' - Subsets each returned object to selected fields
#' - Optionally flattens nested output into a `data.table`
#'
#' @param endpoint `character(1)`. Name of the SENAITE API endpoint to query
#'   (e.g., `"analysisrequest"`, `"client"`, `"worksheet"`).
#'
#' @param username `character(1)`. SENAITE username for HTTP authentication.
#'   Defaults to the value stored in the system keyring under
#'   `"senaite_username"`.
#'
#' @param password `character(1)`. SENAITE password for HTTP authentication.
#'   Defaults to the value stored in the system keyring under
#'   `"senaite_password"`.
#'
#' @param fields `character`. Additional field names to retrieve from each
#'   SENAITE object. Core fields (`"title"`, `"uid"`, `"parent_path"`,
#'   `"portal_type"`) are always included.
#'
#' @param params `list`. Named list of query parameters passed to
#'   \code{get_senaite_data()}. Defaults to
#'   `list(complete = TRUE, children = TRUE)`.
#'
#' @param verbose `logical(1)`. If `TRUE` (default), prints progress messages.
#'
#' @param flatten `logical(1)`. If `TRUE` (default), the result is flattened
#'   into a `data.table` using \code{flatten_senaite_output()}. If `FALSE`,
#'   a list of named lists is returned.
#'
#' @param base_url `character(1)`. Base URL of the SENAITE instance.
#'   Defaults to the value stored in the system keyring under `"BASE_URL"`.
#'
#' @returns
#' If `flatten = TRUE`, returns a `data.table` where each row represents
#' a SENAITE object and each column corresponds to a field.
#'
#' If `flatten = FALSE`, returns a `list` of named lists containing only
#' the selected fields.
#'
#' Returns `NULL` if no data is found.
#'
#' @details
#' ## Default Fields
#'
#' The following metadata fields are always included:
#'
#' - `"title"`
#' - `"uid"`
#' - `"parent_path"`
#' - `"portal_type"`
#'
#' These are prepended to any user-supplied `fields` and deduplicated.
#'
#' ## Flattening Behavior
#'
#' When `flatten = TRUE`, nested fields are flattened into a wide
#' structure. Repeated values are collapsed into a single string
#' (see \code{\link{flatten_senaite_output}}).
#'
#' @examples
#' \dontrun{
#' # Retrieve selected fields from analysis requests
#' ars <- get_senaite_fields(
#'   endpoint = "analysisrequest",
#'   fields = c("review_state", "created"),
#'   flatten = TRUE
#' )
#'
#' # Retrieve raw (non-flattened) output
#' clients <- get_senaite_fields(
#'   endpoint = "client",
#'   fields = c("email", "phone"),
#'   flatten = FALSE
#' )
#' }
#'
#' @seealso
#' \code{\link{get_senaite_data}},
#' \code{\link{flatten_senaite_output}},
#' \code{\link[keyring]{key_get}}
#'
#' @export
get_senaite_fields <- function(endpoint,
                               username = keyring::key_get('senaite_username'),
                               password = keyring::key_get('senaite_password'),
                               fields = '',
                               params = list(complete = TRUE, children = TRUE),
                               verbose = TRUE,
                               flatten = TRUE,
                               base_url = keyring::key_get('BASE_URL')) {
  fields <- unique(c(c(
    'title', 'uid', 'parent_path', 'portal_type'
  ), fields))
  query <- get_senaite_data(
    endpoint,
    params = params,
    username = username,
    password = password,
    verbose = verbose
  )
  values <- lapply(query, function(x)
    x[fields])
  if (flatten) {
    flatten_senaite_output(values)
  } else {
    values
  }
}
