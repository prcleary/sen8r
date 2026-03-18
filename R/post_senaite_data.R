#' Create a New Object via the SENAITE API
#'
#' Sends a POST request to the SENAITE REST API to create a new object.
#' The request body is encoded as JSON and submitted to the `create`
#' endpoint.
#'
#' @param body `list` or `named list`. The request payload to be sent to
#'   the SENAITE API. This will be JSON-encoded using
#'   \code{httr::POST(..., encode = "json")}. The structure of `body`
#'   must conform to the SENAITE API specification for object creation.
#'
#' @param username `character(1)`. SENAITE username used for HTTP Basic
#'   Authentication.
#'
#' @param password `character(1)`. SENAITE password used for HTTP Basic
#'   Authentication.
#'
#' @param base_url `character(1)`. Base URL of the SENAITE instance
#'   (e.g., `"https://lims.example.com"`). Defaults to the value stored
#'   in the system keyring under `"BASE_URL"`.
#'
#' @returns
#' If the request succeeds (HTTP status 200), the full `httr` response
#' object is returned invisibly.
#'
#' If the request fails, the `httr` response object is returned visibly,
#' allowing inspection of status codes and error messages.
#'
#' @details
#' ## Endpoint
#'
#' The request is sent to:
#'
#' ```
#' {base_url}/@@API/senaite/v1/create
#' ```
#'
#' ## Authentication
#'
#' Authentication is performed using HTTP Basic Authentication via
#' \code{httr::authenticate()}.
#'
#' ## Error Handling
#'
#' The function checks for HTTP status code 200 to determine success.
#' If the upload fails, a warning message including the HTTP status code
#' is displayed using \code{cli::cli_alert_warning()}.
#'
#' @examples
#' \dontrun{
#' # Example: create a new client
#' new_client <- list(
#'   portal_type = "Client",
#'   title = "Acme Corporation",
#'   email = "info@acme.com"
#' )
#'
#' response <- post_senaite_data(
#'   body = new_client,
#'   username = "your_username",
#'   password = "your_password"
#' )
#' }
#'
#' @seealso
#' \code{\link{construct_senaite_url}},
#' \code{\link[httr]{POST}},
#' \code{\link[httr]{authenticate}}
#'
#' @export
post_senaite_data <- function(body,
                              username = keyring::key_get('senaite_username'),
                              password = keyring::key_get('senaite_password'),
                              base_url = keyring::key_get('BASE_URL')) {
  url <- construct_senaite_url('create', base_url = base_url)
  response <- httr::POST(url,
                         httr::authenticate(username, password),
                         body = body,
                         encode = 'json')
  if (httr::status_code(response) == 200) {
    cli::cli_alert_info('Upload successful')
    invisible(response)
  } else {
    cli::cli_alert_warning('Upload failed.\nHTTP Status: {httr::status_code(response)}')
    response
  }
}
