#' Construct a SENAITE API URL
#'
#' Builds a fully qualified SENAITE REST API URL for a given endpoint and
#' optional query parameters. The function automatically:
#'
#' - Removes trailing slashes from `base_url`
#' - Removes leading slashes from `endpoint`
#' - Appends the SENAITE API path (`@@API/senaite/v1/`)
#' - Adds URL query parameters if provided
#'
#' This function does not perform URL encoding of parameters. If parameters
#' may contain special characters, they should be URL-encoded prior to
#' calling this function.
#'
#' @param endpoint `character(1)`. The API endpoint path relative to
#'   `@@API/senaite/v1/`. For example: `"samples"` or `"analysisrequests/AR-0001"`.
#'
#' @param params `named list`. Optional query parameters to append to the URL.
#'   Names are used as parameter keys and values as parameter values.
#'   Default is an empty list (`list()`).
#'
#' @param base_url `character(1)`. The base URL of the SENAITE instance,
#'   e.g. `"https://lims.example.com"`. Defaults to the value stored in
#'   the system keyring under the key `"BASE_URL"`.
#'
#' @returns
#' A `character(1)` containing the complete API URL.
#'
#' @details
#' The constructed URL follows this structure:
#'
#' ```
#' {base_url}/@@API/senaite/v1/{endpoint}?param1=value1&param2=value2
#' ```
#'
#' If `params` is empty, no query string is appended.
#'
#' @examples
#' # Basic endpoint
#' construct_senaite_url("samples",
#'                       base_url = "https://lims.example.com")
#'
#' # Endpoint with query parameters
#' construct_senaite_url(
#'   endpoint = "analysisrequests",
#'   params = list(review_state = "verified", limit = 10),
#'   base_url = "https://lims.example.com"
#' )
#'
#' @seealso
#' \link[keyring:key_get]{key_get}
#'
#' @export
construct_senaite_url <- function(endpoint,
                                  params = list(),
                                  base_url = keyring::key_get('BASE_URL')) {
  base_url <- sub('/$', '', base_url)
  endpoint <- sub('^/', '', endpoint)
  api_url <- paste0(base_url, '/@@API/senaite/v1/', endpoint)
  if (length(params) > 0) {
    query_string <- paste(names(params),
                          params,
                          sep = '=',
                          collapse = '&')
    api_url <- paste0(api_url, '?', query_string)
  }
  api_url
}
