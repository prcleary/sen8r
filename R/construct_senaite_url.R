#' Construct a SENAITE API URL
#'
#' Build a URL under the supplied instance's `@@API/senaite/v1/` path.
#' Query parameter names and values are URL-encoded. Pass ordinary R
#' strings, not strings that have already been URL-encoded.
#'
#' @param endpoint A relative API endpoint name, such as `"client"`, or a
#'   relative record path, such as `"analysisrequest/AR-0001"`. A single
#'   leading slash is optional. Do not include a URL or query string.
#' @param params A named list of query parameters. Each value must be one
#'   non-missing character, numeric, or logical value. An empty list adds
#'   no query string.
#' @param base_url The SENAITE instance URL, including its port and instance
#'   path if applicable. Required. When called from [get_senaite_data()],
#'   this comes from the selected connection profile.
#'
#' @returns A single character string containing the complete API URL.
#'
#' @examples
#' construct_senaite_url(
#'   "client",
#'   base_url = "https://lims.example.com/senaite"
#' )
#'
#' construct_senaite_url(
#'   "analysisrequest",
#'   params = list(review_state = "verified", limit = 50, b_start = 50),
#'   base_url = "https://lims.example.com/senaite"
#' )
#'
#' @seealso [get_senaite_data()], [senaite_setup()]
#' @export
construct_senaite_url <- function(endpoint, params = list(), base_url) {
  if (missing(base_url)) {
    stop("Supply `base_url` explicitly.", call. = FALSE)
  }

  # Reuse the URL validation in credentials.R. This helper only builds a
  # string; the profile's HTTP opt-in is checked by senaite_credentials()
  # before the getter uses that URL for an authenticated request.
  base_url <- senaite_normalize_base_url(base_url, allow_http = TRUE)

  if (!is.character(endpoint) || length(endpoint) != 1L || is.na(endpoint)) {
    stop("`endpoint` must be a single relative API path.", call. = FALSE)
  }

  # Accept "/client" as well as "client", but not "//host", an absolute
  # URL, a query string, or a path that can escape the API directory.
  endpoint <- sub("^/", "", endpoint)
  if (!grepl("^[A-Za-z0-9._~-]+(/[A-Za-z0-9._~-]+)*\\z",
             endpoint, perl = TRUE) ||
      any(strsplit(endpoint, "/", fixed = TRUE)[[1L]] %in% c(".", ".."))) {
    stop(
      "`endpoint` must be a relative API path such as 'client' or ",
      "'analysisrequest/AR-0001'. Supply query parameters in `params`.",
      call. = FALSE
    )
  }

  api_url <- paste0(base_url, "/@@API/senaite/v1/", endpoint)

  if (!is.list(params) || is.data.frame(params)) {
    stop("`params` must be a named list, or list().", call. = FALSE)
  }
  if (!length(params)) {
    return(api_url)
  }

  keys <- names(params)
  if (is.null(keys) || anyNA(keys) || any(!nzchar(keys)) ||
      anyDuplicated(keys)) {
    stop("Each query parameter must have a unique, non-empty name.",
         call. = FALSE)
  }

  # Encode components individually so that '&' or '=' inside a value
  # stays part of that value rather than changing the query structure.
  encode <- function(x) {
    utils::URLencode(enc2utf8(x), reserved = TRUE, repeated = TRUE)
  }

  values <- vapply(seq_along(params), function(i) {
    value <- params[[i]]

    if (length(value) != 1L || !is.null(dim(value)) ||
        !(is.character(value) || is.numeric(value) || is.logical(value)) ||
        is.na(value)) {
      stop(
        "Query parameter '", keys[[i]],
        "' must be a single, non-missing character, numeric, or logical value.",
        call. = FALSE
      )
    }

    encode(as.character(value))
  }, character(1))

  encoded_keys <- vapply(keys, encode, character(1), USE.NAMES = FALSE)
  query <- paste0(encoded_keys, "=", values, collapse = "&")

  paste0(api_url, "?", query)
}
