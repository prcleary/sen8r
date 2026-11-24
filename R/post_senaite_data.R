#' Create a SENAITE object from a prepared payload
#'
#' Submits a named list to the SENAITE `create` endpoint using the
#' connection settings for the selected profile.
#'
#' @param body A non-empty named list containing the create payload.
#'   Field names and nested values are passed to the API without flattening.
#' @param profile Name of the connection profile saved by [senaite_setup()].
#' @param encoding Request body encoding. Defaults to `"json"`; use `"form"`
#'   for a URL-encoded form.
#'
#' @returns The parsed, unflattened JSON response, or `NULL` for an empty
#'   successful response. HTTP errors raise an error.
#'
#' @seealso [senaite_setup()], [get_senaite_data()]
#' @export
post_senaite_data <- function(body, profile = "default", encoding = c("json", "form")) {
  keys <- names(body)

  if (!is.list(body) || is.data.frame(body) || !length(body) ||
      is.null(keys) || anyNA(keys) || any(!nzchar(keys)) ||
      anyDuplicated(keys)) {
    stop(
      "`body` must be a non-empty list with unique, non-empty names.",
      call. = FALSE
    )
  }

  auth <- senaite_credentials(profile)
  senaite_post_json(path = "create", auth = auth, body = body, encoding = match.arg(encoding))
}

# Internal helper: send one JSON POST request using resolved credentials.
senaite_post_json <- function(path, auth, body, encoding = c("json", "form")) {
  url <- construct_senaite_url(
    endpoint = path,
    params = list(),
    base_url = auth$base_url
  )

  encoding <- match.arg(encoding)

  if (encoding == "form") {
    response <- httr::POST(
      url,
      httr::authenticate(auth$username, auth$password, type = "basic"),
      httr::accept_json(),
      httr::config(followlocation = 0L),
      body = body,
      encode = "form"
    )
  } else {
    payload <- jsonlite::toJSON(
      body, auto_unbox = TRUE, null = "null", na = "null", digits = NA
    )
    response <- httr::POST(
      url,
      httr::authenticate(auth$username, auth$password, type = "basic"),
      httr::accept_json(),
      httr::content_type_json(),
      httr::config(followlocation = 0L),
      body = charToRaw(enc2utf8(as.character(payload)))
    )
  }

  status <- httr::status_code(response)
  if (status >= 300L && status < 400L) {
    stop(
      "SENAITE returned a redirect (HTTP ", status, "). ",
      "Check that the profile contains the final instance URL.",
      call. = FALSE
    )
  }

  text <- httr::content(response, as = "text", encoding = "UTF-8")

  if (httr::http_error(response)) {
    detail <- trimws(text)
    if (!nzchar(detail)) {
      detail <- "(SENAITE returned no error body)"
    }

    stop(
      "SENAITE create failed (HTTP ", status, ").\n",
      substr(detail, 1L, 2000L),
      call. = FALSE
    )
  }
  if (!nzchar(trimws(text))) {
    return(NULL)
  }

  if (!jsonlite::validate(text)) {
    stop(
      "SENAITE returned HTTP ", status, " but the response was not JSON. ",
      "Check whether the object was created before retrying.",
      call. = FALSE
    )
  }

  jsonlite::fromJSON(text, simplifyVector = FALSE)
}
