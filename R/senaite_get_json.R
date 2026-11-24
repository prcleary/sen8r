# Fetch one JSON response from a relative SENAITE API path.
# Internal: no @export.
senaite_get_json <- function(path, auth, query = list()) {
  url <- construct_senaite_url(
    endpoint = path,
    params = query,
    base_url = auth$base_url
  )

  response <- httr::GET(
    url,
    httr::authenticate(auth$username, auth$password, type = "basic"),
    httr::accept_json(),
    httr::config(followlocation = 0L)
  )

  status <- httr::status_code(response)
  if (status >= 300L && status < 400L) {
    stop(
      "SENAITE returned a redirect (HTTP ", status, "). ",
      "Check that the profile contains the final instance URL.",
      call. = FALSE
    )
  }

  httr::stop_for_status(response, task = "retrieve SENAITE data")

  jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
}
