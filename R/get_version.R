#' Get SENAITE version metadata
#'
#' @inheritParams get_senaite_data
#' @returns The parsed version response from the selected SENAITE instance.
#' @export
get_version <- function(profile = "default") {
  auth <- senaite_credentials(profile)
  senaite_get_json(path = "version", auth = auth)
}
