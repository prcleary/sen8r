#' Create a cached lookup for a SENAITE endpoint
#'
#' Download and flatten an endpoint's records once, then return a function
#' that translates values between fields without making further API requests.
#'
#' @param endpoint A SENAITE collection endpoint, such as `"analysisservice"`,
#'   `"client"`, or `"samplepoint"`.
#' @param profile Connection profile passed to [get_senaite_data()].
#' @param verbose If `TRUE`, report download progress and the available
#'   lookup fields.
#'
#' @returns A function with signature `function(values, from, to)`.
#'   `values` is a vector of lookup values; `from` and `to` are exact,
#'   case-sensitive field names in the flattened table. The result is the
#'   `to` column indexed in the same order as `values`. For an atomic
#'   `to` column, an unmatched value returns a typed `NA`.
#'
#' @details
#' Use a unique field such as `"uid"` for `from` when possible. If several
#' records have the same `from` value, the first matching record is used.
#' Missing input values are treated as unmatched.
#'
#' The lookup is a snapshot of one endpoint on one profile. Create it again
#' to pick up changes on the server. An empty collection raises an error
#' because there are no records from which to discover the lookup fields.
#'
#' @examples
#' \dontrun{
#' service_lookup <- senaite_lookup("analysisservice", profile = "default")
#' service_lookup(c("uid1", "uid2"), from = "uid", to = "title")
#' }
#'
#' @seealso [get_senaite_data()], [flatten_senaite_output()]
#' @export
senaite_lookup <- function(endpoint, profile = "default", verbose = TRUE) {
  records <- get_senaite_data(
    endpoint = endpoint,
    params = list(children = "yes", complete = "yes"),
    profile = profile,
    verbose = verbose
  )

  if (is.null(records)) {
    stop(
      "Endpoint '", endpoint, "' did not return a collection.",
      call. = FALSE
    )
  }
  if (!length(records)) {
    stop(
      "No records available for lookup in endpoint '", endpoint, "'.",
      call. = FALSE
    )
  }

  lookup_table <- flatten_senaite_output(records)
  rm(records)  # Don't retain both raw and flattened records in the closure.
  fields <- names(lookup_table)

  if (!length(fields)) {
    stop("No lookup fields found in endpoint '", endpoint, "'.",
         call. = FALSE)
  }

  if (verbose) {
    cli::cli_alert_info("Lookup fields: {paste(fields, collapse = ', ')}")
  }

  check_field <- function(field, arg) {
    if (!is.character(field) || length(field) != 1L || is.na(field) ||
        !field %in% fields) {
      stop(
        "`", arg, "` must be one of: ", paste(fields, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }

  function(values, from, to) {
    check_field(from, "from")
    check_field(to, "to")

    positions <- match(values, lookup_table[[from]], incomparables = NA)
    lookup_table[[to]][positions]
  }
}
