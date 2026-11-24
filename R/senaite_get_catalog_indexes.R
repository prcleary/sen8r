#' Get the search indexes for a SENAITE catalog
#'
#' Request metadata for one catalog and return its index names. Supply the
#' catalog ID (for example, `"senaite_catalog_client"`), not the collection
#' endpoint name (`"client"`).
#'
#' @param catalog_id A single catalog ID on the selected SENAITE instance.
#' @inheritParams get_senaite_data
#'
#' @returns A character vector of index names, in the order returned by the
#'   server. An empty index list returns `character(0)`. A missing or
#'   unrecognized `indexes` field raises an error.
#'
#' @examples
#' \dontrun{
#' indexes <- senaite_get_catalog_indexes(
#'   "senaite_catalog_client",
#'   profile = "lab-lan"
#' )
#' "getClientID" %in% indexes
#' }
#'
#' @export
senaite_get_catalog_indexes <- function(catalog_id, profile = "default") {
  if (!is.character(catalog_id) || length(catalog_id) != 1L ||
      is.na(catalog_id) ||
      !grepl("^[A-Za-z0-9_~-][A-Za-z0-9._~-]*\\z", catalog_id, perl = TRUE)) {
    stop("`catalog_id` must be a single catalog ID, not a path or URL.",
         call. = FALSE)
  }

  auth <- senaite_credentials(profile)
  path <- paste0("catalogs/", catalog_id)
  metadata <- senaite_get_json(path = path, auth = auth)

  if (!is.list(metadata) || is.null(metadata[["indexes"]])) {
    fields <- names(metadata)
    stop(
      "No top-level `indexes` field in the response from '", path, "'. ",
      "Response fields: ",
      if (length(fields)) paste(fields, collapse = ", ") else "<none>",
      ".", call. = FALSE
    )
  }

  indexes <- metadata[["indexes"]]

  # A JSON object keyed by index name: return its keys, not its values.
  if (is.list(indexes) && !is.null(names(indexes)) &&
      all(nzchar(names(indexes)))) {
    return(names(indexes))
  }

  # With simplifyVector = FALSE, a JSON array of strings is an unnamed
  # list. vapply() also returns character(0) for an empty array.
  is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  if (is.list(indexes) && is.null(names(indexes)) &&
      all(vapply(indexes, is_string, logical(1)))) {
    return(vapply(indexes, identity, character(1), USE.NAMES = FALSE))
  }

  stop("Unrecognized `indexes` format in the response from '", path, "'.",
       call. = FALSE)
}
