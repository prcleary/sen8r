#' Retrieve records from a SENAITE API endpoint
#'
#' Sends an authenticated GET request to the SENAITE instance stored in a
#' named profile. For collection responses, retrieves all pages and returns
#' their items as one list.
#'
#' @param endpoint A single endpoint name, such as `"client"`,
#'   `"analysisrequest"`, or `"sampletype"`. Supply a name, not a URL.
#' @param params A named list of query parameters, such as
#'   `list(review_state = "verified")`. Do not include `limit` or `b_start`;
#'   the getter manages pagination.
#' @param limit Positive whole number of records to request per page.
#'   This is a page size, not a limit on the total number returned.
#' @param verbose If `TRUE`, report download progress.
#' @param profile Name of the connection profile saved by [senaite_setup()].
#'   Defaults to `"default"`.
#'
#' @returns A list of SENAITE records. An empty collection returns `list()`.
#'   For compatibility with the original getter, a successful response
#'   without a `pages` field returns `NULL`. HTTP errors, including 404,
#'   raise an error.
#'
#' @examples
#' \dontrun{
#' clients <- get_senaite_data("client", profile = "lab-lan")
#'
#' verified <- get_senaite_data(
#'   "analysisrequest",
#'   params = list(review_state = "verified"),
#'   limit = 50,
#'   profile = "lab-lan"
#' )
#' }
#'
#' @seealso [senaite_setup()]
#' @export
get_senaite_data <- function(endpoint = "analysis",
                             params = list(),
                             limit = 100,
                             verbose = TRUE,
                             profile = "default") {

  # Accept an endpoint name only. The destination comes from the profile.
  if (!is.character(endpoint) || length(endpoint) != 1L ||
      is.na(endpoint) || !grepl("^[A-Za-z][A-Za-z0-9_]*$", endpoint)) {
    stop(
      "`endpoint` must be a single endpoint name, e.g. 'client', not a URL.",
      call. = FALSE
    )
  }

  if (!is.list(params) || is.data.frame(params)) {
    stop("`params` must be a named list, or list().", call. = FALSE)
  }

  if (length(params)) {
    nm <- names(params)

    if (is.null(nm) || anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm)) {
      stop("Each entry in `params` must have a unique, non-empty name.",
           call. = FALSE)
    }

    if (any(nm %in% c("limit", "b_start"))) {
      stop(
        "Use the `limit` argument for page size. ",
        "Do not supply `limit` or `b_start` in `params`.",
        call. = FALSE
      )
    }
  }

  if (!is.numeric(limit) || length(limit) != 1L || !is.finite(limit) ||
      limit < 1 || limit != floor(limit)) {
    stop("`limit` must be a positive whole number.", call. = FALSE)
  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }

  auth <- senaite_credentials(profile)

  fetch_page <- function(page, offset) {
    query <- params
    query$limit <- limit
    if (offset > 0) {
      query$b_start <- offset
    }

    if (verbose) {
      cli::cli_alert_info("Downloading {endpoint}: page {page}")
    }

    senaite_get_json(path = endpoint, auth = auth, query = query)
  }
  first <- fetch_page(page = 1L, offset = 0)

  if (is.null(first[["pages"]])) {
    if (verbose) {
      cli::cli_alert_info(
        "The {endpoint} response has no 'pages' field; returning NULL."
      )
    }
    return(NULL)
  }

  n_pages <- suppressWarnings(as.numeric(first[["pages"]]))
  if (length(n_pages) != 1L || !is.finite(n_pages) ||
      n_pages < 0 || n_pages != floor(n_pages)) {
    stop("SENAITE returned an invalid 'pages' value.", call. = FALSE)
  }

  if (n_pages == 0) {
    return(list())
  }

  page_items <- function(body, page) {
    items <- body[["items"]]
    if (is.null(items) || !is.list(items)) {
      stop("SENAITE page ", page, " did not contain an 'items' list.",
           call. = FALSE)
    }
    items
  }

  batches <- vector("list", n_pages)
  batches[[1L]] <- page_items(first, 1L)

  if (n_pages > 1L) {
    for (page in seq.int(2L, n_pages)) {
      offset <- (page - 1) * limit
      batches[[page]] <- page_items(fetch_page(page, offset), page)
    }
  }

  # Join the pages without flattening the fields inside each record.
  do.call(c, unname(batches))
}
