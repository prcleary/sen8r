#' Build a SENAITE object creation payload
#'
#' @param portal_type SENAITE object type, for example `"SamplePoint"`.
#' @param parent_path Absolute site path of the destination container,
#'   for example `"/senaite/setup/samplepoints"`.
#' @param fields Named list of fields for the new object. Field names and
#'   values are preserved; SENAITE validates type-specific requirements.
#'
#' @returns A named list suitable for [post_senaite_data()].
#' @examples
#' senaite_create_body(
#'   "SamplePoint", "/senaite/setup/samplepoints",
#'   fields = list(title = "Training sample point")
#' )
#' @export
senaite_create_body <- function(portal_type, parent_path, fields = list()) {
  is_string <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }

  if (!is_string(portal_type)) {
    stop("`portal_type` must be one non-empty string.", call. = FALSE)
  }
  if (!is_string(parent_path) || !startsWith(parent_path, "/")) {
    stop("`parent_path` must be a site path beginning with '/'.", call. = FALSE)
  }
  if (!is.list(fields) || is.data.frame(fields)) {
    stop("`fields` must be a named list or list().", call. = FALSE)
  }

  if (length(fields)) {
    keys <- names(fields)
    if (is.null(keys) || anyNA(keys) || any(!nzchar(keys)) ||
        anyDuplicated(keys)) {
      stop("Each field must have a unique, non-empty name.", call. = FALSE)
    }
    if (any(keys %in% c("portal_type", "parent_path"))) {
      stop("Supply `portal_type` and `parent_path` as arguments, not fields.",
           call. = FALSE)
    }
  }

  c(list(portal_type = portal_type, parent_path = parent_path), fields)
}
