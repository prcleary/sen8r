#' Fetch one SENAITE record by UID
#'
#' Makes one GET request using the record UID. The caller does not need
#' to know the portal type or collection name.
#'
#' @param uid A SENAITE record UID (32 hexadecimal characters).
#' @param profile SENAITE connection profile.
#' @returns The record as a named list, not a list of records.
#' @export
get_senaite_record <- function(uid, profile = "default") {
  if (!is.character(uid) || length(uid) != 1L || is.na(uid)) {
    stop("`uid` must be one SENAITE record UID.", call. = FALSE)
  }

  uid <- tolower(trimws(uid))
  if (nchar(uid) != 32L || !grepl("^[0-9a-f]{32}$", uid)) {
    stop("`uid` must contain exactly 32 hexadecimal characters, not a URL.",
         call. = FALSE)
  }

  body <- senaite_get_json(
    path = uid,
    auth = senaite_credentials(profile),
    query = list(complete = "yes")
  )

  # Accept either a record directly or a response containing one item.
  record <- NULL
  if (is.list(body) && !is.null(body[["uid"]])) {
    record <- body
  } else if (is.list(body) && "items" %in% names(body)) {
    items <- body[["items"]]
    if (is.list(items) && length(items) == 0L) {
      stop("No record was returned for UID ", uid, ".", call. = FALSE)
    }
    if (is.list(items) && length(items) == 1L) {
      record <- items[[1L]]
    }
  }

  if (!is.list(record) || is.null(record[["uid"]])) {
    stop("Expected one SENAITE record with a UID. Response fields: ",
         paste(names(body), collapse = ", "), ".", call. = FALSE)
  }
  if (!identical(tolower(record[["uid"]]), uid)) {
    stop("SENAITE returned a different UID from the one requested.",
         call. = FALSE)
  }

  record
}
