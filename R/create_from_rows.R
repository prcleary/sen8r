#' Create SENAITE records from spreadsheet rows
#'
#' Maps every row before making any create calls. In a live run, stops
#' after the first create or UID-extraction error. Never retries a POST.
#'
#' @param data A data frame or tibble with one row per record.
#' @param create A single-record create function accepting `profile`.
#'   It should return only after completing its own read-back checks.
#' @param map_row A function taking a one-row data frame and returning
#'   a named list of arguments for `create`, excluding `profile`.
#'   It should validate required values and must not make any writes.
#' @param profile SENAITE connection profile.
#' @param dry_run If TRUE (default), prepare arguments without posting.
#' @param get_uid Function extracting one UID string from a successful
#'   create response. Defaults to the SENAITE single-item create format.
#' @returns A list with `report` (one row per input row) and `arguments`
#'   (the mapped arguments, in the same order). `input_row` is the
#'   one-based data-frame row number, not the Excel worksheet row number.
#' @export
create_from_rows <- function(data, create, map_row, profile = "default",
                             dry_run = TRUE,
                             get_uid = function(x) {
                               if (length(x$items) == 1L) x$items[[1L]]$uid
                             }) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }
  cols <- names(data)
  if (anyNA(cols) || any(!nzchar(cols)) || anyDuplicated(cols)) {
    stop("Spreadsheet columns must have unique, non-empty names.",
         call. = FALSE)
  }
  if (!is.function(create) || !is.function(map_row) || !is.function(get_uid)) {
    stop("`create`, `map_row`, and `get_uid` must be functions.",
         call. = FALSE)
  }
  if (!is.logical(dry_run) || length(dry_run) != 1L || is.na(dry_run)) {
    stop("`dry_run` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(profile) || length(profile) != 1L ||
      is.na(profile) || !nzchar(trimws(profile))) {
    stop("`profile` must be one non-empty string.", call. = FALSE)
  }

  params <- names(formals(create))
  has_dots <- "..." %in% params
  if (!has_dots && !"profile" %in% params) {
    stop("`create` must accept a `profile` argument.", call. = FALSE)
  }

  n <- nrow(data)
  arguments <- vector("list", n)
  report <- data.frame(
    input_row = seq_len(n),
    status = rep("not_attempted", n),
    uid = rep(NA_character_, n),
    message = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )

  # Phase 1: prepare every row; no create calls in this loop.
  for (i in seq_len(n)) {
    args <- tryCatch({
      a <- map_row(data[i, , drop = FALSE])
      keys <- names(a)
      if (!is.list(a) || is.data.frame(a) || !length(a) ||
          is.null(keys) || anyNA(keys) || any(!nzchar(keys)) ||
          anyDuplicated(keys)) {
        stop("Mapper must return a non-empty, uniquely named argument list.")
      }
      if ("profile" %in% keys) {
        stop("Set `profile` on the runner, not in the row mapper.")
      }
      unknown <- setdiff(keys, params)
      if (!has_dots && length(unknown)) {
        stop("Unknown create argument(s): ", paste(unknown, collapse = ", "))
      }
      a
    }, error = identity)

    if (inherits(args, "error")) {
      report$status[i] <- "invalid"
      report$message[i] <- conditionMessage(args)
    } else {
      arguments[[i]] <- args
    }
  }

  if (any(report$status == "invalid")) {
    return(list(report = report, arguments = arguments))
  }
  if (dry_run) {
    report$status[] <- "ready"
    return(list(report = report, arguments = arguments))
  }

  # Phase 2: one live create at a time.
  for (i in seq_len(n)) {
    uid <- tryCatch({
      result <- do.call(create, c(arguments[[i]], list(profile = profile)))
      id <- get_uid(result)
      if (!is.character(id) || length(id) != 1L ||
          is.na(id) || !nzchar(trimws(id))) {
        stop("Create returned, but no single UID could be extracted.")
      }
      id
    }, error = identity)

    if (inherits(uid, "error")) {
      report$status[i] <- "needs_review"
      report$message[i] <- conditionMessage(uid)
      # Preserve a UID if the wrapper supplies it on the error condition.
      # For existing wrappers using plain stop(), keep their full message.
      id <- uid[["uid"]]
      if (is.character(id) && length(id) == 1L && !is.na(id)) {
        report$uid[i] <- id
      }
      break
    }

    report$status[i] <- "verified"
    report$uid[i] <- uid
  }

  list(report = report, arguments = arguments)
}
