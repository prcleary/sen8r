#' Flatten SENAITE records into a data.table
#'
#' Convert a collection of parsed SENAITE records to one row per record.
#' Named nested objects become dotted columns. Arrays never create extra rows.
#'
#' @param x A list of records, as returned by [get_senaite_data()]. Each
#'   record must be a named list. Supply a collection, not a single record.
#' @param collapse A single string used to join arrays of simple values.
#'   Defaults to `NULL` to keep arrays as list-columns.
#'
#' @returns A `data.table` in input row order. Columns are ordered by first
#'   appearance. Missing scalar values are `NA`; missing list-column values
#'   are `NULL`. A field that is `NULL` in every record is logical `NA`.
#'   An empty collection returns a zero-row, zero-column `data.table`.
#'
#' @details
#' Intended for JSON parsed with `simplifyVector = FALSE`. Unnamed lists
#' represent arrays; non-empty named lists represent objects. Multi-element
#' atomic vectors are also accepted as arrays. Arrays containing objects,
#' nested arrays, or nulls are kept intact rather than converted to text.
#'
#' Integer and double scalars may share a numeric column. Other incompatible
#' scalar types are kept in a list-column rather than silently coerced.
#' Literal dots in object field names are rejected to prevent collisions
#' with generated paths such as `location.latitude`.
#'
#' @examples
#' records <- list(
#'   list(uid = "a", title = "North", count = 2L,
#'        location = list(latitude = 51.5), tags = list("urgent", "repeat")),
#'   list(uid = "b", title = "South", count = NULL, tags = list())
#' )
#' flatten_senaite_output(records)
#' flatten_senaite_output(records, collapse = NULL)
#'
#' @export
flatten_senaite_output <- function(x, collapse = NULL) {
  if (is.null(x) || !is.list(x) || is.data.frame(x)) {
    stop("`x` must be a list of SENAITE records.", call. = FALSE)
  }
  if (!is.null(collapse) &&
      (!is.character(collapse) || length(collapse) != 1L || is.na(collapse))) {
    stop("`collapse` must be a single string or NULL.", call. = FALSE)
  }
  if (!length(x)) {
    return(data.table::data.table())
  }

  # JSON scalar types. Check before combining values so, for example,
  # a numeric count isn't turned into text by a title in the same record.
  is_scalar <- function(v) {
    !is.object(v) && is.null(dim(v)) && length(v) == 1L &&
      typeof(v) %in% c("logical", "integer", "double", "character")
  }

  flatten_record <- function(record, i) {
    if (!is.list(record) || is.data.frame(record) ||
        (length(record) && is.null(names(record)))) {
      stop("Record ", i, " must be a named list.", call. = FALSE)
    }

    row <- list()

    walk <- function(obj, prefix = "") {
      keys <- names(obj)
      if (anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys) ||
          any(grepl(".", keys, fixed = TRUE))) {
        stop(
          "Record ", i, ": field names must be unique within each object, ",
          "non-empty, and free of literal dots.",
          call. = FALSE
        )
      }

      for (j in seq_along(obj)) {
        key <- if (nzchar(prefix)) paste(prefix, keys[j], sep = ".") else keys[j]
        value <- obj[[j]]

        if (is.list(value) && !is.null(names(value)) && length(value)) {
          walk(value, key)
          next
        }

        if (!is.null(value) && !is_scalar(value) && !is.null(collapse)) {
          # A list of objects, or an array with a null element, is not
          # safely representable as a delimited string.
          parts <- if (is.atomic(value)) as.list(value) else value
          if (all(vapply(parts, is_scalar, logical(1))) &&
              !any(vapply(parts, anyNA, logical(1)))) {
            value <- paste(vapply(parts, as.character, character(1)),
                           collapse = collapse)
          }
        }

        # [key] <- list(value) retains a named NULL; [[key]] <- NULL
        # would delete the field.
        row[key] <<- list(value)
      }
    }

    walk(record)
    row
  }

  rows <- lapply(seq_along(x), function(i) flatten_record(x[[i]], i))
  fields <- unique(unlist(lapply(rows, names), use.names = FALSE))

  if (!length(fields)) {
    stop("The collection contains records but no named fields.", call. = FALSE)
  }

  make_column <- function(key) {
    values <- lapply(rows, `[[`, key)
    present <- !vapply(values, is.null, logical(1))

    if (!any(present)) {
      return(rep(NA, length(rows)))
    }

    observed <- values[present]
    if (!all(vapply(observed, is_scalar, logical(1)))) {
      return(values)
    }

    types <- unique(vapply(observed, typeof, character(1)))
    if (length(types) > 1L && !all(types %in% c("integer", "double"))) {
      return(values)
    }

    # Allocate from the observed type so missing integer, character, etc.
    # values get the appropriate typed NA.
    combined <- unlist(observed, use.names = FALSE)
    column <- combined[rep(NA_integer_, length(rows))]
    column[present] <- combined
    column
  }

  columns <- lapply(fields, make_column)
  names(columns) <- fields
  data.table::as.data.table(columns)
}
