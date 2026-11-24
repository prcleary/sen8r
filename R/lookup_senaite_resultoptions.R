#' Translate SENAITE result codes to labels
#'
#' @param result A single character string. When options are supplied,
#'   this must encode a JSON string, number, or array of those values.
#' @param result_options A list of option records with `ResultValue` and
#'   `ResultText` fields. `NULL` or an empty list means no translation.
#'
#' @returns A single character string. Labels are joined with `", "` in
#'   selection order. Unknown codes are retained with a warning. With no
#'   options, `result` is returned unchanged. A missing result, JSON `null`,
#'   or an empty selection returns `NA_character_` when options are supplied.
#'
#' @examples
#' opts <- list(
#'   list(ResultValue = "P", ResultText = "Positive"),
#'   list(ResultValue = "N", ResultText = "Negative")
#' )
#' lookup_senaite_resultoptions('["N","P"]', opts)
#' lookup_senaite_resultoptions("Not detected", list())
#'
#' @export
lookup_senaite_resultoptions <- function(result, result_options) {
  if (!is.character(result) || length(result) != 1L) {
    stop("`result` must be a single character string.", call. = FALSE)
  }
  if (!length(result_options) || is.na(result)) {
    return(result)
  }

  selected <- jsonlite::fromJSON(result, simplifyVector = FALSE)
  if (is.null(selected) || identical(selected, list())) {
    return(NA_character_)
  }

  is_code <- function(x) {
    (is.character(x) || is.numeric(x)) && length(x) == 1L && !is.na(x)
  }

  # A JSON scalar is one selection; an unnamed JSON array can hold several.
  if (!is.list(selected)) selected <- list(selected)
  if (!is.null(names(selected)) || !all(vapply(selected, is_code, logical(1)))) {
    stop("Expected a JSON code or an array of codes.", call. = FALSE)
  }
  codes <- vapply(selected, as.character, character(1))

  keys <- vapply(result_options, function(option) {
    key <- option[["ResultValue"]]
    if (!is_code(key)) {
      stop("Each option needs a single `ResultValue`.", call. = FALSE)
    }
    as.character(key)
  }, character(1))

  labels <- vapply(result_options, function(option) {
    label <- option[["ResultText"]]
    if (!is.character(label) || length(label) != 1L || is.na(label)) {
      stop("Each option needs a single `ResultText` label.", call. = FALSE)
    }
    label
  }, character(1))

  if (anyDuplicated(keys)) {
    stop("`ResultValue` must be unique within an option set.", call. = FALSE)
  }

  index <- match(codes, keys)
  unknown <- is.na(index)
  if (any(unknown)) {
    warning("Unknown result code(s): ",
            paste(unique(codes[unknown]), collapse = ", "), call. = FALSE)
  }

  output <- codes
  output[!unknown] <- labels[index[!unknown]]
  paste(output, collapse = ", ")
}
