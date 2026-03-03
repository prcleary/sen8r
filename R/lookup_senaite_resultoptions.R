#' Translate SENAITE Result Codes to Human-Readable Text
#'
#' Converts encoded SENAITE result values into their corresponding
#' human-readable labels using a provided result options table.
#'
#' This is typically used for analyses where results are stored as
#' coded values (e.g., JSON-encoded selections) and must be mapped
#' to descriptive text.
#'
#' @param result `character(1)`. A JSON-encoded string representing one or
#'   more selected result values (e.g., `"[\"P\", \"N\"]"`).
#'
#' @param result_options `list`. A list of result option objects, each
#'   containing at least:
#'   - `ResultValue`: the stored code
#'   - `ResultText`: the human-readable label
#'
#' @returns
#' If matching result options are found:
#' - A `character(1)` containing the corresponding `ResultText`.
#' - If multiple matches are found, they are concatenated with `", "`.
#'
#' If no result options are provided (empty list), returns the original
#' `result` unchanged.
#'
#' @details
#' The function:
#'
#' 1. Parses the JSON-encoded `result` using \code{jsonlite::fromJSON()}.
#' 2. Converts `result_options` into a `data.table`.
#' 3. Matches `ResultValue` against the parsed result values.
#' 4. Returns the corresponding `ResultText`.
#'
#' This is particularly useful when working with categorical analysis
#' results returned by the SENAITE API.
#'
#' @examples
#' \dontrun{
#' result_json <- "[\"P\", \"N\"]"
#'
#' options_list <- list(
#'   list(ResultValue = "P", ResultText = "Positive"),
#'   list(ResultValue = "N", ResultText = "Negative")
#' )
#'
#' lookup_senaite_resultoptions(result_json, options_list)
#' # Returns: "Positive, Negative"
#' }
#'
#' @seealso
#' \code{\link[jsonlite]{fromJSON}},
#' \code{\link[data.table]{rbindlist}}
#'
#' @export
lookup_senaite_resultoptions <- function(result, result_options) {
  res1 <- jsonlite::fromJSON(result)
  ro1 <- data.table::rbindlist(result_options, fill = TRUE, use.names = TRUE)
  if (nrow(ro1) > 0) {
    output <- ro1[ResultValue %in% res1, ResultText]
    if (length(output) > 1) {
      output2 <- paste0(output, collapse = ', ')
      output2
    } else {
      output
    }
  } else {
    result
  }
}
