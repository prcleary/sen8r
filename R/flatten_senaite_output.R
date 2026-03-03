#' Flatten Nested SENAITE API Output into a Tabular Format
#'
#' Converts a list of nested SENAITE API response objects into a flat
#' tabular structure. Each element of the input list is unpacked,
#' transformed into a one-row data frame, and combined into a single
#' `data.table`.
#'
#' Nested list elements are unlisted, and duplicated field names are
#' collapsed into a single string using the specified separator.
#'
#' @param x `list`. A list of SENAITE API response objects. Each element
#'   should be a named list (possibly nested) representing one record.
#'
#' @param collapse `character(1)`. A string used to concatenate multiple
#'   values that map to the same field name. Defaults to `"+"`.
#'
#' @returns
#' A `data.table` where:
#' - Each element of `x` becomes one row
#' - List names become column names
#' - Repeated values are collapsed into a single string
#' - Missing fields are filled with `NA`
#'
#' @details
#' The function performs the following steps:
#'
#' 1. Unlists each record in `x`
#' 2. Converts it to a two-column tibble (`name`, `value`)
#' 3. Pivots to wide format
#' 4. Collapses duplicate field values using `collapse`
#' 5. Row-binds all records into a single `data.table`
#'
#' This is particularly useful for flattening JSON responses from
#' the SENAITE REST API into an analysis-ready structure.
#'
#' @examples
#' \dontrun{
#' api_output <- list(
#'   list(id = "S1", client = "Client A", tags = c("urgent", "external")),
#'   list(id = "S2", client = "Client B", tags = c("internal"))
#' )
#'
#' flatten_senaite_output(api_output)
#' }
#'
#' @seealso
#' \code{\link[tibble]{enframe}},
#' \code{\link[tidyr]{pivot_wider}},
#' \code{\link[data.table]{rbindlist}}
#'
#' @export
flatten_senaite_output <- function(x, collapse = '+') {
  unpacked_list <- lapply(x, function(item) {
    enframed <- tibble::enframe(unlist(item))
    enframed |> tidyr::pivot_wider(
      names_from = 'name',
      values_from = 'value',
      values_fn = \(x) paste0(x, collapse = collapse)
    )
  })
  data.table::rbindlist(unpacked_list, use.names = TRUE, fill = TRUE)
}
