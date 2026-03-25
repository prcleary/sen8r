#' View and Inspect SENAITE Output
#'
#' Converts a nested SENAITE API response object into a flat two-column
#' tibble for easy inspection. Optionally opens the result in the RStudio
#' data viewer.
#'
#' This function is primarily intended for exploratory analysis and
#' debugging of raw SENAITE API responses.
#'
#' @param x `list`. A (possibly nested) SENAITE API response object.
#'   Typically a single item returned by \code{get_senaite_data()}.
#'
#' @param new_tab `logical(1)`. If `TRUE` (default), opens the flattened
#'   output in the RStudio Viewer using \code{View()}. If `FALSE`, the
#'   tibble is returned invisibly without opening a viewer tab.
#'
#' @returns
#' Invisibly returns a `tibble` with two columns:
#'
#' - `name`: field names
#' - `value`: corresponding values (after flattening with `unlist()`)
#'
#' @details
#' The function:
#'
#' 1. Flattens the input object using \code{unlist()}.
#' 2. Converts it to a two-column tibble using \code{tibble::enframe()}.
#' 3. Optionally opens it in the RStudio Viewer.
#'
#' ## RStudio Requirement
#'
#' The `View()` function is intended for interactive use in RStudio.
#' In non-interactive or non-RStudio environments, `new_tab = TRUE`
#' may have no effect.
#'
#' @examples
#' \dontrun{
#' # Retrieve one analysis request
#' ars <- get_senaite_data("analysisrequest", limit = 1)
#'
#' # View raw output structure
#' view_senaite_output(ars[[1]])
#'
#' # Get tibble without opening Viewer
#' output_tbl <- view_senaite_output(ars[[1]], new_tab = FALSE)
#' }
#'
#' @seealso
#' \code{\link[tibble]{enframe}},
#' \code{\link{get_senaite_data}}
#'
#' @export
view_senaite_output <- function(x, new_tab = TRUE) {
  senaite_output <- tibble::enframe(unlist(x))
  if (new_tab) {
    View <- get("View", envir = as.environment("package:utils"))
    title <- deparse(substitute(x))
    View(senaite_output, title)
  }
  invisible(senaite_output)
}
