#' Map one spreadsheet row to create_labcontact arguments
#'
#' Requires `firstname` and `surname`. Optional scalar columns use the
#' argument names of `create_labcontact`. Address columns use `physical_`
#' or `postal_` followed by address, city, district, state, zip, or country.
#' Blank optional cells are omitted. Non-blank cells must contain text.
#'
#' @param row A one-row data frame or tibble.
#' @returns A named argument list for `create_labcontact`, excluding profile.
#' @export
labcontact_row_args <- function(row) {
  if (!is.data.frame(row) || nrow(row) != 1L) {
    stop("`row` must be a one-row data frame.", call. = FALSE)
  }

  required <- c("firstname", "surname")
  optional <- c(
    "salutation", "middle_initial", "middle_name", "job_title",
    "email_address", "business_phone", "business_fax",
    "home_phone", "mobile_phone"
  )
  parts <- c("address", "city", "district", "state", "zip", "country")
  allowed <- c(required, optional,
               paste0("physical_", parts), paste0("postal_", parts))

  unknown <- setdiff(names(row), allowed)
  if (length(unknown)) {
    stop("Unrecognised LabContact column(s): ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }

  read_cell <- function(key, required = FALSE) {
    x <- row[[key]]
    if (is.factor(x)) x <- as.character(x)

    # Accept absent columns and imported blank cells (including logical NA).
    if (is.null(x) || (is.atomic(x) && length(x) == 1L && is.na(x))) {
      if (required) stop("Missing `", key, "`.", call. = FALSE)
      return(NULL)
    }
    if (!is.character(x) || length(x) != 1L) {
      stop("`", key, "` must contain text. Check the spreadsheet cell ",
           "and import type.", call. = FALSE)
    }
    x <- trimws(x)
    if (!nzchar(x)) {
      if (required) stop("Blank `", key, "`.", call. = FALSE)
      return(NULL)
    }
    unname(x)
  }

  args <- list()
  for (key in c(required, optional)) {
    value <- read_cell(key, required = key %in% required)
    if (!is.null(value)) args[[key]] <- value
  }

  for (prefix in c("physical", "postal")) {
    address <- list()
    for (part in parts) {
      value <- read_cell(paste(prefix, part, sep = "_"))
      if (!is.null(value)) address[[part]] <- value
    }
    if (length(address)) args[[paste0(prefix, "_address")]] <- address
  }

  args
}
