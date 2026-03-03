
#' Download a Report from the SENAITE API
#'
#' Downloads a report file from a SENAITE LIMS API endpoint using
#' HTTP basic authentication and saves it locally.
#'
#' The function performs a `GET` request to the provided URL. If the
#' request is successful (HTTP status 200), the response content is
#' written to `output_file` as binary data.
#'
#' Credentials default to values stored in the system keyring.
#'
#' @param url `character(1)`. Fully qualified URL to the SENAITE report
#'   endpoint (e.g., a PDF or other binary report resource).
#'
#' @param output_file `character(1)`. Path (including filename) where the
#'   downloaded file should be saved. If the file already exists, it will
#'   be overwritten.
#'
#' @param username `character(1)`. SENAITE username used for authentication.
#'   Defaults to the value stored in the system keyring under
#'   `"senaite_username"`.
#'
#' @param password `character(1)`. SENAITE password used for authentication.
#'   Defaults to the value stored in the system keyring under
#'   `"senaite_password"`.
#'
#' @returns
#' Invisibly returns `TRUE` if the download succeeds (HTTP 200),
#' otherwise invisibly returns `FALSE`.
#'
#' @details
#' Authentication is performed using HTTP Basic Authentication via
#' \code{httr::authenticate()}.
#'
#' If the request fails, a warning message including the HTTP status code
#' is displayed using \code{cli::cli_alert_warning()}.
#'
#' This function does not automatically create directories for
#' `output_file`. Ensure the target directory exists before calling.
#'
#' @examples
#' \dontrun{
#' # Example: download a PDF report
#' report_url <- "https://lims.example.com/@@API/senaite/v1/report/AR-0001"
#'
#' download_senaite_report(
#'   url = report_url,
#'   output_file = "AR-0001-report.pdf",
#'   username = "your_username",
#'   password = "your_password"
#' )
#' }
#'
#' @seealso
#' \code{\link[httr]{GET}},
#' \code{\link[httr]{authenticate}},
#' \code{\link[keyring]{key_get}}
#'
#' @export
download_senaite_report <- function(url,
                                    output_file,
                                    username = keyring::key_get('senaite_username'),
                                    password = keyring::key_get('senaite_password')) {
  response <- httr::GET(url, httr::authenticate(username, password))
  if (httr::status_code(response) == 200) {
    writeBin(httr::content(response, 'raw'), output_file)
    cli::cli_alert_info('Download successful: {output_file}')
    invisible(TRUE)
  } else {
    cli::cli_alert_warning(
      'Failed to download file.\nHTTP Status: {httr::status_code(response)}'
    )
    invisible(FALSE)
  }
}
