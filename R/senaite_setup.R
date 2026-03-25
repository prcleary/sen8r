#' Interactive Setup for SENAITE API Credentials
#'
#' Launches an interactive setup process to configure local credentials
#' and required packages for working with the SENAITE API.
#'
#' This function:
#'
#' - Displays an informational dialog
#' - Installs required packages (if missing)
#' - Prompts the user to securely store:
#'   - SENAITE username
#'   - SENAITE password
#'   - SENAITE base URL
#'
#' Credentials are stored securely using the system keyring via
#' \code{keyring::key_set()}.
#'
#' @returns
#' Invisibly returns `NULL`. This function is called for its side effects.
#'
#' @details
#' ## Credential Storage
#'
#' The following keys are created in the system keyring:
#'
#' - `"senaite_username"`
#' - `"senaite_password"`
#' - `"BASE_URL"`
#'
#' These values are used as defaults in other package functions.
#'
#' ## Base URL Format
#'
#' The base URL should follow this format:
#'
#' ```
#' https://senaite.example.com/instancename
#' ```
#'
#' Do not include a trailing slash.
#'
#' ## RStudio Requirement
#'
#' This function uses \code{rstudioapi::showDialog()} and is intended to be
#' run inside RStudio. In non-RStudio environments, the dialog may not appear.
#'
#' ## Package Installation
#'
#' If the `{pacman}` package is not installed, it will be installed
#' automatically. Required dependencies are then loaded.
#'
#' @examples
#' \dontrun{
#' # Run interactive setup
#' senaite_setup()
#' }
#'
#' @seealso
#' \code{\link[keyring]{key_set}},
#' \code{\link[rstudioapi]{showDialog}}
#'
#' @export
senaite_setup <- function() {
  rstudioapi::showDialog(
    'Setup',
    'When you click OK, you will be asked to enter your user name and password and the URL for SENAITE (in the format "https://senaite.example.com/instancename"). These will be stored as secrets on your computer so that you do not have to enter them again.'
  )
  if (!requireNamespace('pacman', quietly = TRUE)) {
    utils::install.packages('pacman')
  }
  standard_packages <- c(
    'DBI',
    'assertthat',
    'cli',
    'data.table',
    'dplyr',
    'glue',
    'httr',
    'janitor',
    'jsonlite',
    'keyring',
    'lubridate',
    'purrr',
    'rstudioapi',
    'stringr',
    'tibble',
    'tidyr'
  )
  pacman::p_load(char = standard_packages)
  keyring::key_set('senaite_username', prompt = 'Enter Senaite user name: ')
  keyring::key_set('senaite_password', prompt = 'Enter Senaite password: ')
  keyring::key_set('BASE_URL', prompt = 'Enter base SENAITE URL: ')
}
