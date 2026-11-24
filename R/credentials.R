# SENAITE credential storage -----------------------------------------------
#
# Public function:
#   senaite_setup()        Create or update a named connection profile.
#
# Internal function for API callers:
#   senaite_credentials()  Read a profile's URL, username, and password.


# Backend selection --------------------------------------------------------

# Reuse the selected backend within the current R session.
# This environment holds the backend object, not a list of SENAITE passwords.
.senaite_state <- new.env(parent = emptyenv())

senaite_backend <- function() {
  if (!is.null(.senaite_state$backend)) {
    return(.senaite_state$backend)
  }

  os <- Sys.info()[["sysname"]]

  backend <- switch(
    os,
    Windows = keyring::backend_wincred$new(),
    Linux   = keyring::backend_file$new(),
    stop(
      "SENAITE credential storage is supported on Windows and Linux. ",
      "Detected OS: ", os, ".",
      call. = FALSE
    )
  )

  .senaite_state$backend <- backend
  backend
}


# Key names and validation -------------------------------------------------

# The profile entry stores connection settings, not the password.
senaite_profile_key <- function(profile) {
  if (!is.character(profile) || length(profile) != 1L || is.na(profile) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", profile)) {
    stop(
      "`profile` must be a name such as 'default', 'production', or 'lab-lan'.",
      call. = FALSE
    )
  }

  paste0("sen8r:profile:", profile)
}

# The service name includes the scheme, host, port, and instance path.
# The backend's separate username argument identifies the account.
senaite_password_key <- function(base_url) {
  paste0("sen8r:password:", base_url)
}

senaite_normalize_base_url <- function(base_url, allow_http = FALSE) {
  if (!is.logical(allow_http) || length(allow_http) != 1L || is.na(allow_http)) {
    stop("`allow_http` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.character(base_url) || length(base_url) != 1L || is.na(base_url)) {
    stop("Supply one SENAITE instance URL.", call. = FALSE)
  }

  # Accept a pasted URL with surrounding whitespace or a trailing slash.
  base_url <- sub("/+$", "", trimws(base_url))

  if (!grepl("^https?://", base_url) ||
      grepl("[?#[:space:][:cntrl:]]", base_url) ||
      grepl("\\", base_url, fixed = TRUE)) {
    stop(
      "Use a full http:// or https:// instance URL without spaces, ",
      "backslashes, a query string, or a fragment.",
      call. = FALSE
    )
  }

  url <- httr::parse_url(base_url)

  if (is.null(url$hostname) || !nzchar(url$hostname) ||
      !is.null(url$username) || !is.null(url$password)) {
    stop(
      "The URL must include a hostname or IP address. ",
      "Supply the username and password separately, not in the URL.",
      call. = FALSE
    )
  }

  if (identical(url$scheme, "http") && !allow_http) {
    stop(
      "This SENAITE instance uses HTTP. Use HTTPS if available, or set ",
      "`allow_http = TRUE` when configuring this profile.",
      call. = FALSE
    )
  }

  base_url
}


# Public setup function ----------------------------------------------------

#' Configure credentials for a named SENAITE instance
#'
#' Stores connection settings and a password for a SENAITE instance.
#' Windows uses Windows Credential Store. Linux uses an encrypted file
#' keyring, which may prompt for its own passphrase when created or unlocked.
#' The keyring passphrase is separate from the SENAITE account password.
#'
#' @param profile Local profile name, such as `"default"`, `"lab-lan"`,
#'   or `"production"`. Run setup again with the same name to update it.
#' @param base_url Full SENAITE instance URL, including the port and instance
#'   path if applicable. If `NULL`, prompts interactively.
#' @param username SENAITE account name. If `NULL`, prompts interactively.
#' @param allow_http Whether to accept an unencrypted HTTP URL for this
#'   profile. Defaults to `FALSE`. Only enable this deliberately.
#'
#' @details Profiles using the same instance URL and username share a
#'   password entry. Updating that password updates it for both profiles.
#'   Setup stores credentials; it does not test a login to SENAITE.
#'
#' @returns Invisibly returns `NULL`.
#' @examples
#' \dontrun{
#' senaite_setup(
#'   profile = "lab-lan",
#'   base_url = "http://192.168.1.193:8080/senaite",
#'   allow_http = TRUE
#' )
#'
#' senaite_setup(
#'   profile = "production",
#'   base_url = "https://lab.example.org/senaite"
#' )
#' }
#' @export
senaite_setup <- function(profile = "default", base_url = NULL,
                          username = NULL, allow_http = FALSE) {
  profile_key <- senaite_profile_key(profile)

  if (!interactive()) {
    stop("Run senaite_setup() in an interactive R session.", call. = FALSE)
  }

  backend <- senaite_backend()

  if (is.null(base_url)) {
    base_url <- readline(sprintf("SENAITE instance URL for '%s': ", profile))
  }
  base_url <- senaite_normalize_base_url(base_url, allow_http = allow_http)

  uses_http <- startsWith(base_url, "http://")
  if (uses_http) {
    warning(
      "HTTP is enabled for ", base_url, ". Credentials and API data ",
      "will not be encrypted in transit.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  if (is.null(username)) {
    username <- readline(sprintf("SENAITE username for %s: ", base_url))
  }
  if (!is.character(username) || length(username) != 1L || is.na(username) ||
      !nzchar(trimws(username))) {
    stop("Supply a non-empty SENAITE username.", call. = FALSE)
  }
  username <- trimws(username)

  config <- list(
    base_url   = base_url,
    username   = username,
    allow_http = uses_http
  )

  # The backend prompts for the password without putting it in R history.
  # Write it first so cancelling the prompt does not repoint the profile.
  message("Enter the SENAITE account password for ", username, " at ", base_url, ".")
  backend$set(
    service  = senaite_password_key(base_url),
    username = username
  )

  backend$set_with_value(
    service  = profile_key,
    username = "config",
    password = as.character(jsonlite::toJSON(config, auto_unbox = TRUE))
  )

  message("Saved SENAITE profile '", profile, "' for ", base_url, ".")
  invisible(NULL)
}


# Internal retrieval function ----------------------------------------------

# Use this inside the package's HTTP request code. Do not log or print its
# result: the password must be available in memory to authenticate a request.
senaite_credentials <- function(profile = "default") {
  profile_key <- senaite_profile_key(profile)
  backend <- senaite_backend()

  config_json <- tryCatch(
    backend$get(service = profile_key, username = "config"),
    error = function(e) {
      stop(
        "Cannot read SENAITE profile '", profile, "'. ",
        "Check that the credential store can be unlocked and that you ",
        "have run senaite_setup() for this profile.\n",
        "Backend message: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  config <- jsonlite::fromJSON(config_json)
  base_url <- senaite_normalize_base_url(
    config$base_url,
    allow_http = isTRUE(config$allow_http)
  )

  password <- backend$get(
    service  = senaite_password_key(base_url),
    username = config$username
  )

  invisible(list(
    base_url = base_url,
    username = config$username,
    password = password
  ))
}
