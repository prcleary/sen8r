#' Create a SENAITE laboratory contact
#'
#' Creates a LabContact using the form fields from the SENAITE JSON API
#' 2.6.0 create example. Reads the new contact back by UID before returning.
#' Each successful call creates a new record; names are not unique identifiers.
#'
#' @param firstname Contact's first name.
#' @param surname Contact's surname.
#' @param profile SENAITE connection profile.
#' @param parent_path Site path to the laboratory contacts folder.
#' @returns The original create response. The new contact's UID is in
#'   `result$items[[1L]]$uid`.
#' @export
create_labcontact <- function(firstname, surname, profile = "default",
                              parent_path = "/senaite/bika_setup/bika_labcontacts") {
  check_text <- function(x, name) {
    if (!is.character(x) || length(x) != 1L || is.na(x) ||
        !nzchar(trimws(x))) {
      stop("`", name, "` must be one non-empty string.", call. = FALSE)
    }
    trimws(x)
  }

  firstname   <- check_text(firstname, "firstname")
  surname     <- check_text(surname, "surname")
  parent_path <- check_text(parent_path, "parent_path")

  form <- senaite_create_body(
    portal_type = "LabContact",
    parent_path = parent_path,
    fields = list(Firstname = firstname, Surname = surname)
  )
  created <- post_senaite_data(
    body = form, profile = profile, encoding = "form"
  )

  uid <- if (length(created$items) == 1L) created$items[[1L]]$uid
  if (!is.character(uid) || length(uid) != 1L || is.na(uid) || !nzchar(uid)) {
    stop("POST succeeded but did not return one lab contact UID. ",
         "Check SENAITE before retrying.", call. = FALSE)
  }

  # The typed GET returns an items response, as with the department reader.
  contacts <- tryCatch(
    get_senaite_data(
      "labcontact",
      params = list(complete = "yes"),
      profile = profile
    ),
    error = function(e) {
      stop("LabContact ", uid, " was created, but read-back failed: ",
           conditionMessage(e), ". Do not repost.", call. = FALSE)
    }
  )

  matches <- Filter(function(x) identical(x$uid, uid), contacts)
  if (length(matches) != 1L) {
    stop("LabContact ", uid, " was created, but its UID could not be ",
         "verified on read-back. Do not repost.", call. = FALSE)
  }

  saved <- matches[[1L]]
  if (!identical(saved[["Firstname"]], firstname) ||
      !identical(saved[["Surname"]], surname)) {
    stop("LabContact ", uid, " was created, but Firstname and Surname ",
         "could not both be verified on read-back. Inspect the saved ",
         "record before taking further action. Do not repost.", call. = FALSE)
  }

  created
}
