#' Create a SENAITE laboratory contact
#'
#' Creates one LabContact and reads it back by UID to verify every supplied
#' contact field. Optional fields set to NULL are omitted from the POST.
#' Names are not unique; calling this function again can create a duplicate.
#'
#' @param firstname Contact's first name.
#' @param surname Contact's surname.
#' @param profile SENAITE connection profile.
#' @param parent_path Site path to the laboratory contacts folder.
#' @param salutation Optional greeting title, as entered in the web UI.
#' @param job_title Optional job title.
#' @param email_address Optional email address.
#' @param business_phone,business_fax,home_phone,mobile_phone Optional
#'   telephone or fax numbers. Supply as character strings.
#' @param middle_initial Optional middle initial, as entered in the web UI.
#' @param middle_name Optional middle name, as entered in the web UI.
#' @param physical_address,postal_address Optional named lists of address
#'   components. Allowed names are `address`, `city`, `district`, `state`,
#'   `zip`, and `country`. Supply values as non-empty strings; omit unused
#'   components. Use a string for `zip` to preserve leading zeros.
#' @returns The original create response. The new contact's UID is in
#'   `result$items[[1L]]$uid`.
#' @export
create_labcontact <- function(firstname, surname, profile = "default",
                              parent_path = "/senaite/bika_setup/bika_labcontacts",
                              salutation = NULL, job_title = NULL,
                              email_address = NULL, business_phone = NULL,
                              business_fax = NULL, home_phone = NULL,
                              mobile_phone = NULL, middle_initial = NULL,
                              middle_name = NULL, physical_address = NULL,
                              postal_address = NULL) {
  check_text <- function(x, name) {
    if (!is.character(x) || length(x) != 1L || is.na(x) ||
        !nzchar(trimws(x))) {
      stop("`", name, "` must be one non-empty string.", call. = FALSE)
    }
    trimws(x)
  }

  parent_path <- check_text(parent_path, "parent_path")
  fields <- list(
    Firstname = check_text(firstname, "firstname"),
    Surname   = check_text(surname, "surname")
  )

  optional <- list(
    Salutation    = salutation,
    JobTitle      = job_title,
    EmailAddress  = email_address,
    BusinessPhone = business_phone,
    BusinessFax   = business_fax,
    HomePhone     = home_phone,
    MobilePhone   = mobile_phone,
    Middleinitial = middle_initial,
    Middlename = middle_name
  )

  for (key in names(optional)) {
    if (!is.null(optional[[key]])) {
      fields[[key]] <- check_text(optional[[key]], key)
    }
  }

  check_address <- function(x, name) {
    allowed <- c("address", "city", "district", "state", "zip", "country")
    keys <- names(x)

    if (!is.list(x) || is.data.frame(x) || !length(x) ||
        is.null(keys) || anyNA(keys) || any(!nzchar(keys)) ||
        anyDuplicated(keys) || !all(keys %in% allowed)) {
      stop("`", name, "` must be a non-empty named list with unique keys ",
           "chosen from: ", paste(allowed, collapse = ", "), ".",
           call. = FALSE)
    }

    for (key in keys) {
      x[[key]] <- check_text(x[[key]], paste0(name, "$", key))
    }
    x
  }

  addresses <- list()
  if (!is.null(physical_address)) {
    addresses$PhysicalAddress <- check_address(physical_address, "physical_address")
  }
  if (!is.null(postal_address)) {
    addresses$PostalAddress <- check_address(postal_address, "postal_address")
  }

  form <- senaite_create_body(
    portal_type = "LabContact",
    parent_path = parent_path,
    fields = c(fields, addresses)
  )
  created <- post_senaite_data(
    body = form,
    profile = profile,
    encoding = if (length(addresses)) "json" else "form"
  )
  uid <- if (length(created$items) == 1L) created$items[[1L]]$uid
  if (!is.character(uid) || length(uid) != 1L || is.na(uid) || !nzchar(uid)) {
    stop("POST returned without one lab contact UID. ",
         "Check SENAITE before retrying.", call. = FALSE)
  }

  saved <- tryCatch(
    get_senaite_record(uid, profile = profile),
    error = function(e) {
      stop("LabContact ", uid, " was created, but read-back failed: ",
           conditionMessage(e), ". Do not repost.", call. = FALSE)
    }
  )

  verified <- vapply(names(fields), function(key) {
    identical(saved[[key]], fields[[key]])
  }, logical(1))

  if (any(!verified)) {
    stop("LabContact ", uid, " was created, but these fields did not ",
         "read back exactly as submitted: ",
         paste(names(fields)[!verified], collapse = ", "),
         ". Inspect the saved record; do not repost.", call. = FALSE)
  }

  mismatches <- character()
  for (key in names(addresses)) {
    expected <- addresses[[key]]
    actual <- saved[[key]]

    for (part in names(expected)) {
      if (!is.list(actual) || !identical(actual[[part]], expected[[part]])) {
        mismatches <- c(mismatches, paste0(key, "$", part))
      }
    }
  }

  if (length(mismatches)) {
    stop("LabContact ", uid, " was created, but these address components ",
         "did not read back exactly as submitted: ",
         paste(mismatches, collapse = ", "),
         ". Inspect the saved record. Do not repost.", call. = FALSE)
  }
  created
}
