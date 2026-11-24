#' Create a SENAITE department
#'
#' Creates a department using the URL-encoded form tested with SENAITE 2.6.0.
#' The manager must already exist as a LabContact. After creation, fetches
#' the saved department and verifies its code and manager UID.
#'
#' @param title Department title, for example `"Haematology"`.
#' @param department_id Department code, for example `"Haem"`.
#' @param manager_uid UID of the existing LabContact who manages the department.
#' @param profile SENAITE connection profile.
#' @param parent_path Site path to the departments folder.
#' @returns The original create response, after successful verification.
#' @export
create_department <- function(title, department_id, manager_uid,
                              profile = "default",
                              parent_path = "/senaite/setup/departments") {
  inputs <- list(title = title, department_id = department_id,
                 manager_uid = manager_uid)
  for (key in names(inputs)) {
    x <- inputs[[key]]
    if (!is.character(x) || length(x) != 1L || is.na(x) ||
        !nzchar(trimws(x))) {
      stop("`", key, "` must be one non-empty string.", call. = FALSE)
    }
  }

  title <- trimws(title)
  department_id <- trimws(department_id)
  manager_uid <- trimws(manager_uid)

  read_departments <- function() {
    get_senaite_data(
      "department", params = list(complete = "yes"), profile = profile
    )
  }

  # Catch an accidental rerun before issuing a write.
  same_text <- function(x, y) {
    is.character(x) && length(x) == 1L && !is.na(x) &&
      tolower(trimws(x)) == tolower(y)
  }
  existing <- read_departments()
  duplicate <- vapply(existing, function(x) {
    identical(x$parent_path, parent_path) &&
      (same_text(x$title, title) || same_text(x$department_id, department_id))
  }, logical(1))
  if (any(duplicate)) {
    stop("A department with that title or code already exists in ",
         parent_path, ". Nothing was posted.", call. = FALSE)
  }

  # Write names follow the 2.6.0 form example; read names differ below.
  form <- senaite_create_body(
    portal_type = "Department",
    parent_path = parent_path,
    fields = list(title = title, DepartmentID = department_id,
                  Manager = manager_uid)
  )
  created <- post_senaite_data(body = form, profile = profile, encoding = "form")

  if (length(created$items) != 1L || is.null(created$items[[1L]]$uid)) {
    stop("POST succeeded but did not return one department UID. ",
         "Check SENAITE before retrying.", call. = FALSE)
  }
  uid <- created$items[[1L]]$uid

  saved <- tryCatch(
    Filter(function(x) identical(x$uid, uid), read_departments()),
    error = function(e) {
      stop("POST succeeded for department ", uid, ", but read-back failed: ",
           conditionMessage(e), ". Do not repost.", call. = FALSE)
    }
  )
  if (length(saved) != 1L ||
      !identical(saved[[1L]]$department_id, department_id) ||
      !is.list(saved[[1L]]$manager) ||
      !identical(saved[[1L]]$manager$uid, manager_uid)) {
    stop("Department ", uid, " was created, but its saved code or manager ",
         "could not be verified. Do not repost.", call. = FALSE)
  }

  created
}
