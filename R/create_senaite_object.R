#' Create a SENAITE object
#'
#' Builds a payload with [senaite_create_body()] and submits it with
#' [post_senaite_data()]. To inspect a payload without creating an object,
#' use [senaite_create_body()] on its own.
#'
#' @inheritParams senaite_create_body
#' @inheritParams post_senaite_data
#' @returns The response from [post_senaite_data()], returned unchanged.
#' @examples
#' \dontrun{
#' created <- create_senaite_object(
#'   portal_type = "SamplePoint",
#'   parent_path = "/senaite/setup/samplepoints",
#'   fields = list(title = "Training sample point"),
#'   profile = "default"
#' )
#' }
#' @export
create_senaite_object <- function(portal_type, parent_path, fields = list(),
                                  profile = "default") {
  body <- senaite_create_body(
    portal_type = portal_type,
    parent_path = parent_path,
    fields = fields
  )

  post_senaite_data(body = body, profile = profile)
}
