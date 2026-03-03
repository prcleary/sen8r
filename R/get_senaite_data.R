#' Retrieve Data from the SENAITE API
#'
#' Downloads data from a specified SENAITE REST API endpoint with optional
#' query parameters and automatic pagination handling.
#'
#' The function performs authenticated `GET` requests and automatically
#' retrieves all pages of results when the API response indicates multiple
#' pages are available.
#'
#' @param endpoint `character(1)`. The SENAITE API endpoint to query.
#'   Must be one of the supported endpoint names (see Details).
#'
#' @param params `list`. Named list of query parameters to include in the
#'   API request. Default is an empty list.
#'
#' @param limit `numeric(1)`. Maximum number of records to retrieve per page.
#'   Defaults to `100`. This value is passed to the API as a query parameter.
#'
#' @param verbose `logical(1)`. If `TRUE` (default), progress messages are
#'   printed using \code{cli::cli_alert_info()}.
#'
#' @param username `character(1)`. SENAITE username for HTTP authentication.
#'   Defaults to the value stored in the system keyring under
#'   `"senaite_username"`.
#'
#' @param password `character(1)`. SENAITE password for HTTP authentication.
#'   Defaults to the value stored in the system keyring under
#'   `"senaite_password"`.
#'
#' @param base_url `character(1)`. Base URL of the SENAITE instance.
#'   Defaults to the value stored in the system keyring under `"BASE_URL"`.
#'
#' @returns
#' If successful, returns a `list` of items retrieved from the API.
#' Each element corresponds to a SENAITE object.
#'
#' If the endpoint does not return paginated content or is not found,
#' returns `NULL`.
#'
#' @details
#' ## Pagination
#'
#' If the API response contains a `pages` field greater than 1,
#' the function automatically retrieves additional pages using the
#' `b_start` parameter and concatenates all returned `items`.
#'
#' ## Authentication
#'
#' Requests are authenticated using HTTP Basic Authentication via
#' \code{httr::authenticate()}.
#'
#' ## Supported Endpoints
#'
#' The `endpoint` argument must match one of the supported SENAITE API
#' endpoint names (e.g., `"analysis"`, `"analysisrequest"`,
#' `"client"`, `"sampletype"`, `"worksheet"`, etc.).
#'
#' Use `match.arg()` to ensure only valid endpoints are accepted.
#'
#' @examples
#' \dontrun{
#' # Retrieve clients
#' clients <- get_senaite_data("client", limit = 50)
#'
#' # Retrieve verified analysis requests
#' ars <- get_senaite_data(
#'   endpoint = "analysisrequest",
#'   params = list(review_state = "verified"),
#'   limit = 100
#' )
#' }
#'
#' @seealso
#' \code{\link{construct_senaite_url}},
#' \code{\link[httr]{GET}},
#' \code{\link[httr]{authenticate}},
#' \code{\link[keyring]{key_get}}
#'
#' @export
get_senaite_data <- function(endpoint = c(
  'analysis',
  'analysiscategories',
  'analysiscategory',
  'analysisprofile',
  'analysisprofiles',
  'analysisrequest',
  'analysisrequestsfolder',
  'analysisservice',
  'analysisservices',
  'analysisspec',
  'analysisspecs',
  'antibiotic',
  'antibioticclass',
  'arreport',
  'artemplate',
  'artemplates',
  'astpanel',
  'astpanelfolder',
  'attachment',
  'attachmenttype',
  'attachmenttypes',
  'auditlog',
  'autoimportlog',
  'batch',
  'batchfolder',
  'batchlabel',
  'batchlabels',
  'bikasetup',
  'breakpointstable',
  'breakpointstables',
  'calculation',
  'calculations',
  'catalogs',
  'client',
  'clientfolder',
  'contact',
  'samplecontainer',
  'samplecontainers',
  'containertype',
  'containertypes',
  'databox',
  'databoxfolder',
  'department',
  'departments',
  'duplicateanalysis',
  'dynamicanalysisspec',
  'dynamicanalysisspecs',
  'instrument',
  'instrumentcalibration',
  'instrumentcertification',
  'instrumentlocation',
  'instrumentlocations',
  'instrumentmaintenancetask',
  'instruments',
  'instrumentscheduledtask',
  'instrumenttype',
  'instrumenttypes',
  'instrumentvalidation',
  'interpretationtemplate',
  'invoice',
  'labcontact',
  'labcontacts',
  'laboratory',
  'labproduct',
  'labproducts',
  'manufacturer',
  'manufacturers',
  'method',
  'methods',
  'microorganism',
  'microorganismcategory',
  'microorganismcategories',
  'microorganismfolder',
  'multifile',
  'patient',
  'patientfolder',
  'plone_site',
  'preservation',
  'preservations',
  'pricelist',
  'pricelistfolder',
  'referenceanalysis',
  'referencedefinition',
  'referencedefinitions',
  'referencesample',
  'referencesamplesfolder',
  'reflexrule',
  'reflexrulefolder',
  'rejectanalysis',
  'report',
  'reportfolder',
  'samplecondition',
  'sampleconditions',
  'samplematrices',
  'samplematrix',
  'samplepoint',
  'samplepoints',
  'sampletype',
  'sampletypes',
  'samplingdeviation',
  'samplingdeviations',
  'search',
  'storagelocation',
  'storagelocations',
  'subgroup',
  'subgroups',
  'supplier',
  'suppliercontact',
  'suppliers',
  'supplyorder',
  'supplyorderfolder',
  'version',
  'worksheet',
  'worksheetfolder',
  'worksheettemplate',
  'worksheettemplates'
),
params = list(),
limit = 100,
verbose = TRUE,
username = keyring::key_get('senaite_username'),
password = keyring::key_get('senaite_password'),
base_url = keyring::key_get('BASE_URL')) {
  if (!is.list(params)) {
    stop('`params` should be a list', call. = FALSE)
  }

  endpoint <- match.arg(endpoint)

  params <- c(params, limit = limit)

  url <- construct_senaite_url(endpoint, params, base_url = base_url)

  if (verbose) {
    cli::cli_alert_info('Downloading: {url}')
  }

  response <- httr::GET(url, httr::authenticate(username, password))

  httr::stop_for_status(response)

  output <- httr::content(response)

  if (is.null(output$pages)) {
    if (verbose) {
      cli::cli_alert_warning('Not found')
    }

    return(NULL)
  }

  n_pages <- output$pages

  if (n_pages <= 1) {
    return(output$items)
  }

  b_start <- seq(limit, n_pages * limit, limit)

  next_urls <- paste0(url, '&b_start=', b_start)

  next_outputs <- lapply(next_urls, function(x) {
    if (verbose) {
      cli::cli_alert_info('Downloading: {x}')
    }
    resp <- httr::GET(x, httr::authenticate(username, password))
    httr::stop_for_status(resp)
    httr::content(resp)
  })

  items <- unlist(c(list(output$items), lapply(next_outputs, function(x)
    x$items)), recursive = FALSE)

  items
}
